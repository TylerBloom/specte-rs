use std::env::home_dir;

use futures::StreamExt;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;
use tokio_stream::wrappers::UnboundedReceiverStream;
use troupe::ActorBuilder;
use troupe::ActorState;
use troupe::Permanent;
use troupe::Scheduler;
use troupe::async_trait;
use troupe::sink::SinkActor;
use troupe::sink::SinkClient;
use xilem::AnyWidgetView;
use xilem::WidgetView;
use xilem::core::fork;
use xilem::view::flex_col;
use xilem::view::image;
use xilem::view::label;
use xilem::view::text_button;
use xilem::view::worker;

use crate::config::Config;
use crate::emu_core::EmuRecv;
use crate::emu_core::EmuSend;
use crate::emu_core::Frame;
use crate::emu_core::Image;
use crate::keys::Keystroke;
use crate::trove::Trove;
use crate::utils::identity_proxy;

pub struct UiState {
    send: EmuSend,
    /// Used to communicate with the emulator proxy
    emu_proxy_client: SinkClient<Permanent, EmulatorProxyMessage>,
    key_proxy_client: SinkClient<Permanent, KeyboardProxyMessage>,
    pub(crate) add_game_client: SinkClient<Permanent, AddGameMessage>,
    cursor: StateCursor,
    pub(crate) home: HomeState,
    game: InGameState,
    settings: SettingsState,
}

#[derive(Debug, Clone, Copy)]
pub enum StateCursor {
    Home,
    InGame,
    Settings,
}

pub struct HomeState {
    pub(crate) trove: Trove,
}

pub struct InGameState {
    image: Image,
    frames: usize,
}

pub struct SettingsState {}

#[derive(Debug)]
pub enum UiMessage {
    HomeMessage(HomeMessage),
    InGameMessage(InGameMessage),
    SettingsMessage(SettingsMessage),
    SwitchToSettings,
    Keystroke(Keystroke),
    Escape,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HomeMessage {
    AddGame(String, Vec<u8>),
    StartGame(String),
}

#[derive(Debug)]
pub enum InGameMessage {
    NextFrame((Image, usize)),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SettingsMessage {}

impl UiState {
    pub fn new(
        config: Config,
        send: EmuSend,
        emu_recv: EmuRecv,
        key_recv: UnboundedReceiver<UiMessage>,
    ) -> Self {
        let trove = config.get_trove();
        let image = Image::empty();
        let mut builder = ActorBuilder::new(EmulatorProxy::default());
        builder.attach_stream(emu_recv.into_stream().fuse());
        let emu_proxy_client = builder.launch();

        let mut builder = ActorBuilder::new(KeyboardProxy::default());
        builder.attach_stream(UnboundedReceiverStream::new(key_recv).fuse());
        let key_proxy_client = builder.launch();

        let add_game_client= ActorBuilder::new(AddGameWorker::default()).launch();

        Self {
            send,
            emu_proxy_client,
            key_proxy_client,
            add_game_client,
            cursor: StateCursor::Home,
            home: HomeState { trove },
            game: InGameState { image, frames: 0 },
            settings: SettingsState {},
        }
    }

    pub fn app_logic(&mut self) -> impl WidgetView<UiState> + use<> {
        println!("Running app logic...");
        self.view()
    }

    pub fn update(&mut self, msg: UiMessage) {
        let cursor = match msg {
            UiMessage::HomeMessage(msg) if matches!(self.cursor, StateCursor::Home) => {
                self.home.update(&self.send, msg)
            }
            UiMessage::HomeMessage(_) => unreachable!(),
            UiMessage::InGameMessage(msg) if matches!(self.cursor, StateCursor::InGame) => {
                self.game.update(msg)
            }
            UiMessage::InGameMessage(_) => None,
            UiMessage::SettingsMessage(msg) if matches!(self.cursor, StateCursor::Settings) => {
                self.settings.update(msg)
            }
            UiMessage::SettingsMessage(_) => unreachable!(),
            UiMessage::SwitchToSettings => Some(StateCursor::Settings),
            UiMessage::Escape => {
                self.send.pause();
                Some(StateCursor::Home)
            }
            UiMessage::Keystroke(key) => {
                self.send.keystroke(key);
                None
            }
        };
        if let Some(cursor) = cursor {
            self.cursor = cursor;
        }
    }

    pub fn view(&self) -> Box<AnyWidgetView<UiState>> {
        let main_widget = match self.cursor {
            StateCursor::Home => self.home.view().boxed(),
            StateCursor::InGame => self.game.view().boxed(),
            StateCursor::Settings => self.settings.view().boxed(),
        };
        let emu_worker = worker(
            identity_proxy,
            Self::update_emu_proxy_sender,
            Self::process_next_frame,
        );
        let key_worker = worker(identity_proxy, Self::update_key_proxy_sender, Self::update);
        fork(fork(main_widget, emu_worker), key_worker).boxed()
    }

    fn update_emu_proxy_sender(&mut self, send: UnboundedSender<Frame>) {
        self.emu_proxy_client.send(send);
    }

    fn update_key_proxy_sender(&mut self, send: UnboundedSender<UiMessage>) {
        self.key_proxy_client.send(send);
    }

    fn process_next_frame(&mut self, (image, count): Frame) {
        if matches!(self.cursor, StateCursor::InGame) {
            self.game.image = image;
            self.game.frames = count;
        }
    }
}

/// Xilem doesn't have a notice of "subscriptions". Async events that occur largely outside of the
/// state of your UI aren't directly support. It does, however, have a "worker" widget. This widget
/// handles the nitty-gritty details of proxying the event loop with a message for the correct widget
/// and ensuring that widget actually exists.
///
/// This proxy acts as an intermediary between the emulator task and the UI state's "worker"
/// widget's task. It consists of two input channels and one output channel. The first input is from
/// the emulator for streaming frames. The output channel sends the frame data to the worker
/// widgets's task. Since the worker widget can appear and come back (e.g. you're in game, go to the
/// home menu, and go back in game), the output channel might need to be swapped for a new channel.
/// That new channel is received from the second input channel.
#[derive(Default)]
pub struct EmulatorProxy {
    sender: Option<UnboundedSender<Frame>>,
}

#[derive(derive_more::From)]
pub enum EmulatorProxyMessage {
    NewSender(UnboundedSender<Frame>),
    Frame(Frame),
}

#[async_trait]
impl ActorState for EmulatorProxy {
    type ActorType = SinkActor;
    type Permanence = Permanent;
    type Message = EmulatorProxyMessage;
    type Output = ();

    async fn process(&mut self, _scheduler: &mut Scheduler<Self>, msg: Self::Message) {
        match msg {
            EmulatorProxyMessage::NewSender(send) => self.sender = Some(send),
            EmulatorProxyMessage::Frame(frame) => {
                if let Some(send) = self.sender.as_ref() {
                    send.send(frame).unwrap();
                }
            }
        }
    }
}

/// Fuctions very similarly to the `EmulatorProxy` but for keyboard events not meant for the
/// emulator core.
#[derive(Default)]
pub struct KeyboardProxy {
    sender: Option<UnboundedSender<UiMessage>>,
}

#[derive(derive_more::From)]
pub enum KeyboardProxyMessage {
    NewSender(UnboundedSender<UiMessage>),
    Message(UiMessage),
}

#[async_trait]
impl ActorState for KeyboardProxy {
    type ActorType = SinkActor;
    type Permanence = Permanent;
    type Message = KeyboardProxyMessage;
    type Output = ();

    async fn process(&mut self, _scheduler: &mut Scheduler<Self>, msg: Self::Message) {
        match msg {
            KeyboardProxyMessage::NewSender(send) => self.sender = Some(send),
            KeyboardProxyMessage::Message(msg) => {
                if let Some(send) = self.sender.as_ref() {
                    send.send(msg).unwrap();
                }
            }
        }
    }
}

#[derive(Default)]
pub struct AddGameWorker {
    sender: Option<UnboundedSender<(String, Vec<u8>)>>,
}

#[derive(Debug, derive_more::From)]
pub enum AddGameMessage {
    AddGame,
    NewSender(UnboundedSender<(String, Vec<u8>)>),
}

#[async_trait]
impl ActorState for AddGameWorker {
    type ActorType = SinkActor;
    type Permanence = Permanent;
    type Message = AddGameMessage;
    type Output = ();

    async fn process(&mut self, _scheduler: &mut Scheduler<Self>, msg: Self::Message) {
        match msg {
            AddGameMessage::NewSender(send) => self.sender = Some(send),
            AddGameMessage::AddGame => {
                if let Some(send) = self.sender.as_ref() {
                    let dialog = rfd::AsyncFileDialog::new();
                    #[cfg(not(target_family = "wasm"))]
                    let dialog = dialog.set_directory(home_dir().unwrap());

                    let Some(handle) = dialog.pick_file().await else {
                        return;
                    };

                    send.send((handle.file_name(), handle.read().await))
                        .unwrap();
                }
            }
        }
    }
}

impl HomeState {
    fn update(&mut self, send: &EmuSend, msg: HomeMessage) -> Option<StateCursor> {
        match msg {
            HomeMessage::AddGame(name, rom) => {
                self.trove.add_game(name, rom);
                None
            }
            HomeMessage::StartGame(file) => {
                let game = self.trove.fetch_game(&file);
                send.start_game(game);
                Some(StateCursor::InGame)
            }
        }
    }

    pub fn view(&self) -> impl WidgetView<UiState> + use<> {
        flex_col((self.settings_button(), self.trove.display()))
    }

    fn settings_button(&self) -> impl WidgetView<UiState> + use<> {
        text_button("Settings", |_: &mut UiState| {})
    }
}

impl InGameState {
    fn update(&mut self, msg: InGameMessage) -> Option<StateCursor> {
        match msg {
            InGameMessage::NextFrame((image, frames)) => {
                self.image = image;
                self.frames = frames
            }
        }
        None
    }

    pub fn view(&self) -> impl WidgetView<UiState> + use<> {
        flex_col((
            label(format!("Frame #{}", self.frames)),
            image(self.image.0.clone()),
        ))
    }
}

impl SettingsState {
    fn update(&mut self, _msg: SettingsMessage) -> Option<StateCursor> {
        todo!()
    }

    pub fn view(&self) -> impl WidgetView<UiState> + use<> {
        label("UNDER CONSTRUCTION!!!")
    }
}
