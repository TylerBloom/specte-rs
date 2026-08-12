use std::env::home_dir;

use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::mpsc::error::SendError;
use tokio::sync::mpsc::unbounded_channel;
use xilem::AnyWidgetView;
use xilem::WidgetView;
use xilem::core::fork;
use xilem::view::flex_col;
use xilem::view::image;
use xilem::view::label;
use xilem::view::text_button;
use xilem::view::worker;
use xilem_core::MessageProxy;

use crate::config::Config;
use crate::emu_core::EmuRecv;
use crate::emu_core::EmuSend;
use crate::emu_core::Frame;
use crate::emu_core::Image;
use crate::keys::Keystroke;
use crate::trove::Trove;

pub struct UiState {
    send: EmuSend,
    /// Used to communicate with the emulator proxy
    proxy_send: UnboundedSender<UnboundedSender<Frame>>,
    cursor: StateCursor,
    home: HomeState,
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
    trove: Trove,
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
    AddGame,
    StartGame(String),
}

#[derive(Debug)]
pub enum InGameMessage {
    NextFrame((Image, usize)),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SettingsMessage {}

impl UiState {
    pub fn new(config: Config, send: EmuSend, recv: EmuRecv) -> Self {
        let trove = config.get_trove();
        let image = Image::empty();
        let (proxy_send, proxy_recv) = unbounded_channel();
        let proxy = EmulatorProxy::new(recv, proxy_recv);
        tokio::spawn(proxy.run());
        Self {
            send,
            proxy_send,
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
        let worker = worker(
            Self::emu_proxy_worker,
            Self::update_proxy_sender,
            Self::process_next_frame,
        );
        fork(main_widget, worker).boxed()
    }

    async fn emu_proxy_worker(proxy: MessageProxy<Frame>, mut recv: UnboundedReceiver<Frame>) {
        loop {
            let msg = recv.recv().await.unwrap();
            proxy.message(msg).unwrap();
        }
    }

    fn update_proxy_sender(&mut self, send: UnboundedSender<Frame>) {
        self.proxy_send.send(send).unwrap();
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
pub struct EmulatorProxy {
    emu_recv: EmuRecv,
    worker_send_recv: UnboundedReceiver<UnboundedSender<Frame>>,
}

impl EmulatorProxy {
    fn new(emu_recv: EmuRecv, worker_send_recv: UnboundedReceiver<UnboundedSender<Frame>>) -> Self {
        Self {
            emu_recv,
            worker_send_recv,
        }
    }

    async fn run(mut self) {
        let Self {
            emu_recv,
            worker_send_recv: work_send_recv,
        } = &mut self;
        let mut send = work_send_recv.recv().await.unwrap();
        loop {
            let frame = emu_recv.next_frame().await;
            match send.send(frame) {
                Ok(()) => {}
                Err(SendError(frame)) => {
                    while let Some(new_send) = work_send_recv.recv().await {
                        send = new_send;
                    }
                    send.send(frame).unwrap();
                }
            }
        }
    }
}

impl HomeState {
    fn update(&mut self, send: &EmuSend, msg: HomeMessage) -> Option<StateCursor> {
        match msg {
            HomeMessage::AddGame => {
                let path = home_dir().unwrap();
                let game = rfd::FileDialog::new().set_directory(&path).pick_file();
                if let Some(file) = game {
                    self.trove.add_game(file);
                }
                None
            }
            HomeMessage::StartGame(file) => {
                let game = self.trove.fetch_game(file);
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
