#[cfg(not(target_family = "wasm"))]
use std::env::home_dir;

use futures::StreamExt;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;
use tokio_stream::wrappers::UnboundedReceiverStream;
use troupe::ActorBuilder;
use troupe::ActorState;
use troupe::Scheduler;
use troupe::joint::JointClient;
use troupe::sink::SinkActor;
use troupe::sink::SinkClient;

use crate::config::Config;
use crate::emu_core::EmuMessage;
use crate::emu_core::EmuOutput;
use crate::emu_core::Frame;
use crate::emu_core::Image;
use crate::keys::ControlSignal;
use crate::keys::Keystroke;
use crate::trove::Trove;

pub struct UiState {
    emu_client: JointClient<EmuMessage, EmuOutput>,
    /// Used to communicate with the emulator proxy
    emu_proxy_client: SinkClient<EmulatorProxyMessage>,
    key_proxy_client: SinkClient<KeyboardProxyMessage>,
    pub(crate) add_game_client: SinkClient<AddGameMessage>,
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
        emu_client: JointClient<EmuMessage, EmuOutput>,
        key_recv: UnboundedReceiver<UiMessage>,
    ) -> Self {
        let trove = config.get_trove();
        let image = Image::blank();
        let emu_proxy_client = ActorBuilder::new(EmulatorProxy::default())
            .attach_stream(emu_client.stream().map(Result::unwrap).fuse())
            .spawn();

        let key_proxy_client = ActorBuilder::new(KeyboardProxy::default())
            .attach_stream(UnboundedReceiverStream::new(key_recv).fuse())
            .spawn();

        let add_game_client = ActorBuilder::new(AddGameWorker::default()).spawn();

        Self {
            emu_client,
            emu_proxy_client,
            key_proxy_client,
            add_game_client,
            cursor: StateCursor::Home,
            home: HomeState { trove },
            game: InGameState { image, frames: 0 },
            settings: SettingsState {},
        }
    }

    pub fn update(&mut self, msg: UiMessage) {
        let cursor = match msg {
            UiMessage::HomeMessage(msg) if matches!(self.cursor, StateCursor::Home) => {
                self.home.update(&self.emu_client, msg)
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
                self.emu_client
                    .send(Keystroke::Control(ControlSignal::Pause));
                Some(StateCursor::Home)
            }
            UiMessage::Keystroke(key) => {
                self.emu_client.send(key);
                None
            }
        };
        if let Some(cursor) = cursor {
            self.cursor = cursor;
        }
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
    EmuMessage(EmuOutput),
}

impl ActorState for EmulatorProxy {
    type ActorKind = SinkActor;
    type Message = EmulatorProxyMessage;

    async fn process(&mut self, _scheduler: &mut Scheduler<Self>, msg: Self::Message) {
        match msg {
            EmulatorProxyMessage::NewSender(send) => self.sender = Some(send),
            EmulatorProxyMessage::EmuMessage(msg) => match msg {
                EmuOutput::Frame(frame) => {
                    if let Some(send) = self.sender.as_ref() {
                        send.send(frame).unwrap();
                    }
                }
                EmuOutput::Snapshot(_, _) => todo!(),
                EmuOutput::SaveState(_, _) => todo!(),
            },
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

impl ActorState for KeyboardProxy {
    type ActorKind = SinkActor;
    type Message = KeyboardProxyMessage;

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

impl ActorState for AddGameWorker {
    type ActorKind = SinkActor;
    type Message = AddGameMessage;

    async fn process(&mut self, _scheduler: &mut Scheduler<Self>, msg: Self::Message) {
        match msg {
            AddGameMessage::NewSender(send) => self.sender = Some(send),
            // The file dialog's future isn't `Send` on the WASM target (it holds JS handles
            // internally), but `#[async_trait]` requires `process`'s future to be `Send`. Since
            // `spawn_local` doesn't require its future to be `Send`, the dialog is driven to
            // completion off to the side instead of being awaited directly here.
            #[cfg(target_family = "wasm")]
            AddGameMessage::AddGame => {
                if let Some(send) = self.sender.clone() {
                    wasm_bindgen_futures::spawn_local(async move {
                        let Some(handle) = rfd::AsyncFileDialog::new().pick_file().await else {
                            return;
                        };
                        send.send((handle.file_name(), handle.read().await))
                            .unwrap();
                    });
                }
            }
            #[cfg(not(target_family = "wasm"))]
            AddGameMessage::AddGame => {
                if let Some(send) = self.sender.as_ref() {
                    let dialog = rfd::AsyncFileDialog::new().set_directory(home_dir().unwrap());
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
    fn update(
        &mut self,
        send: &JointClient<EmuMessage, EmuOutput>,
        msg: HomeMessage,
    ) -> Option<StateCursor> {
        match msg {
            HomeMessage::AddGame(name, rom) => {
                self.trove.add_game(name, rom);
                None
            }
            HomeMessage::StartGame(file) => {
                let game = self.trove.fetch_game(&file);
                send.send(game);
                Some(StateCursor::InGame)
            }
        }
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
}

impl SettingsState {
    fn update(&mut self, _msg: SettingsMessage) -> Option<StateCursor> {
        todo!()
    }
}

#[cfg(not(target_family = "wasm"))]
mod native {
    use masonry::peniko::ImageAlphaType;
    use masonry::peniko::ImageData;
    use xilem::AnyWidgetView;
    use xilem::Blob;
    use xilem::ImageFormat;
    use xilem::WidgetView;
    use xilem::core::fork;
    use xilem::view::flex_col;
    use xilem::view::label;
    use xilem::view::text_button;
    use xilem::view::worker;

    use super::*;
    use crate::utils::identity_proxy;

    impl super::UiState {
        pub fn app_logic(&mut self) -> impl WidgetView<UiState> + use<> {
            self.view()
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
    }

    impl super::HomeState {
        pub fn view(&self) -> impl WidgetView<UiState> + use<> {
            flex_col((self.settings_button(), self.trove.display()))
        }

        fn settings_button(&self) -> impl WidgetView<UiState> + use<> {
            text_button("Settings", |_: &mut UiState| {})
        }
    }

    impl super::InGameState {
        pub fn view(&self) -> impl WidgetView<UiState> + use<> {
            let image = ImageData {
                data: Blob::from(self.image.pixels.clone()),
                format: ImageFormat::Rgba8,
                alpha_type: ImageAlphaType::Alpha,
                width: self.image.width,
                height: self.image.height,
            };
            flex_col((
                label(format!("Frame #{}", self.frames)),
                xilem::view::image(image),
            ))
        }
    }

    impl super::SettingsState {
        pub fn view(&self) -> impl WidgetView<UiState> + use<> {
            label("UNDER CONSTRUCTION!!!")
        }
    }
}

#[cfg(target_family = "wasm")]
mod wasm {
    use tokio::sync::mpsc::UnboundedSender;
    use tokio::sync::mpsc::unbounded_channel;
    use web_sys::wasm_bindgen::Clamped;
    use web_sys::wasm_bindgen::JsCast;
    use xilem_web::DomView;
    use xilem_web::concurrent::ShutdownSignal;
    use xilem_web::concurrent::TaskProxy;
    use xilem_web::concurrent::task;
    use xilem_web::core::fork;
    use xilem_web::elements::html::button;
    use xilem_web::elements::html::canvas;
    use xilem_web::elements::html::div;
    use xilem_web::elements::html::p;
    use xilem_web::interfaces::Element as _;

    use super::*;

    impl super::UiState {
        pub fn app_logic(&mut self) -> impl DomView<UiState> + use<> {
            self.view()
        }

        pub fn view(&self) -> impl DomView<UiState> + use<> {
            let main_widget = match self.cursor {
                StateCursor::Home => self.home.view().boxed(),
                StateCursor::InGame => self.game.view().boxed(),
                StateCursor::Settings => self.settings.view().boxed(),
            };
            fork(
                fork(main_widget, task(emu_frame_task_init, emu_frame_task_event)),
                task(keyboard_task_init, keyboard_task_event),
            )
        }
    }

    impl super::HomeState {
        pub fn view(&self) -> impl DomView<UiState> + use<> {
            div((self.settings_button(), self.trove.display()))
        }

        fn settings_button(&self) -> impl DomView<UiState> + use<> {
            button("Settings").on_click(|_: &mut UiState, _| {})
        }
    }

    impl super::InGameState {
        pub fn view(&self) -> impl DomView<UiState> + use<> {
            let image = self.image.clone();
            div((
                p(format!("Frame #{}", self.frames)),
                canvas(())
                    .attr("width", image.width.to_string())
                    .attr("height", image.height.to_string())
                    .after_rebuild(move |el: &web_sys::HtmlCanvasElement| paint_frame(el, &image)),
            ))
        }
    }

    impl super::SettingsState {
        pub fn view(&self) -> impl DomView<UiState> + use<> {
            p("UNDER CONSTRUCTION!!!")
        }
    }

    /// Paints a decoded emulator frame onto the given canvas element via the 2D canvas API.
    fn paint_frame(canvas: &web_sys::HtmlCanvasElement, image: &Image) {
        let ctx = canvas
            .get_context("2d")
            .unwrap()
            .unwrap()
            .dyn_into::<web_sys::CanvasRenderingContext2d>()
            .unwrap();
        let data = web_sys::ImageData::new_with_u8_clamped_array_and_sh(
            Clamped(&image.pixels),
            image.width,
            image.height,
        )
        .unwrap();
        ctx.put_image_data(&data, 0.0, 0.0).unwrap();
    }

    #[derive(Debug)]
    enum EmuFrameTaskMessage {
        NewSender(UnboundedSender<Frame>),
        Frame(Frame),
    }

    #[derive(Debug)]
    enum KeyboardTaskMessage {
        NewSender(UnboundedSender<UiMessage>),
        Keyboard(UiMessage),
    }

    async fn emu_frame_task_init(proxy: TaskProxy, _shutdown: ShutdownSignal) {
        let (send, mut recv) = unbounded_channel();
        proxy.send_message(EmuFrameTaskMessage::NewSender(send));
        loop {
            let frame = recv.recv().await.unwrap();
            proxy.send_message(EmuFrameTaskMessage::Frame(frame));
        }
    }

    fn emu_frame_task_event(state: &mut UiState, msg: EmuFrameTaskMessage) {
        match msg {
            EmuFrameTaskMessage::NewSender(send) => state.update_emu_proxy_sender(send),
            EmuFrameTaskMessage::Frame(frame) => state.process_next_frame(frame),
        }
    }

    async fn keyboard_task_init(proxy: TaskProxy, _shutdown: ShutdownSignal) {
        let (send, mut recv) = unbounded_channel();
        proxy.send_message(KeyboardTaskMessage::NewSender(send));
        loop {
            let msg = recv.recv().await.unwrap();
            proxy.send_message(KeyboardTaskMessage::Keyboard(msg));
        }
    }

    fn keyboard_task_event(state: &mut UiState, msg: KeyboardTaskMessage) {
        match msg {
            KeyboardTaskMessage::NewSender(send) => state.update_key_proxy_sender(send),
            KeyboardTaskMessage::Keyboard(msg) => state.update(msg),
        }
    }
}
