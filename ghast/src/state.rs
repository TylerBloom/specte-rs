use std::sync::Arc;

use futures::StreamExt;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio_stream::wrappers::UnboundedReceiverStream;
use troupe::ActorBuilder;
use troupe::ActorState;
use troupe::Scheduler;
use troupe::sink::SinkActor;

use crate::config::Config;
use crate::emu_core::EmuMessage;
use crate::emu_core::EmuOutput;
use crate::emu_core::Image;
use crate::emu_core::SinkClient;
use crate::emu_core::SinkSendClient;
use crate::keys::ControlSignal;
use crate::keys::Keystroke;

pub struct UiState {
    pub(crate) emu_client: SinkClient<EmuMessage>,
    /// Used to communicate with the emulator proxy
    pub(crate) proxy_client: troupe::sink::SinkClient<UiProxyMessage>,
    pub(crate) cursor: StateCursor,
    pub(crate) home: HomeState,
    pub(crate) game: InGameState,
    pub(crate) settings: SettingsState,
    #[allow(dead_code)]
    pub(crate) config: Config,
}

#[derive(Debug, Clone, Copy)]
pub enum StateCursor {
    Home,
    InGame,
    Settings,
}

pub struct HomeState {
    games: Vec<Arc<str>>,
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
    Keystroke(Keystroke),
    InitProxy(MessageProxy<UiMessage>),
    SwitchToSettings,
    Escape,
}

impl From<EmuOutput> for UiMessage {
    fn from(msg: EmuOutput) -> Self {
        match msg {
            EmuOutput::Frame(frame) => UiMessage::InGameMessage(frame.into()),
            EmuOutput::TroveGames(games) => UiMessage::HomeMessage(HomeMessage::TroveGames(games)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HomeMessage {
    AddGame,
    StartGame(String),
    TroveGames(Vec<Arc<str>>),
}

#[derive(Debug, derive_more::From)]
pub enum InGameMessage {
    NextFrame((Image, usize)),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SettingsMessage {}

impl UiState {
    pub fn new(
        config: Config,
        emu_client: SinkSendClient<EmuMessage, EmuOutput>,
        key_recv: UnboundedReceiver<UiMessage>,
    ) -> Self {
        let image = Image::blank();

        let proxy = UiProxy::Uninit(vec![]);

        let (emu_client, stream) = emu_client.split();

        let proxy_client = ActorBuilder::new(proxy)
            .attach_stream(UnboundedReceiverStream::new(key_recv).fuse())
            .attach_stream(stream.fuse())
            .spawn();

        emu_client.send(EmuMessage::FetchGameList);

        Self {
            config,
            emu_client,
            proxy_client,
            cursor: StateCursor::Home,
            home: HomeState { games: vec![] },
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
            UiMessage::InitProxy(sender) => {
                self.update_proxy_sender(sender);
                None
            }
        };
        if let Some(cursor) = cursor {
            self.cursor = cursor;
        }
    }

    fn update_proxy_sender(&mut self, sender: MessageProxy<UiMessage>) {
        self.proxy_client.send(sender);
    }
}

impl HomeState {
    fn update(&mut self, send: &SinkClient<EmuMessage>, msg: HomeMessage) -> Option<StateCursor> {
        match msg {
            HomeMessage::AddGame => {
                send.send(EmuMessage::AddGame);
                send.send(EmuMessage::FetchGameList);
                None
            }
            HomeMessage::StartGame(file) => {
                // send.send(EmuMessage::LoadGame(file));
                // Some(StateCursor::InGame)
                todo!()
            }
            HomeMessage::TroveGames(games) => {
                self.games = games;
                None
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

/// State that runs parallel the main UI state. Responsible for sending messages from various
/// other bits of state (actors) to the main UI state.
///
/// Xilem does not imposes two major design constraints. First, it does not have a straightforward
/// mechanism for delivering messages from async sources. Rather, it provides widgets that provide
/// an unbounded sender. Sending a message on that sender will trigger a message for the UI state
/// to response to. Rather than sending this channel to all of the bits of state, this proxies
/// messages to UI.
enum UiProxy {
    Uninit(Vec<UiMessage>),
    Working(MessageProxy<UiMessage>),
}

pub(crate) enum UiProxyMessage {
    NewProxy(MessageProxy<UiMessage>),
    ProxyMessage(UiMessage),
}

impl ActorState for UiProxy {
    type Message = UiProxyMessage;
    type ActorKind = SinkActor;

    async fn process(&mut self, _scheduler: &mut Scheduler<Self>, msg: Self::Message) {
        match msg {
            UiProxyMessage::NewProxy(proxy) => match self {
                UiProxy::Uninit(messages) => {
                    for msg in std::mem::take(messages) {
                        proxy.send_message(msg);
                    }
                    *self = Self::Working(proxy);
                }
                UiProxy::Working(_) => {
                    *self = Self::Working(proxy);
                }
            },
            UiProxyMessage::ProxyMessage(msg) => match self {
                UiProxy::Uninit(messages) => messages.push(msg),
                UiProxy::Working(sender) => {
                    sender.send_message(msg);
                }
            },
        }
    }
}

impl From<MessageProxy<UiMessage>> for UiProxyMessage {
    fn from(value: MessageProxy<UiMessage>) -> Self {
        UiProxyMessage::NewProxy(value)
    }
}

impl<T: Into<UiMessage>> From<T> for UiProxyMessage {
    fn from(value: T) -> Self {
        UiProxyMessage::ProxyMessage(value.into())
    }
}

#[cfg(not(target_family = "wasm"))]
pub use native::MessageProxy;

#[cfg(target_family = "wasm")]
pub use wasm::MessageProxy;

#[cfg(not(target_family = "wasm"))]
mod native {
    use std::fmt::Debug;

    use masonry::peniko::ImageAlphaType;
    use masonry::peniko::ImageData;
    use xilem::AnyWidgetView;
    use xilem::Blob;
    use xilem::ImageFormat;
    use xilem::WidgetView;
    use xilem::core::fork;
    use xilem::view::flex_col;
    use xilem::view::label;
    use xilem::view::task;
    use xilem::view::text_button;

    use super::*;

    #[derive(Debug)]
    pub struct MessageProxy<M: 'static + Send + Debug>(xilem_core::MessageProxy<M>);

    impl<M: 'static + Send + Debug> MessageProxy<M> {
        pub fn send_message(&self, msg: M) {
            let _ = self.0.message(msg);
        }
    }

    impl UiState {
        pub fn app_logic(&mut self) -> impl WidgetView<UiState> + use<> {
            self.view()
        }

        pub fn view(&self) -> Box<AnyWidgetView<UiState>> {
            let main_widget = match self.cursor {
                StateCursor::Home => self.home.view().boxed(),
                StateCursor::InGame => self.game.view().boxed(),
                StateCursor::Settings => self.settings.view().boxed(),
            };
            let proxy_task = task(ui_proxy_init, Self::update);
            fork(main_widget, proxy_task).boxed()
        }
    }

    async fn ui_proxy_init(proxy: xilem_core::MessageProxy<UiMessage>) {
        let msg_proxy = MessageProxy(proxy.clone());
        proxy.message(UiMessage::InitProxy(msg_proxy)).unwrap();
    }

    impl HomeState {
        pub fn view(&self) -> impl WidgetView<UiState> + use<> {
            flex_col((
                self.settings_button(),
                label("Trove"),
                self.add_game_set_button(),
                self.display_games(),
            ))
        }

        fn settings_button(&self) -> impl WidgetView<UiState> + use<> {
            text_button("Settings", |_: &mut UiState| {})
        }

        pub fn add_game_set_button(&self) -> impl WidgetView<UiState> + use<> {
            text_button("Add Game Set", |state: &mut UiState| {
                state.emu_client.send(EmuMessage::AddGame);
            })
        }

        pub fn display_games(&self) -> impl WidgetView<UiState> + use<> {
            let col = self
                .games
                .iter()
                .map(|name| {
                    let name = name.clone();
                    text_button(name.clone(), move |state: &mut UiState| {
                        state.update(UiMessage::HomeMessage(HomeMessage::StartGame(
                            (&*name).to_owned(),
                        )));
                    })
                })
                .collect::<Vec<_>>();

            flex_col(col)
        }
    }

    impl InGameState {
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

    impl SettingsState {
        pub fn view(&self) -> impl WidgetView<UiState> + use<> {
            label("UNDER CONSTRUCTION!!!")
        }
    }
}

#[cfg(target_family = "wasm")]
mod wasm {
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

    impl UiState {
        pub fn app_logic(&mut self) -> impl DomView<UiState> + use<> {
            self.view()
        }

        pub fn view(&self) -> impl DomView<UiState> + use<> {
            let main_widget = match self.cursor {
                StateCursor::Home => self.home.view().boxed(),
                StateCursor::InGame => self.game.view().boxed(),
                StateCursor::Settings => self.settings.view().boxed(),
            };
            let proxy_task = task(ui_proxy_init, Self::update);
            fork(main_widget, proxy_task).boxed()
        }
    }

    async fn ui_proxy_init(proxy: TaskProxy, _shutdown: ShutdownSignal) {
        let (send, mut recv) = unbounded_channel();
        proxy.send_message(UiMessage::InitProxy(send));
        loop {
            let msg = recv.recv().await.unwrap();
            proxy.send_message(msg);
        }
    }

    impl HomeState {
        pub fn view(&self) -> impl DomView<UiState> + use<> {
            let games = self
                .games
                .iter()
                .map(|name| {
                    let name = String::from(&**name);
                    button(name.clone()).on_click(move |state: &mut UiState, _| {
                        state.update(UiMessage::HomeMessage(HomeMessage::StartGame(name.clone())));
                    })
                })
                .collect::<Vec<_>>();
            div((self.settings_button(), div(games)))
        }

        fn settings_button(&self) -> impl DomView<UiState> + use<> {
            button("Settings").on_click(|_: &mut UiState, _| {})
        }
    }

    impl InGameState {
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

    impl SettingsState {
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
}
