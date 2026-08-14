use ghast::config::Config;
use ghast::emu_core::EmuHandle;
use ghast::emu_core::EmuSend;
use ghast::keys::KeyWatcher;
use ghast::state::UiMessage;
use ghast::state::UiState;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::mpsc::unbounded_channel;
use web_sys::wasm_bindgen::JsCast;
use web_sys::wasm_bindgen::closure::Closure;
use winit::application::ApplicationHandler;
use winit::event::WindowEvent;
use winit::event_loop::ActiveEventLoop;
use winit::event_loop::EventLoop;
use winit::platform::web::EventLoopExtWebSys;
use winit::platform::web::WindowAttributesExtWebSys;
use winit::window::Window;
use winit::window::WindowId;
use xilem_web::App;
use xilem_web::document_body;

/// A `winit` window backed by a canvas that exists only to receive keyboard focus. It isn't
/// rendered into or shown to the user; `xilem_web` owns the visible DOM.
struct KeyCaptureApp {
    window: Option<Window>,
    keys: KeyWatcher,
    send: EmuSend,
    key_proxy_send: UnboundedSender<UiMessage>,
}

impl ApplicationHandler for KeyCaptureApp {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        if self.window.is_some() {
            return;
        }
        let canvas = focus_sink_canvas();
        let attrs = Window::default_attributes()
            .with_canvas(Some(canvas))
            .with_append(true);
        self.window = Some(event_loop.create_window(attrs).unwrap());
    }

    fn window_event(
        &mut self,
        _event_loop: &ActiveEventLoop,
        _window_id: WindowId,
        event: WindowEvent,
    ) {
        if let WindowEvent::KeyboardInput { event, .. } = &event
            && let Some(msg) = self.keys.register_event(event)
        {
            match msg {
                UiMessage::Keystroke(key) => self.send.keystroke(key),
                UiMessage::Escape => self.key_proxy_send.send(UiMessage::Escape).unwrap(),
                _ => unreachable!(),
            }
        }
    }
}

/// Creates a canvas that covers the whole page but never intercepts pointer events, so it can sit
/// on top of the real UI purely to hold keyboard focus. A page-wide click listener keeps it
/// focused whenever focus lands elsewhere (e.g. after clicking a button).
fn focus_sink_canvas() -> web_sys::HtmlCanvasElement {
    let document = web_sys::window().unwrap().document().unwrap();
    let canvas: web_sys::HtmlCanvasElement = document
        .create_element("canvas")
        .unwrap()
        .dyn_into()
        .unwrap();
    let style = canvas.style();
    style.set_property("position", "fixed").unwrap();
    style.set_property("inset", "0").unwrap();
    style.set_property("pointer-events", "none").unwrap();
    style.set_property("opacity", "0").unwrap();

    let refocus_target = canvas.clone();
    let refocus = Closure::<dyn FnMut()>::new(move || {
        let _ = refocus_target.focus();
    });
    document
        .add_event_listener_with_callback("click", refocus.as_ref().unchecked_ref())
        .unwrap();
    refocus.forget();

    let _ = canvas.focus();
    canvas
}

pub fn main() {
    let conf = Config::read();
    let (send, recv) = EmuHandle::contruct_and_launch().split();

    let (key_proxy_send, key_proxy_recv) = unbounded_channel();

    let state = UiState::new(conf, send.clone(), recv, key_proxy_recv);

    let key_capture = KeyCaptureApp {
        window: None,
        keys: KeyWatcher::new(),
        send,
        key_proxy_send,
    };
    EventLoop::new().unwrap().spawn_app(key_capture);

    App::new(document_body(), state, UiState::app_logic).run();
}
