use std::sync::Arc;

use ghast::config::Config;
use ghast::emu_core::EmuCore;
use ghast::emu_core::EmuMessage;
use ghast::keys::KeyWatcher;
use ghast::state::UiMessage;
use ghast::state::UiState;

use clap::Parser;
use masonry::theme::default_property_set;
use masonry_winit::app::AppDriver;
use masonry_winit::app::MasonryUserEvent;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::mpsc::unbounded_channel;
use tracing_subscriber::EnvFilter;
use troupe::ActorBuilder;
use troupe::sink::SinkClient;
use winit::application::ApplicationHandler;
use winit::error::EventLoopError;
use winit::event::ElementState;
use winit::event::WindowEvent;
use winit::keyboard::KeyCode;
use winit::keyboard::PhysicalKey;
use xilem::EventLoop;
use xilem::WindowOptions;
use xilem::Xilem;

#[derive(Debug, Parser)]
struct Args {
    path: String,
}

/// An application not managed by Xilem, but which wishes to embed Xilem.
struct ExternalApp {
    masonry_state: masonry_winit::app::MasonryState<'static>,
    app_driver: Box<dyn AppDriver>,
    keys: KeyWatcher,
    key_proxy_send: UnboundedSender<UiMessage>,
    send: SinkClient<EmuMessage>,
}

impl ApplicationHandler<MasonryUserEvent> for ExternalApp {
    fn resumed(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        self.masonry_state
            .handle_resumed(event_loop, &mut *self.app_driver);
    }

    fn suspended(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        self.masonry_state.handle_suspended(event_loop);
    }

    fn about_to_wait(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        self.masonry_state.handle_about_to_wait(event_loop);
    }

    fn window_event(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        window_id: winit::window::WindowId,
        event: winit::event::WindowEvent,
    ) {
        if let WindowEvent::KeyboardInput { event, .. } = &event
            && let Some(msg) = self.keys.register_event(event)
        {
            match msg {
                UiMessage::Keystroke(key) => {
                    self.send.send(key);
                }
                UiMessage::Escape => self.key_proxy_send.send(UiMessage::Escape).unwrap(),
                _ => panic!(),
            }
        }
        self.masonry_state.handle_window_event(
            event_loop,
            window_id,
            event,
            self.app_driver.as_mut(),
        );
    }

    fn user_event(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        event: MasonryUserEvent,
    ) {
        self.masonry_state
            .handle_user_event(event_loop, event, self.app_driver.as_mut());
    }

    fn device_event(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        device_id: winit::event::DeviceId,
        event: winit::event::DeviceEvent,
    ) {
        // Handle the escape key to exit the app outside of masonry/xilem
        if let winit::event::DeviceEvent::Key(key) = &event
            && key.state == ElementState::Pressed
            && key.physical_key == PhysicalKey::Code(KeyCode::Escape)
        {
            event_loop.exit();
            return;
        }

        self.masonry_state.handle_device_event(
            event_loop,
            device_id,
            event,
            self.app_driver.as_mut(),
        );
    }

    fn new_events(
        &mut self,
        event_loop: &winit::event_loop::ActiveEventLoop,
        cause: winit::event::StartCause,
    ) {
        self.masonry_state.handle_new_events(event_loop, cause);
    }

    fn exiting(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        self.masonry_state.handle_exiting(event_loop);
    }

    fn memory_warning(&mut self, event_loop: &winit::event_loop::ActiveEventLoop) {
        self.masonry_state.handle_memory_warning(event_loop);
    }
}

fn main() -> Result<(), EventLoopError> {
    tracing_subscriber::fmt()
        .compact()
        .with_env_filter(EnvFilter::from_default_env())
        .init();

    let rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .unwrap();
    let rt = Arc::new(rt);

    let fut = {
        let rt = rt.clone();
        async move {
            let event_loop = EventLoop::with_user_event().build().unwrap();

            let conf = Config::read();

            let emu_client = ActorBuilder::new(EmuCore::new()).spawn();

            let (key_proxy_send, key_proxy_recv) = unbounded_channel();

            let state = UiState::new(conf, emu_client.clone(), key_proxy_recv);

            let window_size = winit::dpi::LogicalSize::new(800.0, 800.0);
            let window_options =
                WindowOptions::new("Specters - Ghast GBC").with_min_inner_size(window_size);

            let xilem = Xilem::new_simple_with_tokio(state, UiState::app_logic, window_options, rt);

            let proxy = event_loop.create_proxy();
            let (driver, windows) = xilem
                .into_driver_and_windows(move |event| proxy.send_event(event).map_err(|err| err.0));

            let masonry_state = masonry_winit::app::MasonryState::new(
                event_loop.create_proxy(),
                windows,
                default_property_set(),
            );

            let mut app = ExternalApp {
                masonry_state,
                app_driver: Box::new(driver),
                keys: KeyWatcher::new(),
                send: emu_client.sink(),
                key_proxy_send,
            };
            event_loop.run_app(&mut app)
        }
    };

    rt.block_on(fut)
}
