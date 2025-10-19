use std::sync::Arc;
use std::sync::Mutex;

use ghast::emu_core::Emulator;
use iced::Element;
use iced::Subscription;
use iced::Task;
use tokio::sync::broadcast::Receiver;
use tokio_stream::StreamExt;
use tokio_stream::wrappers::BroadcastStream;

use crate::WindowMessage;

/*
pub struct WindowState {
    gb: Arc<Mutex<Emulator>>,
    inbound: Receiver<WindowMessage>,
}

impl WindowState {
    pub fn new(gb: Arc<Mutex<Emulator>>, inbound: Receiver<WindowMessage>) -> Self {
        Self { gb, inbound }
    }

    pub fn run(self) {
        iced::application(
            "Specters - Tombstone GBC Debugger",
            WindowState::update,
            WindowState::view,
        )
        .subscription(WindowState::subscription)
        .run_with(move || (self, Task::none()));
    }

    fn update(&mut self, msg: Message) {
        self.gb.lock().unwrap().update(msg)
    }

    fn view(&self) -> Element<Message> {
        self.gb.lock().unwrap().view_owned()
    }

    fn subscription(&self) -> Subscription<Message> {
        let sub = self.gb.lock().unwrap().subscription();
        let recv = BroadcastStream::new(self.inbound.resubscribe())
            .map(Result::unwrap)
            .map(Message::from);
        Subscription::batch([sub, Subscription::run_with_id(0, recv)])
    }
}
*/
