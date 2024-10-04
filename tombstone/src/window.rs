use std::sync::{Arc, Mutex};

use ghast::state::{Emulator, Message};
use iced::{Element, Subscription, Task};
use tokio::sync::broadcast::Receiver;
use tokio_stream::{wrappers::BroadcastStream, StreamExt};

pub struct WindowState {
    gb: Arc<Mutex<Emulator>>,
    inbound: Receiver<Message>,
}

impl WindowState {
    pub fn new(gb: Arc<Mutex<Emulator>>, inbound: Receiver<Message>) -> Self {
        Self { gb, inbound }
    }

    pub fn run(self) {
        std::thread::spawn(move || {
            iced::application(
                "Specters - Tombstone GBC Debugger",
                WindowState::update,
                WindowState::view,
            )
            .subscription(WindowState::subscription)
            .run_with(move || (self, Task::none()))
        });
    }

    fn update(&mut self, msg: Message) {
        self.gb.lock().unwrap().update(msg)
    }

    fn view(&self) -> Element<Message> {
        self.gb.lock().unwrap().view_owned()
    }

    fn subscription(&self) -> Subscription<Message> {
        let sub = self.gb.lock().unwrap().subscription();
        let recv = BroadcastStream::new(self.inbound.resubscribe()).map(Result::unwrap);
        Subscription::batch([sub, Subscription::run_with_id(0, recv)])
    }
}
