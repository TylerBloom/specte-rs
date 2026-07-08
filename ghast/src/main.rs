use clap::Parser;

use iced::Task;
use tokio_stream::StreamExt;

use ghast::config::Config;
use ghast::emu_core::EmuHandle;
use ghast::state::InGameMessage;
use ghast::state::UiMessage;
use ghast::state::UiState;

#[derive(Debug, Parser)]
struct Args {
    path: String,
}

pub fn main() -> iced::Result {
    let conf = Config::read();
    let boot_fn = move || {
        let (send, recv) = EmuHandle::contruct_and_launch().split();
        let stream = recv
            .into_stream()
            .map(InGameMessage::NextFrame)
            .map(UiMessage::InGameMessage);
        (UiState::new(conf.clone(), send), Task::stream(stream))
    };
    iced::application(boot_fn, UiState::update, UiState::view)
        .title("Specters - Ghast GBC")
        .subscription(UiState::subscription)
        .run()
}
