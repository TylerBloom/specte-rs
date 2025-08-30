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
    iced::application("Specters - Ghast GBC", UiState::update, UiState::view)
        .subscription(UiState::subscription)
        .run_with(move || {
            let (send, recv) = EmuHandle::contruct_and_launch().split();
            let stream = recv
                .into_stream()
                .map(InGameMessage::NextFrame)
                .map(UiMessage::InGameMessage);
            (UiState::new(conf, send), Task::stream(stream))
        })
}
