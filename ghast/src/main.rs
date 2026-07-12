use clap::Parser;

use iced::Task;
use tokio_stream::StreamExt;

use ghast::config::Config;
use ghast::emu_core::EmuHandle;
use ghast::state::InGameMessage;
use ghast::state::UiMessage;
use ghast::state::UiState;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::util::SubscriberInitExt;

#[derive(Debug, Parser)]
struct Args {
    path: String,
}

pub fn main() -> iced::Result {
    tracing_subscriber::fmt()
        .compact()
        .with_env_filter(EnvFilter::from_default_env())
        .init();
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
