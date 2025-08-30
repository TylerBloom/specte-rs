#![allow(unused)]
use std::env::home_dir;
use std::path::PathBuf;

use clap::Parser;
use ghast::keys::Keystroke;
use ghast::state::InGameMessage;
use ghast::state::UiMessage;
use ghast::state::UiState;
use iced::keyboard::on_key_press;
use iced::Element;
use iced::Subscription;
use iced::Task;
use iced::advanced::graphics::image::image_rs::imageops::crop;
use iced::advanced::image::Bytes;
use iced::advanced::image::Id;
use iced::widget::Button;
use iced::widget::Column;
use iced::widget::Image;
use iced::widget::Text;
use iced::widget::column;
use iced::widget::image::Handle;
use tokio_stream::StreamExt;

use ghast::config::Config;
use ghast::emu_core::EmuHandle;
use ghast::emu_core::EmuRecv;
use ghast::emu_core::EmuSend;
use ghast::emu_core::Emulator;
use ghast::trove::Trove;

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
