#![allow(unused)]
use std::{env::home_dir, path::PathBuf};

use config::Config;
use ghast::state::Emulator;

use clap::Parser;
use iced::{
    Element, Subscription, Task,
    widget::{Button, Column, Text, column},
};
use trove::Trove;

mod config;
mod trove;

#[derive(Debug, Parser)]
struct Args {
    path: String,
}

pub fn main() -> iced::Result {
    let conf = Config::read();
    iced::application("Specters - Ghast GBC", UiState::update, UiState::view)
        .subscription(UiState::subscription)
        .run_with(move || (UiState::new(conf), Task::none()))
}

#[derive(Debug, Clone, PartialEq)]
enum UiMessage {
    AddGameSet,
    LoadSettings,
}

enum UiState {
    Home(Trove),
}

impl UiState {
    pub fn new(config: Config) -> Self {
        // Determine how this should load (directly into a game, screenshot selection, or "home"
        // page) and return that state.
        // TODO: Actually implement this
        let trv = config.get_trove();
        Self::Home(trv)
    }

    pub fn update(&mut self, msg: UiMessage) {
        match msg {
            UiMessage::AddGameSet => {
                let path = home_dir().unwrap();
                let game = rfd::FileDialog::new().set_directory(&path).pick_file();
                if let Some(file) = game {
                    println!("Looking for ROM at {file:?}");
                    match self {
                        UiState::Home(trove) => trove.add_game(file),
                    };
                }
            }
            UiMessage::LoadSettings => todo!(),
        }
    }

    pub fn view(&self) -> Element<UiMessage> {
        match self {
            UiState::Home(trv) => column![self.settings_button(), trv.display()].into(),
        }
    }

    pub fn subscription(&self) -> Subscription<UiMessage> {
        Subscription::none()
    }

    fn settings_button(&self) -> Element<'static, UiMessage> {
        Button::new("Settings")
            .on_press(UiMessage::LoadSettings)
            .into()
    }
}

impl Trove {
    fn display(&self) -> Element<'static, UiMessage> {
        let children = std::iter::once("Trove".into())
            .chain(std::iter::once(self.add_game_set_button()))
            .chain(self.game_sets_display());
        Column::with_children(children).into()
    }

    fn add_game_set_button(&self) -> Element<'static, UiMessage> {
        Button::new("Add Game Set")
            .on_press(UiMessage::AddGameSet)
            .into()
    }

    fn game_sets_display(&self) -> impl IntoIterator<Item = Element<'static, UiMessage>> {
        std::fs::read_dir(&self.path)
            .unwrap()
            .map(Result::unwrap)
            .filter(|item| item.file_type().unwrap().is_dir())
            .map(|item| Text::new(item.file_name().to_str().unwrap().to_owned()))
            .map(Into::into)
    }
}
