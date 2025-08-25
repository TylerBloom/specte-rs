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

struct UiState {
    ctx: StateContext,
    inner: InnerUiState,
}

struct StateContext {
}

enum InnerUiState {
    Home(HomeState),
    InGame(InGameState),
    Settings(SettingsState),
}

struct HomeState {
    trove: Trove,
}

struct InGameState {}

struct SettingsState {}

#[derive(Debug, Clone, PartialEq)]
enum UiMessage {
    HomeMessage(HomeMessage),
    InGameMessage(InGameMessage),
    SettingsMessage(SettingsMessage),
    SwitchToSettings,
}

#[derive(Debug, Clone, PartialEq)]
enum HomeMessage {
    AddGame,
    StartGame(String),
}

#[derive(Debug, Clone, PartialEq)]
enum InGameMessage {}

#[derive(Debug, Clone, PartialEq)]
enum SettingsMessage {}

impl UiState {
    pub fn new(config: Config) -> Self {
        // Determine how this should load (directly into a game, screenshot selection, or "home"
        // page) and return that state.
        // TODO: Actually implement this
        let trove = config.get_trove();
        let home = HomeState { trove };
        Self::Home(home)
    }

    pub fn update(&mut self, msg: UiMessage) {
        match (self, msg) {
            (Self::Home(home), UiMessage::HomeMessage(home_message)) => home.update(home_message),
            (Self::InGame(in_game), UiMessage::InGameMessage(in_game_message)) => {
                in_game.update(in_game_message)
            }
            (Self::Settings(settings), UiMessage::SettingsMessage(settings_message)) => {
                settings.update(settings_message)
            }
            (_, UiMessage::SwitchToSettings) => todo!(),
            _ => unreachable!(),
        }
        /*
        UiMessage::AddGame(Str) => {

        }
        UiMessage::LoadSettings => todo!(),
        */
    }

    pub fn view(&self) -> Element<'_, UiMessage> {
        match self {
            UiState::Home(state) => state.view().map(UiMessage::HomeMessage),
            UiState::InGame(state) => state.view().map(UiMessage::InGameMessage),
            UiState::Settings(state) => state.view().map(UiMessage::SettingsMessage),
        }
    }

    pub fn subscription(&self) -> Subscription<UiMessage> {
        Subscription::none()
    }
}

impl HomeState {
    fn update(&mut self, msg: HomeMessage) {
        match msg {
            HomeMessage::AddGame => {
                let path = home_dir().unwrap();
                let game = rfd::FileDialog::new().set_directory(&path).pick_file();
                if let Some(file) = game {
                    println!("Looking for ROM at {file:?}");
                    self.trove.add_game(file);
                }
            }
            HomeMessage::StartGame(file) => {
                let data = self.trove.fetch_game(file);
            }
        }
    }

    pub fn view(&self) -> Element<'_, HomeMessage> {
        column![self.settings_button(), self.trove.display()].into()
    }

    pub fn subscription(&self) -> Subscription<HomeMessage> {
        Subscription::none()
    }

    fn settings_button(&self) -> Element<'static, HomeMessage> {
        Button::new("Settings").into()
    }
}

impl InGameState {
    fn update(&mut self, msg: InGameMessage) {
        todo!()
    }

    pub fn view(&self) -> Element<'_, InGameMessage> {
        todo!()
    }

    pub fn subscription(&self) -> Subscription<InGameMessage> {
        todo!()
    }
}

impl SettingsState {
    fn update(&mut self, msg: SettingsMessage) {
        todo!()
    }

    pub fn view(&self) -> Element<'_, SettingsMessage> {
        todo!()
    }

    pub fn subscription(&self) -> Subscription<SettingsMessage> {
        Subscription::none()
    }
}

impl Trove {
    fn display(&self) -> Element<'static, HomeMessage> {
        let children = std::iter::once("Trove".into())
            .chain(std::iter::once(self.add_game_set_button()))
            .chain(self.display_games());
        Column::with_children(children).into()
    }

    fn add_game_set_button(&self) -> Element<'static, HomeMessage> {
        Button::new("Add Game Set")
            .on_press(HomeMessage::AddGame)
            .into()
    }

    fn display_games(&self) -> impl IntoIterator<Item = Element<'static, HomeMessage>> {
        std::fs::read_dir(&self.path)
            .unwrap()
            .map(Result::unwrap)
            .filter(|item| item.file_type().unwrap().is_file())
            .map(|item| Text::new(item.file_name().to_str().unwrap().to_owned()))
            .map(Into::into)
    }
}
