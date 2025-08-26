#![allow(unused)]
use std::{env::home_dir, path::PathBuf};

use config::Config;
use ghast::{
    emu_core::{EmuHandle, EmuRecv, EmuSend},
    state::Emulator,
};

use clap::Parser;
use iced::{
    Element, Subscription, Task,
    advanced::{
        graphics::image::image_rs::imageops::crop,
        image::{Bytes, Id},
    },
    widget::{Button, Column, Image, Text, column, image::Handle},
};
use tokio_stream::StreamExt;
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
        .run_with(move || {
            let (send, recv) = EmuHandle::contruct_and_launch().split();
            let stream = recv
                .into_stream()
                .map(InGameMessage::NextFrame)
                .map(UiMessage::InGameMessage);
            (UiState::new(conf, send), Task::stream(stream))
        })
}

struct UiState {
    send: EmuSend,
    cursor: StateCursor,
    home: HomeState,
    game: InGameState,
    settings: SettingsState,
}

#[derive(Debug, Clone, Copy)]
enum StateCursor {
    Home,
    InGame,
    Settings,
}

struct HomeState {
    trove: Trove,
}

struct InGameState {
    image: Handle,
}

struct SettingsState {}

#[derive(Debug)]
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

#[derive(Debug)]
enum InGameMessage {
    NextFrame(Handle),
}

#[derive(Debug, Clone, PartialEq)]
enum SettingsMessage {}

impl UiState {
    pub fn new(config: Config, send: EmuSend) -> Self {
        let trove = config.get_trove();
        let image = Handle::from_rgba(0, 0, Bytes::default());
        Self {
            send,
            cursor: StateCursor::Home,
            home: HomeState { trove },
            game: InGameState { image },
            settings: SettingsState {},
        }
    }

    pub fn update(&mut self, msg: UiMessage) {
        let cursor = match (self.cursor, msg) {
            (StateCursor::Home, UiMessage::HomeMessage(home_message)) => {
                self.home.update(&self.send, home_message)
            }
            (StateCursor::InGame, UiMessage::InGameMessage(in_game_message)) => {
                self.game.update(in_game_message)
            }
            (StateCursor::Settings, UiMessage::SettingsMessage(settings_message)) => {
                self.settings.update(settings_message)
            }
            (_, UiMessage::SwitchToSettings) => todo!(),
            _ => unreachable!(),
        };
        if let Some(cursor) = cursor {
            self.cursor = cursor;
        }
    }

    pub fn view(&self) -> Element<'_, UiMessage> {
        match self.cursor {
            StateCursor::Home => self.home.view().map(UiMessage::HomeMessage),
            StateCursor::InGame => self.game.view().map(UiMessage::InGameMessage),
            StateCursor::Settings => self.settings.view().map(UiMessage::SettingsMessage),
        }
    }

    pub fn subscription(&self) -> Subscription<UiMessage> {
        Subscription::none()
    }
}

impl HomeState {
    fn update(&mut self, send: &EmuSend, msg: HomeMessage) -> Option<StateCursor> {
        match msg {
            HomeMessage::AddGame => {
                let path = home_dir().unwrap();
                let game = rfd::FileDialog::new().set_directory(&path).pick_file();
                if let Some(file) = game {
                    self.trove.add_game(file);
                }
                None
            }
            HomeMessage::StartGame(file) => {
                let game = self.trove.fetch_game(file);
                send.start_game(game);
                Some(StateCursor::InGame)
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
    fn update(&mut self, msg: InGameMessage) -> Option<StateCursor> {
        match msg {
            InGameMessage::NextFrame(image) => self.image = image,
        }
        None
    }

    pub fn view(&self) -> Element<'_, InGameMessage> {
        Image::new(self.image.clone()).into()
    }
}

impl SettingsState {
    fn update(&mut self, msg: SettingsMessage) -> Option<StateCursor> {
        todo!()
    }

    pub fn view(&self) -> Element<'_, SettingsMessage> {
        todo!()
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
            .map(|item| {
                let file_name = item.file_name().to_str().unwrap().to_owned();
                Button::new(Text::new(file_name.clone()))
                    .on_press(HomeMessage::StartGame(file_name))
            })
            .map(Into::into)
    }
}
