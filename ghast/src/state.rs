use std::borrow::Cow;
use std::env::home_dir;
use std::path::PathBuf;

use clap::Parser;
use iced::Alignment;
use iced::Element;
use iced::Subscription;
use iced::Task;
use iced::advanced::graphics::image::image_rs::imageops::crop;
use iced::advanced::image::Bytes;
use iced::advanced::image::Id;
use iced::keyboard::on_key_press;
use iced::widget::Button;
use iced::widget::Column;
use iced::widget::Image;
use iced::widget::Scrollable;
use iced::widget::Text;
use iced::widget::column;
use iced::widget::image::Handle;
use iced::widget::row;
use iced::widget::text;
use tokio_stream::StreamExt;

use spirit::Gameboy;
use spirit::StartUpSequence;
use spirit::ppu::Pixel;

use crate::config::Config;
use crate::debug::Debugger;
use crate::emu_core::EmuHandle;
use crate::emu_core::EmuRecv;
use crate::emu_core::EmuSend;
use crate::keys::Keystroke;
use crate::trove::Trove;
use crate::utils::pixel_to_bytes;
use crate::utils::scale_up_image;

pub struct UiState {
    send: EmuSend,
    cursor: StateCursor,
    home: HomeState,
    game: InGameState,
    settings: SettingsState,
}

#[derive(Debug, Clone, Copy)]
pub enum StateCursor {
    Home,
    InGame,
    Settings,
}

pub struct HomeState {
    trove: Trove,
}

pub struct InGameState {
    image: Handle,
}

pub struct SettingsState {}

#[derive(Debug)]
pub enum UiMessage {
    HomeMessage(HomeMessage),
    InGameMessage(InGameMessage),
    SettingsMessage(SettingsMessage),
    SwitchToSettings,
    Keystroke(Keystroke),
}

#[derive(Debug, Clone, PartialEq)]
pub enum HomeMessage {
    AddGame,
    StartGame(String),
}

#[derive(Debug)]
pub enum InGameMessage {
    NextFrame(Handle),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SettingsMessage {}

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
        let cursor = match msg {
            UiMessage::HomeMessage(msg) if matches!(self.cursor, StateCursor::Home) => {
                self.home.update(&self.send, msg)
            }
            UiMessage::HomeMessage(_) => unreachable!(),
            UiMessage::InGameMessage(msg) if matches!(self.cursor, StateCursor::InGame) => {
                self.game.update(msg)
            }
            UiMessage::InGameMessage(_) => None,
            UiMessage::SettingsMessage(msg) if matches!(self.cursor, StateCursor::Settings) => {
                self.settings.update(msg)
            }
            UiMessage::SettingsMessage(_) => unreachable!(),
            UiMessage::SwitchToSettings => Some(StateCursor::Settings),
            UiMessage::Keystroke(key) => match key {
                Keystroke::Escape => {
                    self.send.pause();
                    Some(StateCursor::Home)
                }
            },
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
        on_key_press(Keystroke::convert).map(UiMessage::Keystroke)
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
