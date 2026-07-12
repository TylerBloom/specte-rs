use std::env::home_dir;

use bytes::Bytes;
use iced::Element;
use iced::Subscription;
use iced::keyboard::listen;
use iced::widget::Button;
use iced::widget::Image;
use iced::widget::Text;
use iced::widget::column;
use iced::widget::image::Handle;
use spirit::ButtonInput;
use spirit::JoypadInput;
use spirit::SsabInput;

use crate::config::Config;
use crate::emu_core::EmuSend;
use crate::keys::KeyWatcher;
use crate::keys::Keystroke;
use crate::trove::Trove;

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
    frames: usize,
}

pub struct SettingsState {}

#[derive(Debug)]
pub enum UiMessage {
    HomeMessage(HomeMessage),
    InGameMessage(InGameMessage),
    SettingsMessage(SettingsMessage),
    SwitchToSettings,
    Keystroke(Keystroke),
    Escape,
}

#[derive(Debug, Clone, PartialEq)]
pub enum HomeMessage {
    AddGame,
    StartGame(String),
}

#[derive(Debug)]
pub enum InGameMessage {
    NextFrame((Handle, usize)),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SettingsMessage {}

impl UiState {
    pub fn new(config: Config, send: EmuSend) -> Self {
        let trove = config.get_trove();
        let image = Handle::from_rgba(160, 144, Bytes::from(vec![0; 160 * 144]));
        Self {
            send,
            cursor: StateCursor::Home,
            home: HomeState { trove },
            game: InGameState { image, frames: 0 },
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
            UiMessage::Escape => {
                self.send.pause();
                Some(StateCursor::Home)
            }
            UiMessage::Keystroke(key) => {
                self.send.keystroke(key);
                None
            }
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
        let mapper = KeyWatcher::new();
        listen()
            .with(mapper)
            .filter_map(|(mapper, event)| mapper.register_event(event))
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
            InGameMessage::NextFrame((image, frames)) => {
                self.image = image;
                self.frames = frames
            }
        }
        None
    }

    pub fn view(&self) -> Element<'_, InGameMessage> {
        column![
            Text::from(&*format!("Frame #{}", self.frames).leak()),
            Image::new(self.image.clone())
        ]
        .into()
    }
}

impl SettingsState {
    fn update(&mut self, _msg: SettingsMessage) -> Option<StateCursor> {
        todo!()
    }

    pub fn view(&self) -> Element<'_, SettingsMessage> {
        todo!()
    }
}
