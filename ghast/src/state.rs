use std::env::home_dir;

use xilem::AnyWidgetView;
use xilem::WidgetView;
use xilem::view::flex_col;
use xilem::view::image;
use xilem::view::label;
use xilem::view::text_button;

use crate::config::Config;
use crate::emu_core::EmuSend;
use crate::emu_core::Image;
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
    image: Image,
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
    NextFrame((Image, usize)),
}

#[derive(Debug, Clone, PartialEq)]
pub enum SettingsMessage {}

impl UiState {
    pub fn new(config: Config, send: EmuSend) -> Self {
        let trove = config.get_trove();
        let image = Image::empty();
        Self {
            send,
            cursor: StateCursor::Home,
            home: HomeState { trove },
            game: InGameState { image, frames: 0 },
            settings: SettingsState {},
        }
    }

    pub fn app_logic(&mut self) -> impl WidgetView<UiState> + use<> {
        self.view()
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

    pub fn view(&self) -> Box<AnyWidgetView<UiState>> {
        match self.cursor {
            StateCursor::Home => self.home.view().boxed(),
            StateCursor::InGame => self.game.view().boxed(),
            StateCursor::Settings => self.settings.view().boxed(),
        }
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

    pub fn view(&self) -> impl WidgetView<UiState> + use<> {
        flex_col((self.settings_button(), self.trove.display()))
    }

    fn settings_button(&self) -> impl WidgetView<UiState> + use<> {
        text_button("Settings", |_: &mut UiState| {})
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

    pub fn view(&self) -> impl WidgetView<UiState> + use<> {
        flex_col((
            label(format!("Frame #{}", self.frames)),
            image(self.image.0.clone())
        ))
    }
}

impl SettingsState {
    fn update(&mut self, _msg: SettingsMessage) -> Option<StateCursor> {
        todo!()
    }

    pub fn view(&self) -> impl WidgetView<UiState> + use<> {
        label("UNDER CONSTRUCTION!!!")
    }
}
