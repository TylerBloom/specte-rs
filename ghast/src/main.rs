#![allow(unused)]

use iced::{Application, Settings};
use state::Example;

mod debug;
mod state;
mod utils;

pub fn main() -> iced::Result {
    Example::run(Settings {
        antialiasing: true,
        ..Settings::default()
    })
}
