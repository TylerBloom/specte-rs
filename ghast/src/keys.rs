use iced::keyboard::Key;
use iced::keyboard::Modifiers;
use iced::keyboard::key::Named;

#[derive(Debug)]
pub enum Keystroke {
    Escape,
}

impl Keystroke {
    pub fn convert(key: Key, _mods: Modifiers) -> Option<Self> {
        match key {
            Key::Named(Named::Escape) => Some(Self::Escape),
            _ => None,
        }
    }
}
