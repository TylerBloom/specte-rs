use iced::keyboard::{key::Named, Key, Modifiers};



#[derive(Debug)]
pub enum Keystroke {
    Escape
}

impl Keystroke {
    pub fn convert(key: Key, mods: Modifiers) -> Option<Self> {
        match key {
            Key::Named(Named::Escape) => Some(Self::Escape),
            _ => None,
        }
    }
}
