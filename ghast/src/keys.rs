use iced::keyboard::Event;
use iced::keyboard::Key;
use iced::keyboard::key::Named;

#[derive(Debug)]
pub enum Keystroke {
    Escape,
}

impl Keystroke {
    pub fn convert(event: Event) -> Option<Self> {
        match event {
            Event::KeyPressed {
                key: Key::Named(Named::Escape),
                ..
            } => Some(Self::Escape),
            _ => None,
        }
    }
}
