use std::cell::Cell;
use std::hash::Hash;
use std::sync::Arc;
use std::sync::Mutex;

use iced::keyboard::Event;
use iced::keyboard::Key;
use iced::keyboard::key::Named;
use spirit::ButtonInput;
use spirit::JoypadInput;
use spirit::SsabInput;
use tokio::time::Duration;
use tokio::time::Instant;

use crate::state::UiMessage;

/// The data to be passed into the emulation core mapped from keyboard inputs
#[derive(Debug, Clone, Copy)]
pub enum Keystroke {
    Control(ControlSignal),
    Button(ButtonInteration),
}

#[derive(Debug, Clone, Copy)]
pub enum ControlSignal {
    Pause,
    NextFrame,
}

#[derive(Debug, Clone, Copy)]
pub enum ButtonInteration {
    ButtonPress(ButtonInput),
    ButtonRelease(ButtonInput),
}

/// Encodes the outputs from the `KeyMapper`. Similar to `Keystroke` but stripped of press/release
/// notices.
#[derive(Debug, Clone, Copy)]
enum IntermediateKeystroke {
    Escape,
    Control(ControlSignal),
    Button(ButtonInput),
}

#[derive(Default, Clone)]
pub struct KeyWatcher(Arc<Mutex<KeyWatcherInner>>);

impl KeyWatcher {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn register_event(&self, event: Event) -> Option<UiMessage> {
        self.0.lock().unwrap().register_event(event)
    }
}

impl Hash for KeyWatcher {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.lock().unwrap().hash(state);
    }
}

/// Contains all of the state needed to map keyboard inputs to GBC button inputs.
///
/// Each field contains a simple state machine for tracking the time between initial button press
/// and release.
#[derive(Hash, Default)]
struct KeyWatcherInner {
    mapper: KeyMapper,
    up: bool,
    down: bool,
    left: bool,
    right: bool,
    a: bool,
    b: bool,
    start: bool,
    select: bool,
}

impl KeyWatcherInner {
    fn new() -> Self {
        Self::default()
    }

    fn register_event(&mut self, event: Event) -> Option<UiMessage> {
        match event {
            Event::KeyPressed { key, .. } => self.register_press(key),
            Event::KeyReleased { key, .. } => self.register_release(key),
            _ => None,
        }
    }

    fn register_press(&mut self, key: Key) -> Option<UiMessage> {
        match self.mapper.map(key)? {
            IntermediateKeystroke::Escape => Some(UiMessage::Escape),
            IntermediateKeystroke::Control(signal) => Some(Keystroke::Control(signal).into()),
            IntermediateKeystroke::Button(button) => {
                let field = self.button_ref(button);
                match field {
                    true => return None,
                    false => {
                        *field = true;
                        Some(Keystroke::Button(ButtonInteration::ButtonPress(button)).into())
                    }
                }
            }
        }
    }

    fn register_release(&mut self, key: Key) -> Option<UiMessage> {
        let IntermediateKeystroke::Button(button) = self.mapper.map(key)? else {
            return None;
        };
        *self.button_ref(button) = false;
        Some(Keystroke::Button(ButtonInteration::ButtonRelease(button)).into())
    }

    pub fn button_ref(&mut self, button: ButtonInput) -> &mut bool {
        match button {
            ButtonInput::Joypad(JoypadInput::Up) => &mut self.up,
            ButtonInput::Joypad(JoypadInput::Down) => &mut self.down,
            ButtonInput::Joypad(JoypadInput::Left) => &mut self.left,
            ButtonInput::Joypad(JoypadInput::Right) => &mut self.right,
            ButtonInput::Ssab(SsabInput::A) => &mut self.a,
            ButtonInput::Ssab(SsabInput::B) => &mut self.b,
            ButtonInput::Ssab(SsabInput::Start) => &mut self.start,
            ButtonInput::Ssab(SsabInput::Select) => &mut self.select,
        }
    }
}

/// Maps raw keyboard input into messages for the core emulator.
#[derive(Hash, Default)]
pub struct KeyMapper {}

impl KeyMapper {
    fn map(&self, event: Key) -> Option<IntermediateKeystroke> {
        match event {
            Key::Named(Named::Escape) => {
                Some(IntermediateKeystroke::Escape)
            }
            Key::Named(Named::Space) => Some(IntermediateKeystroke::Control(ControlSignal::Pause)),
            Key::Named(Named::ArrowRight) => {
                Some(IntermediateKeystroke::Control(ControlSignal::NextFrame))
            }
            Key::Named(Named::Enter) => Some(IntermediateKeystroke::Button(ButtonInput::Ssab(
                SsabInput::Start,
            ))),
            Key::Character(c) => match c.as_str() {
                "w" => Some(IntermediateKeystroke::Button(ButtonInput::Joypad(
                    JoypadInput::Up,
                ))),
                "s" => Some(IntermediateKeystroke::Button(ButtonInput::Joypad(
                    JoypadInput::Down,
                ))),
                "a" => Some(IntermediateKeystroke::Button(ButtonInput::Joypad(
                    JoypadInput::Left,
                ))),
                "d" => Some(IntermediateKeystroke::Button(ButtonInput::Joypad(
                    JoypadInput::Right,
                ))),
                "e" => Some(IntermediateKeystroke::Button(ButtonInput::Ssab(
                    SsabInput::A,
                ))),
                "f" => Some(IntermediateKeystroke::Button(ButtonInput::Ssab(
                    SsabInput::B,
                ))),
                "v" => Some(IntermediateKeystroke::Button(ButtonInput::Ssab(
                    SsabInput::Select,
                ))),
                _ => None,
            },
            _ => None,
        }
    }
}

impl From<Keystroke> for UiMessage {
    fn from(value: Keystroke) -> Self {
        UiMessage::Keystroke(value)
    }
}
