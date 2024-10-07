use iced::{
    widget::{column, image::Handle, row, text, Button, Image, Scrollable},
    Alignment, Element, Subscription,
};
use spirit::{Gameboy, StartUpSequence};

use crate::{
    debug::Debugger,
    utils::{pixel_to_bytes, scale_up_image, screen_to_image_scaled},
};

pub struct Emulator {
    gb: EmulatorInner,
    frame: usize,
    count: Option<usize>,
    dbg: Debugger,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Message {
    Play,
    Pause,
    Redraw,
    ScanLine,
    Step(usize),
    Tick,
    PaletteInc,
}

#[allow(clippy::large_enum_variant)]
enum EmulatorInner {
    StartUp(Option<Box<StartUpSequence>>),
    Ready(Gameboy),
}

impl EmulatorInner {
    pub fn gb(&self) -> &Gameboy {
        match self {
            EmulatorInner::StartUp(seq) => seq.as_ref().unwrap().gb(),
            EmulatorInner::Ready(gb) => gb,
        }
    }

    fn frame_step(&mut self) {
        match self {
            EmulatorInner::StartUp(seq) => {
                seq.as_mut().unwrap().frame_step().complete();
                if seq.as_ref().unwrap().is_complete() {
                    *self = EmulatorInner::Ready(seq.take().unwrap().complete())
                }
            }
            EmulatorInner::Ready(gb) => {
                if !gb.is_stopped() {
                    gb.next_frame().complete()
                }
            }
        }
    }

    fn scanline_step(&mut self) {
        match self {
            EmulatorInner::StartUp(seq) => {
                seq.as_mut().unwrap().scanline_step();
                if seq.as_ref().unwrap().is_complete() {
                    *self = EmulatorInner::Ready(seq.take().unwrap().complete())
                }
            }
            EmulatorInner::Ready(gb) => {
                if !gb.is_stopped() {
                    gb.scanline_step()
                }
            }
        }
    }
}

impl Emulator {
    fn screen(&self) -> impl Into<Element<'static, Message>> {
        const SCALE: usize = 4;
        let gb = self.gb.gb();
        let screen = &gb.ppu.screen;
        let (width, height, image) = screen_to_image_scaled(screen, SCALE);
        assert_eq!(width * height * 4, image.len() as u32);
        let col = row![
            self.dbg.view(gb).into(),
            Image::new(Handle::from_rgba(width, height, image)),
        ];
        Scrollable::new(col)
    }

    pub fn gb(&self) -> &Gameboy {
        self.gb.gb()
    }

    pub fn step_instruction(&mut self) {}
}

impl Default for Emulator {
    fn default() -> Self {
        let gb = Gameboy::new(include_bytes!("../../spirit/tests/roms/acid/cgb-acid2.gbc"));
        Self {
            gb: EmulatorInner::StartUp(Some(Box::new(gb))),
            count: Some(0),
            frame: 0,
            dbg: Debugger(0),
        }
    }
}

impl Emulator {
    pub fn update(&mut self, msg: Message) {
        match msg {
            Message::Play => self.count = None,
            Message::Pause => self.count = Some(0),
            Message::Step(count) => {
                if let Some(c) = self.count.as_mut() {
                    *c += count
                }
            }
            Message::Tick => match self.count.as_mut() {
                Some(c) => {
                    if *c > 0 {
                        *c -= 1;
                        self.frame += 1;
                        self.gb.frame_step();
                    }
                }
                None => {
                    self.frame += 1;
                    self.gb.frame_step()
                }
            },
            Message::ScanLine => self.gb.scanline_step(),
            Message::PaletteInc => self.dbg.inc(),
            Message::Redraw => {}
        }
    }

    pub fn view(&self) -> Element<Message> {
        self.view_owned()
    }

    /// This function only exists because `view` can not explicitly return an `Element<'static,
    /// Message>` for... reasons.
    pub fn view_owned(&self) -> Element<'static, Message> {
        column![
            row![
                Button::new(text(format!("To frame {}", self.frame + 1)))
                    .on_press(Message::Step(1)),
                Button::new(text(format!("To frame {}", self.frame + 10)))
                    .on_press(Message::Step(10)),
                Button::new(text("Next Scanline")).on_press(Message::ScanLine),
                Button::new(text("Run")).on_press(Message::Play),
                Button::new(text("Pause")).on_press(Message::Pause),
                Button::new(text(format!("Change palette from {}", self.dbg.0)))
                    .on_press(Message::PaletteInc),
            ],
            self.screen().into(),
        ]
        .padding(20)
        .spacing(20)
        .align_x(Alignment::Center)
        .into()
    }

    pub fn subscription(&self) -> Subscription<Message> {
        match self.count {
            Some(0) => Subscription::none(),
            _ => iced::time::every(std::time::Duration::from_millis(33)).map(|_| Message::Tick),
        }
    }
}
