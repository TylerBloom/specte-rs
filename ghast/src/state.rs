use iced::{
    widget::{column, image::Handle, row, text, Button, Column, Image, Scrollable},
    Alignment, Element, Subscription,
};
use spirit::{ppu::Pixel, Gameboy, StartUpSequence};

use crate::{
    debug::Debugger,
    utils::{pixel_to_bytes, scale_up_image, screen_to_image_scaled},
};

pub struct Emulator {
    gb: EmulatorInner,
    frame: usize,
    count: Option<usize>,
    dbg: Debugger,
    duplicated_screens: Option<Vec<Vec<Vec<Pixel>>>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Message {
    Play,
    Pause,
    Redraw,
    ScanLine,
    Step(usize),
    Tick,
    PaletteInc,
    Screens(Vec<Vec<Vec<Pixel>>>),
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

    pub fn frame_step(&mut self) {
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

    fn step_op(&mut self) {
        match self {
            EmulatorInner::StartUp(gb) => gb.as_mut().unwrap().step(),
            EmulatorInner::Ready(gb) => {
                gb.step().complete();
            },
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
        #[allow(clippy::ptr_arg)]
        fn create_image(screen: &Vec<Vec<Pixel>>) -> Image {
            let (width, height, image) = screen_to_image_scaled(screen, SCALE);
            assert_eq!(width * height * 4, image.len() as u32);
            Image::new(Handle::from_rgba(width, height, image))
        }
        const SCALE: usize = 4;
        let gb = self.gb.gb();
        let screen: Element<'static, Message> = match self.duplicated_screens.as_ref() {
            Some(screens) => {
                Column::from_vec(screens.iter().map(create_image).map(Into::into).collect::<Vec<_>>()).spacing(8).into()
            }
            None => create_image(&gb.ppu.screen).into(),
        };
        let col = row![self.dbg.view(gb).into(), screen];
        Scrollable::new(col)
    }

    pub fn gb(&self) -> &Gameboy {
        self.gb.gb()
    }

    pub fn next_screen(&mut self) {
        self.gb.frame_step()
    }

    pub fn step_op(&mut self) {
        self.gb.step_op()
    }
}

impl Default for Emulator {
    fn default() -> Self {
        let gb = Gameboy::new(include_bytes!("../../spirit/tests/roms/acid/cgb-acid2.gbc"));
        Self {
            gb: EmulatorInner::StartUp(Some(Box::new(gb))),
            count: Some(0),
            frame: 0,
            dbg: Debugger(0),
            duplicated_screens: None,
        }
    }
}

impl Emulator {
    pub fn update(&mut self, msg: Message) {
        match msg {
            Message::Play => {
                self.duplicated_screens = None;
                self.count = None;
            },
            Message::Pause => self.count = Some(0),
            Message::Step(count) => {
                if let Some(c) = self.count.as_mut() {
                    *c += count
                }
                if self.duplicated_screens.is_some() {
                    self.count = Some(0);
                }
            }
            Message::Tick => match self.count.as_mut() {
                Some(c) => {
                    if *c > 0 {
                        *c -= 1;
                        self.frame += 1;
                        if let Some(screens) = self.duplicated_screens.as_ref() {
                            if screens.len() == self.frame {
                                self.frame = 0;
                            }
                        }
                        self.gb.frame_step();
                    }
                }
                None => {
                    self.frame += 1;
                    if let Some(screens) = self.duplicated_screens.as_ref() {
                        if screens.len() == self.frame {
                            self.frame = 0;
                        }
                    }
                    self.gb.frame_step()
                }
            },
            Message::ScanLine => self.gb.scanline_step(),
            Message::PaletteInc => self.dbg.inc(),
            Message::Redraw => {}
            Message::Screens(screens) => {
                self.frame = 0;
                self.count = Some(0);
                self.duplicated_screens = Some(screens)
            }
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
                Button::new(text(self.to_frame_text(1)))
                    .on_press(Message::Step(1)),
                Button::new(text(self.to_frame_text(10)))
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

    fn to_frame_text(&self, inc: usize) -> String {
        match self.duplicated_screens.as_ref() {
            Some(screens) => format!("To frame {}/{}", self.frame + inc, screens.len()),
            None => format!("To frame {}", self.frame + inc),
        }
    }

    pub fn subscription(&self) -> Subscription<Message> {
        match self.count {
            Some(0) => Subscription::none(),
            _ => iced::time::every(std::time::Duration::from_millis(33)).map(|_| Message::Tick),
        }
    }
}
