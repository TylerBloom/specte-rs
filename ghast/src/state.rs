use iced::{
    executor,
    widget::{column, image::Handle, row, text, Button, Image, Scrollable},
    Alignment, Application, Element, Subscription, Theme,
};
use spirit::{Gameboy, StartUpSequence};

use crate::debug::{pixel_to_bytes, Debugger};

pub struct Example {
    gb: Emulator,
    frame: usize,
    count: Option<usize>,
    dbg: Debugger,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Message {
    Play,
    Pause,
    ScanLine,
    Step(usize),
    Tick,
    PaletteInc,
}

#[allow(clippy::large_enum_variant)]
enum Emulator {
    StartUp(Option<Box<StartUpSequence>>),
    Ready(Gameboy),
}

impl Emulator {
    fn gb(&self) -> &Gameboy {
        match self {
            Emulator::StartUp(seq) => seq.as_ref().unwrap().gb(),
            Emulator::Ready(gb) => gb,
        }
    }

    fn is_complete(&self) -> bool {
        match self {
            Emulator::StartUp(seq) => seq.as_ref().unwrap().is_complete(),
            // TODO: This should probably check if the GB is halted...
            Emulator::Ready(gb) => gb.is_stopped(),
        }
    }

    fn frame_step(&mut self) {
        match self {
            Emulator::StartUp(seq) => {
                seq.as_mut().unwrap().frame_step().complete();
                if seq.as_ref().unwrap().is_complete() {
                    *self = Emulator::Ready(seq.take().unwrap().complete())
                }
            }
            Emulator::Ready(gb) => {
                if !gb.is_stopped() {
                    gb.next_frame().complete()
                } else {
                    println!("Gb is halted...");
                }
            }
        }
    }

    fn scanline_step(&mut self) {
        match self {
            Emulator::StartUp(seq) => {
                seq.as_mut().unwrap().scanline_step();
                if seq.as_ref().unwrap().is_complete() {
                    *self = Emulator::Ready(seq.take().unwrap().complete())
                }
            }
            Emulator::Ready(gb) => {
                if !gb.is_stopped() {
                    gb.scanline_step()
                }
            }
        }
    }
}

impl Example {
    fn screen(&self) -> impl Into<Element<Message>> {
        let gb = self.gb.gb();
        let screen = &gb.ppu.screen;
        let col = row![
            Image::new(Handle::from_pixels(
                160,
                144,
                screen
                    .iter()
                    .flatten()
                    .copied()
                    .flat_map(pixel_to_bytes)
                    .collect::<Vec<_>>(),
            )),
            self.dbg.view(gb).into(),
        ];
        Scrollable::new(col)
    }
}

impl Application for Example {
    type Message = Message;
    type Executor = executor::Default;
    type Theme = Theme;
    type Flags = ();

    fn new((): ()) -> (Self, iced::Command<Message>) {
        let gb = Gameboy::new(include_bytes!("../../spirit/tests/roms/acid/cgb-acid2.gbc"));
        (
            Self {
                gb: Emulator::StartUp(Some(Box::new(gb.start_up()))),
                count: Some(0),
                frame: 0,
                dbg: Debugger(0),
            },
            iced::Command::none(),
        )
    }

    fn title(&self) -> String {
        String::from("GameBoy!!!")
    }

    fn update(&mut self, msg: Message) -> iced::Command<Message> {
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
            Message::ScanLine => {
                self.gb.scanline_step()
            }
            Message::PaletteInc => self.dbg.inc(),
        }
        iced::Command::none()
    }

    fn view(&self) -> Element<Message> {
        column![
            row![
                Button::new(text(format!("To frame {}", self.frame + 1)))
                    .on_press(Message::Step(1)),
                Button::new(text(format!("To frame {}", self.frame + 10)))
                    .on_press(Message::Step(10)),
                Button::new(text("Next Scanline"))
                    .on_press(Message::ScanLine),
                Button::new(text("Run")).on_press(Message::Play),
                Button::new(text("Pause")).on_press(Message::Pause),
                Button::new(text(format!("Change palette from {}", self.dbg.0))).on_press(Message::PaletteInc),
            ],
            self.screen().into(),
        ]
        .padding(20)
        .spacing(20)
        .align_items(Alignment::Center)
        .into()
    }

    fn subscription(&self) -> Subscription<Message> {
        match self.count {
            Some(0) => Subscription::none(),
            _ => iced::time::every(std::time::Duration::from_millis(33)).map(|_| Message::Tick),
        }
    }
}
