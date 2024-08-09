#![allow(unused)]

use iced::mouse::Cursor;
use iced::widget::canvas::fill::Rule;
use iced::widget::canvas::{Cache, Fill, Frame, Geometry, Program, Style};
use iced::widget::image::{viewer, Handle, Viewer};
use iced::widget::{button, column, row, text, Button, Canvas, Column, Image, Row, Scrollable};
use iced::{
    executor, Alignment, Application, Color, Element, Length, Point, Renderer, Sandbox, Settings,
    Size, Subscription, Theme,
};
use spirit::cpu::check_bit_const;
use spirit::mem::{BgTileDataIndex, BgTileMapAttrIndex, BgTileMapIndex};
use spirit::ppu::{zip_bits, Pixel};
use spirit::{Gameboy, StartUpSequence};

pub fn main() -> iced::Result {
    // tracing_subscriber::fmt().init();
    Example::run(Settings {
        antialiasing: true,
        ..Settings::default()
    })
}

// NOTE: The start up sequence is only rarely used. Even though the GB is *much* larger, this will
// have little impact on performance.
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
            Emulator::Ready(gb) => gb.is_halted(),
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
                if !gb.is_halted() {
                    gb.next_frame().complete()
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Message {
    Play,
    Pause,
    Step(usize),
    Tick,
}

struct Example {
    gb: Emulator,
    frame: usize,
    count: Option<usize>,
    cache: Cache,
}

impl Example {
    fn screen(&self) -> impl Into<Element<Message>> {
        let screen = &self.gb.gb().ppu.screen;
        let col = column![
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
            row![
                Column::from_vec(vram_0_to_tiles(self.gb.gb()).map(Into::into).collect()),
                Column::from_vec(vram_1_to_tiles(self.gb.gb()).map(Into::into).collect()),
            ],
        ];
        Scrollable::new(col)
    }
}

fn print_tile_map(gb: &Gameboy) {
    let map = if check_bit_const::<3>(gb.mem.io().lcd_control) {
        println!("The second tile map:");
        &gb.mem.vram.vram.0[0x1C00..0x2000]
    } else {
        println!("The first tile map:");
        &gb.mem.vram.vram.0[0x1800..0x1C00]
    };
    for i in 0..32 {
        print!("[");
        for j in 0..32 {
            print!(" {:0>2X},", map[i * 32 + j])
        }
        println!("]");
    }
}

fn vram_0_to_tiles(
    gb: &Gameboy,
) -> impl '_ + Iterator<Item = impl Into<Element<'static, Message>>> {
    let mut iter = (0..0x17FF)
        .step_by(16)
        .map(|i| tile_data_to_pixels(gb, (&gb.mem.vram.vram.0[i..(i + 16)]).try_into().unwrap()));
    // print_tile_map(gb);
    (0..0x20).map(move |_| tiles_into_rows(&mut iter))
}

fn vram_1_to_tiles(
    gb: &Gameboy,
) -> impl '_ + Iterator<Item = impl Into<Element<'static, Message>>> {
    let mut iter = (0..0x17FF)
        .step_by(16)
        .map(|i| tile_data_to_pixels(gb, (&gb.mem.vram.vram.1[i..(i + 16)]).try_into().unwrap()));
    (0..0x20).map(move |_| tiles_into_rows(&mut iter))
}

fn tiles_into_rows(iter: impl Iterator<Item = [[Pixel; 8]; 8]>) -> Row<'static, Message> {
    Row::from_vec(
        iter.take(0x10)
            .map(pixels_to_image)
            .map(Into::into)
            .collect(),
    )
}

fn pixels_to_image(chunk: [[Pixel; 8]; 8]) -> Image<Handle> {
    Image::new(Handle::from_pixels(
        8,
        8,
        chunk
            .into_iter()
            .flatten()
            .flat_map(pixel_to_bytes)
            .collect::<Vec<_>>(),
    ))
}

fn tile_data_to_pixels(gb: &Gameboy, data: [u8; 16]) -> [[Pixel; 8]; 8] {
    let mut iter = (0..8)
        .map(|i| (data[2 * i], data[2 * i + 1]))
        .map(|(lo, hi)| bytes_to_pixels(gb, lo, hi));
    std::array::from_fn(|_| iter.next().unwrap())
}

fn bytes_to_pixels(gb: &Gameboy, lo: u8, hi: u8) -> [Pixel; 8] {
    let palette = gb.mem.io().background_palettes[1u8].clone();
    let mut iter = zip_bits(hi, lo).map(|c| palette.get_color(c).into());
    std::array::from_fn(|_| iter.next().unwrap())
}

fn pixel_to_bytes(Pixel { r, g, b }: Pixel) -> [u8; 4] {
    [r * 8, g * 8, b * 8, 255]
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
                cache: Cache::new(),
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
        }
        self.cache.clear();
        iced::Command::none()
    }

    fn view(&self) -> Element<Message> {
        column![
            row![
                Button::new(text(format!("To frame {}", self.frame + 1)))
                    .on_press(Message::Step(1)),
                Button::new(text(format!("To frame {}", self.frame + 10)))
                    .on_press(Message::Step(10)),
                Button::new(text("Run")).on_press(Message::Play),
                Button::new(text("Pause")).on_press(Message::Pause),
            ],
            self.screen().into(),
        ]
        .padding(20)
        .spacing(20)
        .align_items(Alignment::Center)
        .into()
    }

    fn subscription(&self) -> Subscription<Message> {
        iced::time::every(std::time::Duration::from_millis(33)).map(|_| Message::Tick)
    }
}
