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

struct Example {
    gb: StartUpSequence<'static>,
    count: usize,
    cache: Cache,
}

impl Example {
    fn screen(&self) -> impl Into<Element<usize>> {
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
            print!(" {:0>2X},", map[i*32 + j])
        }
        println!("]");
    }
}

fn vram_0_to_tiles(gb: &Gameboy) -> impl '_ + Iterator<Item = impl Into<Element<'static, usize>>> {
    let mut iter = (0..0x17FF)
        .step_by(16)
        .map(|i| {
            tile_data_to_pixels(gb, (&gb.mem.vram.vram.0[i..(i + 16)]).try_into().unwrap())
        });
    // print_tile_map(gb);
    (0..0x20).map(move |_| tiles_into_rows(&mut iter))
}

fn vram_1_to_tiles(gb: &Gameboy) -> impl '_ + Iterator<Item = impl Into<Element<'static, usize>>> {
    let mut iter = (0..0x17FF)
        .step_by(16)
        .map(|i| tile_data_to_pixels(gb, (&gb.mem.vram.vram.1[i..(i + 16)]).try_into().unwrap()));
    (0..0x20).map(move |_| tiles_into_rows(&mut iter))
}

fn tiles_into_rows(iter: impl Iterator<Item = [[Pixel; 8]; 8]>) -> Row<'static, usize> {
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
    let mut iter = data
        .into_iter()
        .skip(2)
        .zip(data.into_iter().skip(1).step_by(2))
        .map(|(lo, hi)| bytes_to_pixels(gb, lo, hi));
    std::array::from_fn(|_| iter.next().unwrap())
}

fn bytes_to_pixels(gb: &Gameboy, lo: u8, hi: u8) -> [Pixel; 8] {
    let palette = gb.mem.io().background_palettes[1u8].clone();
    let mut iter =
        zip_bits(hi, lo).map(|c| palette.get_color(c).into());
    std::array::from_fn(|_| iter.next().unwrap())
}

fn pixel_to_bytes(Pixel { r, g, b }: Pixel) -> [u8; 4] {
    [r * 8, g * 8, b * 8, 255]
}

impl Application for Example {
    type Message = usize;
    type Executor = executor::Default;
    type Theme = Theme;
    type Flags = ();

    fn new((): ()) -> (Self, iced::Command<usize>) {
        let gb: &'static mut Gameboy = Box::leak(Box::new(Gameboy::new(include_bytes!(
            "../../spirit/tests/roms/acid/which.gb"
        ))));
        (
            Self {
                gb: gb.start_up(),
                count: 0,
                cache: Cache::new(),
            },
            iced::Command::none(),
        )
    }

    fn title(&self) -> String {
        String::from("GameBoy!!!")
    }

    fn update(&mut self, count: usize) -> iced::Command<usize> {
        self.count += count;
        (0..count).for_each(|_| self.gb.frame_step().complete());
        if self.gb.is_complete() {
            todo!()
        }
        self.cache.clear();
        iced::Command::none()
    }

    fn view(&self) -> Element<usize> {
        column![
            row![
                Button::new(text(format!("To frame {}", self.count + 1))).on_press(1),
                Button::new(text(format!("To frame {}", self.count + 10))).on_press(10),
            ],
            self.screen().into(),
        ]
        .padding(20)
        .spacing(20)
        .align_items(Alignment::Center)
        .into()
    }

    /*
    fn subscription(&self) -> Subscription<()> {
        iced::time::every(std::time::Duration::from_millis(33)).map(|_| ())
    }
    */
}
