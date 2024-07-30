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
            Column::from_vec(
                (0u8..(144 / 8))
                    .map(|y| {
                        Row::from_vec(
                            (0u8..(160 / 8))
                                .map(|x| {
                                    let data = (0..8)
                                        .flat_map(|dy| (0..8).map(move |dx| (dy, dx)))
                                        .flat_map(|(dy, dx)| {
                                            pixel_to_bytes(
                                                screen[8 * y as usize + dy][8 * x as usize + dx],
                                            )
                                        })
                                        .collect::<Vec<_>>();
                                    column![
                                        text(format!("({x:0>2}, {y:0>2})")),
                                        Image::new(Handle::from_pixels(8, 8, data)),
                                    ]
                                    .into()
                                })
                                .collect(),
                        )
                        .into()
                    })
                    .collect(),
            )
        ];
        Scrollable::new(col)
        /*
        self.gb.gb().ppu.screen.
        Row::from_vec(children)
            */
    }
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
