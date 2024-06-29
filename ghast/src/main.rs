#![allow(unused)]

use iced::mouse::Cursor;
use iced::widget::canvas::fill::Rule;
use iced::widget::canvas::{Cache, Fill, Frame, Geometry, Program, Style};
use iced::widget::{button, column, text, Canvas};
use iced::{
    executor, Alignment, Application, Color, Element, Length, Point, Renderer, Sandbox, Settings, Size, Subscription, Theme
};
use spirit::ppu::Pixel;
use spirit::{Gameboy, StartUpSequence};

pub fn main() -> iced::Result {
    tracing_subscriber::fmt().init();
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
    fn screen(&self) -> Element<'_, ()> {
        Canvas::new(self)
            .width(Length::Fixed(160.0))
            .height(Length::Fixed(144.0))
            .into()
    }
}

impl Program<()> for &Example {
    type State = ();

    fn draw(
        &self,
        _state: &Self::State,
        renderer: &Renderer,
        _theme: &Theme,
        bounds: iced::Rectangle,
        _cursor: Cursor,
    ) -> Vec<Geometry> {
        let mut frame = Frame::new(renderer, bounds.size());
        self.gb
            .gb()
            .ppu
            .screen
            .iter()
            .enumerate()
            .flat_map(|(i, line)| line.iter().enumerate().map(move |(j, pixel)| (i, j, pixel)))
            .map(|(x, y, pixel)| {
                let point = Point {
                    x: x as f32 * 10.0,
                    y: y as f32 * 10.0,
                };
                let fill = Fill {
                    rule: Rule::NonZero,
                    style: Style::Solid(pixel_to_color(*pixel)),
                };
                (point, fill)
            })
            .for_each(|(point, fill)| {
                frame.fill_rectangle(
                    point,
                    Size {
                        width: 10.0,
                        height: 10.0,
                    },
                    fill,
                )
            });
        vec![frame.into_geometry()]
    }
}

fn pixel_to_color(Pixel { r, g, b }: Pixel) -> Color {
    Color {
        r: r as f32 / 0x1Fu8 as f32,
        g: g as f32 / 0x1Fu8 as f32,
        b: b as f32 / 0x1Fu8 as f32,
        a: 1.0,
    }
}

impl Application for Example {
    type Message = ();
    type Executor = executor::Default;
    type Theme = Theme;
    type Flags = ();

    fn new((): ()) -> (Self, iced::Command<()>) {
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

    fn update(&mut self, (): ()) -> iced::Command<()> {
        self.count += 1;
        self.gb.frame_step().complete();
        if self.gb.is_complete() {
            todo!()
        }
        self.cache.clear();
        iced::Command::none()
    }

    fn view(&self) -> Element<()> {
        column![
            text(format!("Gameboy start: Frame {}", self.count)).width(Length::Shrink).size(50),
            self.screen(),
            button("Clear").padding(8).on_press(()),
        ]
        .padding(20)
        .spacing(20)
        .align_items(Alignment::Center)
        .into()
    }

    fn subscription(&self) -> Subscription<()> {
        iced::time::every(std::time::Duration::from_millis(33)).map(|_| ())
    }
}
