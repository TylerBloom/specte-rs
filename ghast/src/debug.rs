use iced::{
    widget::{column, image::Handle, row, text, Column, Image, Row},
    Element,
};

use spirit::{
    cpu::check_bit_const,
    mem::OamObjectIndex,
    ppu::{zip_bits, Pixel},
    Gameboy,
};

/// Contains all of the data to extract out debug info from the GB.
pub struct Debugger(pub u8);

impl Debugger {
    pub fn view<M: 'static>(&self, gb: &Gameboy) -> impl Into<Element<'static, M>> {
        row![
            column![
                Column::from_vec(vram_0_to_tiles(gb, self.0).map(Into::into).collect()).spacing(3),
                Row::from_vec(tile_map_0(gb).map(Into::into).collect()).spacing(3),
            ]
            .spacing(8),
            column![
                Column::from_vec(vram_1_to_tiles(gb, self.0,).map(Into::into).collect()).spacing(3),
                Row::from_vec(tile_map_1(gb).map(Into::into).collect()).spacing(3),
            ]
            .spacing(3),
            Column::from_vec(oam_data(gb).map(Into::into).collect()).spacing(3),
        ]
        .spacing(8)
    }

    pub fn inc(&mut self) {
        self.0 += 1;
        self.0 %= 8;
    }
}

fn tile_map_0<M: 'static>(
    gb: &Gameboy,
) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    tile_map(&gb.mem.vram.vram[0][0x1800..0x1BFF])
}

fn tile_map_1<M: 'static>(
    gb: &Gameboy,
) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    tile_map(&gb.mem.vram.vram[0][0x1C00..0x1FFF])
}

fn tile_map<M: 'static>(map: &[u8]) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    let to_str = |index| text(format!("{index:0>2X}"));
    let mut cols = std::iter::once("XX ".to_owned())
        .chain((0..32).map(|i| format!("{i:0>2}")))
        .map(text)
        .map(Element::from)
        .map(|txt| {
            let mut val = Vec::with_capacity(33);
            val.push(txt);
            val
        })
        .collect::<Vec<Vec<Element<'static, M>>>>();
    cols[0].extend(
        (0..32)
            .map(|index| text(format!("{index:0>2X }")))
            .map(Into::into),
    );
    map.iter()
        .copied()
        .map(to_str)
        .map(Into::into)
        .enumerate()
        .for_each(|(i, txt)| cols[1 + i % 32].push(txt));
    cols.into_iter().map(Column::from_vec)
}

fn oam_data<M: 'static>(gb: &Gameboy) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    (0..40)
        .map(OamObjectIndex)
        .map(|i| gb.mem[i])
        .map(|obj| oam_obj_repr(gb, obj))
}

fn oam_obj_repr<M: 'static>(
    gb: &Gameboy,
    [y, x, index, attrs]: [u8; 4],
) -> impl Into<Element<'static, M>> {
    row![
        text(format!("y: {y}, ")),
        text(format!("x: {x}, ")),
        text(format!("index: 0x{index:0>2X}, ")),
        text(format!("attrs: 0b{attrs:0>8b}")),
    ]
    .spacing(3)
}

fn print_tile_map(gb: &Gameboy) {
    let map = if check_bit_const::<3>(gb.mem.io().lcd_control) {
        println!("The second tile map:");
        &gb.mem.vram.vram[0][0x1C00..0x2000]
    } else {
        println!("The first tile map:");
        &gb.mem.vram.vram[0][0x1800..0x1C00]
    };
    for i in 0..32 {
        print!("[");
        for j in 0..32 {
            print!(" {:0>2X},", map[i * 32 + j])
        }
        println!("]");
    }
}

fn vram_0_to_tiles<M: 'static>(
    gb: &Gameboy,
    palette: u8,
) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    vram_bank_to_tiles(gb, palette, &gb.mem.vram.vram[0])
}

fn vram_1_to_tiles<M: 'static>(
    gb: &Gameboy,
    palette: u8,
) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    vram_bank_to_tiles(gb, palette, &gb.mem.vram.vram[1])
}

fn vram_bank_to_tiles<'a, M: 'static>(
    gb: &'a Gameboy,
    palette: u8,
    bank: &'a [u8; 0x2000],
) -> impl 'a + Iterator<Item = impl Into<Element<'static, M>>> {
    let mut iter = (0..0x17FF)
        .step_by(16)
        .map(move |i| tile_data_to_pixels(gb, palette, (&bank[i..(i + 16)]).try_into().unwrap()));
    std::iter::once(
        Row::from_vec(
            std::iter::once("XX".to_owned())
                .chain((0..16).map(|i| format!("{i:X}")))
                .map(text)
                .map(Into::into)
                .collect(),
        )
        .spacing(2),
    )
    .chain(
        (0..0x18)
            .map(move |i| row![text(format!("{i:0>2X}")), tiles_into_rows(&mut iter),].spacing(3)),
    )
}

fn tiles_into_rows<M>(iter: impl Iterator<Item = [[Pixel; 8]; 8]>) -> Row<'static, M> {
    Row::from_vec(
        iter.take(0x10)
            .map(pixels_to_image)
            .map(Into::into)
            .collect(),
    )
    .spacing(3)
}

fn pixels_to_image(chunk: [[Pixel; 8]; 8]) -> Image<Handle> {
    Image::new(Handle::from_rgba(
        8,
        8,
        chunk
            .into_iter()
            .flatten()
            .flat_map(pixel_to_bytes)
            .collect::<Vec<_>>(),
    ))
}

fn tile_data_to_pixels(gb: &Gameboy, palette: u8, data: [u8; 16]) -> [[Pixel; 8]; 8] {
    let mut iter = (0..8)
        .map(|i| (data[2 * i], data[2 * i + 1]))
        .map(|(lo, hi)| bytes_to_pixels(gb, palette, lo, hi));
    std::array::from_fn(|_| iter.next().unwrap())
}

fn bytes_to_pixels(gb: &Gameboy, palette: u8, lo: u8, hi: u8) -> [Pixel; 8] {
    let palette = gb.mem.io().background_palettes[palette].clone();
    let mut iter = zip_bits(hi, lo).map(|c| palette.get_color(c).into());
    std::array::from_fn(|_| iter.next().unwrap())
}

pub fn pixel_to_bytes(Pixel { r, g, b }: Pixel) -> [u8; 4] {
    [r * 8, g * 8, b * 8, 255]
}
