use iced::Element;
use iced::widget::Column;
use iced::widget::Image;
use iced::widget::Row;
use iced::widget::image::Handle;
use iced::widget::row;
use iced::widget::text;

use spirit::Gameboy;
use spirit::cpu::check_bit_const;
use spirit::mem::OamObjectIndex;
use spirit::mem::ObjTileDataIndex;
use spirit::mem::WindowTileDataIndex;
use spirit::ppu::OamObject;
use spirit::ppu::Pixel;
use spirit::ppu::zip_bits;

use crate::utils::screen_to_image_scaled;

/// Contains all of the data to extract out debug info from the GB.
pub struct Debugger(pub u8);

impl Debugger {
    pub fn view<M: 'static>(&self, gb: &Gameboy) -> impl Iterator<Item = Element<'static, M>> {
        [
            Column::from_vec(tile_map_0(gb).map(Into::into).collect())
                .spacing(3)
                .into(),
            Column::from_vec(tile_map_1(gb).map(Into::into).collect())
                .spacing(3)
                .into(),
            Column::from_vec(vram_0_to_tiles(gb, self.0).map(Into::into).collect())
                .spacing(3)
                .into(),
            Column::from_vec(vram_1_to_tiles(gb, self.0).map(Into::into).collect())
                .spacing(3)
                .into(),
            Column::from_vec(oam_data(gb).map(Into::into).collect())
                .spacing(3)
                .into(),
        ]
        .into_iter()
    }

    pub fn inc(&mut self) {
        self.0 += 1;
        self.0 %= 8;
    }
}

fn tile_map_0<M: 'static>(
    gb: &Gameboy,
) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    tile_map(
        gb,
        &gb.mem.vram.vram[0][0x1800..=0x1BFF],
        &gb.mem.vram.vram[1][0x1800..=0x1BFF],
    )
}

fn tile_map_1<M: 'static>(
    gb: &Gameboy,
) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    tile_map(
        gb,
        &gb.mem.vram.vram[0][0x1C00..=0x1FFF],
        &gb.mem.vram.vram[1][0x1C00..=0x1FFF],
    )
}

fn tile_map<'a, M: 'static>(
    gb: &'a Gameboy,
    map: &'a [u8],
    attrs: &'a [u8],
) -> impl 'a + Iterator<Item = impl Into<Element<'static, M>>> {
    (0..map.len()).step_by(32).map(|i| {
        tiles_into_rows(
            32,
            (i..i + 32)
                .map(|i| (map[i], attrs[i]))
                .map(|(index, attr)| (gb.mem[WindowTileDataIndex { index, attr }], attr))
                .map(|(data, attr)| tile_data_to_pixels(gb, attr & 0b11, data)),
        )
    })
    // Needs an iterator over 8x8 grids of pixels
    /*
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
    */
}

fn oam_data<M: 'static>(gb: &Gameboy) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    (0..40)
        .map(OamObjectIndex)
        .map(|i| gb.mem[i])
        .map(|obj| oam_obj_repr(gb, obj))
}

fn oam_obj_repr<M: 'static>(gb: &Gameboy, obj_data: [u8; 4]) -> impl Into<Element<'static, M>> {
    let [y, x, index, attrs] = obj_data;
    let obj: [u8; 16] = (&gb.mem[ObjTileDataIndex(index, false)])
        .try_into()
        .unwrap();
    let expected_pixels = tile_data_to_pixels(gb, 4, obj);
    let obj = OamObject::new(obj_data);
    let actual_pixels = std::array::from_fn(|y| {
        let mut digest = obj
            .generate_pixels(obj.y.saturating_add(y as u8), &gb.mem)
            .map(|p| p.as_pixel(&gb.mem));
        digest.reverse();
        digest
    });
    row![
        text(format!("y: {y}, ")),
        text(format!("x: {x}, ")),
        text(format!("index: 0x{index:0>2X}, ")),
        text(format!("attrs: 0b{attrs:0>8b}")),
        pixels_to_image(expected_pixels),
        pixels_to_image(actual_pixels),
    ]
    .spacing(3)
}

#[allow(dead_code)]
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
    .chain((0..0x18).map(move |i| {
        row![text(format!("{i:0>2X}")), tiles_into_rows(0x10, &mut iter),].spacing(3)
    }))
}

fn tiles_into_rows<M>(len: usize, iter: impl Iterator<Item = [[Pixel; 8]; 8]>) -> Row<'static, M> {
    Row::from_vec(
        iter.take(len)
            .map(pixels_to_image)
            .map(Into::into)
            .collect(),
    )
    .spacing(3)
}

fn pixels_to_image(chunk: [[Pixel; 8]; 8]) -> Image<Handle> {
    const SCALE: usize = 4;
    let chunk = chunk.map(|mut row| {
        row.reverse();
        row
    });
    let (width, height, image) = screen_to_image_scaled(&chunk, SCALE);
    Image::new(Handle::from_rgba(width, height, image))
}

fn tile_data_to_pixels(gb: &Gameboy, palette: u8, data: [u8; 16]) -> [[Pixel; 8]; 8] {
    let mut iter = (0..8)
        .map(|i| (data[2 * i], data[2 * i + 1]))
        .map(|(lo, hi)| bytes_to_pixels(gb, palette, lo, hi));
    std::array::from_fn(|_| iter.next().unwrap())
}

fn bytes_to_pixels(gb: &Gameboy, palette: u8, lo: u8, hi: u8) -> [Pixel; 8] {
    /*
    let palette = gb.mem.io().background_palettes[palette].clone();
    let mut iter = zip_bits(hi, lo).map(|c| palette.get_color(c).into());
    std::array::from_fn(|_| iter.next().unwrap())
    */
    todo!()
}
