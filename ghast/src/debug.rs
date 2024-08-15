use iced::{
    widget::{image::Handle, row, Column, Image, Row},
    Element,
};
use spirit::{
    cpu::check_bit_const,
    ppu::{zip_bits, Pixel},
    Gameboy,
};

pub struct Debugger;

impl Debugger {
    pub fn view<M: 'static>(&self, gb: &Gameboy) -> impl Into<Element<'static, M>> {
        row![
            Column::from_vec(vram_0_to_tiles(gb).map(Into::into).collect()),
            Column::from_vec(vram_1_to_tiles(gb).map(Into::into).collect()),
        ]
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

fn vram_0_to_tiles<M: 'static>(
    gb: &Gameboy,
) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    let mut iter = (0..0x17FF)
        .step_by(16)
        .map(|i| tile_data_to_pixels(gb, (&gb.mem.vram.vram.0[i..(i + 16)]).try_into().unwrap()));
    // print_tile_map(gb);
    (0..0x20).map(move |_| tiles_into_rows(&mut iter))
}

fn vram_1_to_tiles<M: 'static>(
    gb: &Gameboy,
) -> impl '_ + Iterator<Item = impl Into<Element<'static, M>>> {
    let mut iter = (0..0x17FF)
        .step_by(16)
        .map(|i| tile_data_to_pixels(gb, (&gb.mem.vram.vram.1[i..(i + 16)]).try_into().unwrap()));
    (0..0x20).map(move |_| tiles_into_rows(&mut iter))
}

fn tiles_into_rows<M>(iter: impl Iterator<Item = [[Pixel; 8]; 8]>) -> Row<'static, M> {
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

pub fn pixel_to_bytes(Pixel { r, g, b }: Pixel) -> [u8; 4] {
    [r * 8, g * 8, b * 8, 255]
}
