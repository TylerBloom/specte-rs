use std::{array, collections::VecDeque};

use heapless::Vec as InlineVec;
use tracing::{info_span, trace};

use crate::{
    cpu::{check_bit, check_bit_const},
    mem::{
        io::{BgPaletteIndex, ObjPaletteIndex},
        vram::{PpuMode, VRam},
        BgTileDataIndex, BgTileMapAttrIndex, BgTileMapIndex, MemoryMap, OamIndex, OamObjectIndex,
        ObjTileDataIndex,
    },
};

// Notes:
// A tile is 16 bytes, which means that each line is 2 bytes.
// Every pixel has a color-depth of 2. The nth bit of the first byte holds the nth pixel's least
// significant bit of the color depth. The most significant bit is in the corresponding bit of
// second byte.

// Plan of attack:
//  - Implement indexing into VRAM
//  - Disregard rendering everything except the background. Also ignore all scrolling effects
//  - Make it possible to draw to a GUI (i.e. add the plumbing and signalling)!!
//  - Add in sprite/object rendering
//  - Add in window rendering
//  - Add in scrolling effects

// TODO:
// 1) There is a gap (12 ticks) between the start of mode 3 and when the first pixel can be output.
//    This is determined by the SCX register. Pixels are discarded based on this.
// 2) There are some quirks with when access to VRAM is cut off for the PPU. These are not
//    implemented but probably should be...
// 3) For now, the PPU will hold a buffer for the pixels. This will likely get moved out.
// 4) There is logic here assuming that the screen will always have 144 elements. That is fine
//    (though, ideally, we would able to represent that with heap-allocated arrays), but we need to
//    be mindful of this when it comes to deserialization. Initially, we can ignore the screen in
//    serde, but eventually, we can also store it (mostly to show the screen save). This will
//    require a hand-roll deser impl or something from a third-party crate.

/// The Pixel Processing Unit
#[derive(Debug, Hash)]
pub struct Ppu {
    // This represents the LCD screen. This will always have a length of 144.
    // Pixels are pushed from the
    pub inner: PpuInner,
    pub screen: Vec<[Pixel; 160]>,
}

#[derive(Debug, Hash)]
pub struct OamObject {
    y: u8,
    x: u8,
    tile_index: u8,
    attrs: u8,
}

impl OamObject {
    fn new(data: &[u8; 4]) -> Self {
        Self {
            y: data[0],
            x: data[1],
            tile_index: data[2],
            attrs: data[3],
        }
    }

    fn populate_buffer(self, y: u8, buffer: &mut [FiFoPixel], mem: &MemoryMap) {
        let x = self.x as usize;
        self.generate_pixels(y, mem)
            .into_iter()
            .enumerate()
            .for_each(|(i, pixel)| buffer[x + i] = pixel);
    }

    fn generate_pixels(self, y: u8, mem: &MemoryMap) -> [FiFoPixel; 8] {
        // The given Y needs to be within the bounds of the object. This should be the case
        // already.
        debug_assert!(y >= self.y);
        debug_assert!(y < self.y + 8);
        let obj = &mem[ObjTileDataIndex(self.tile_index, check_bit_const::<3>(self.attrs))];
        let mut index = (self.y % 8) as usize;
        if check_bit_const::<6>(self.attrs) {
            // 0 <= index < 8 and we want to invert the space
            index = !index;
        }
        let lo = obj[2 * index];
        let hi = obj[2 * index + 1];
        let mut digest = form_pixels(self.attrs, lo, hi).into_array().unwrap();
        if check_bit_const::<5>(self.attrs) {
            digest.as_mut_slice().reverse();
        }
        digest
    }
}

#[derive(Debug, Hash)]
pub enum PpuInner {
    OamScan {
        dots: u8,
        y: u8,
    },
    Drawing {
        obj_fifo: ObjectFiFo,
        bg_fifo: BackgroundFiFo,
        dots: u16,
        x: u8,
        y: u8,
    },
    HBlank {
        dots: u16,
        y: u8,
    },
    VBlank {
        dots: u16,
    },
}

impl Default for PpuInner {
    fn default() -> Self {
        Self::OamScan { dots: 0, y: 0 }
    }
}

impl PpuInner {
    fn tick(&mut self, screen: &mut [[Pixel; 160]], mem: &mut MemoryMap) {
        match self {
            PpuInner::OamScan { dots, y } if *dots == 79 => {
                *self = Self::Drawing {
                    bg_fifo: BackgroundFiFo::new(*y),
                    y: *y,
                    dots: 0,
                    x: 0,
                    obj_fifo: ObjectFiFo::new(*y, mem),
                };
            }
            PpuInner::OamScan { dots, y } => *dots += 1,
            PpuInner::Drawing { dots, x, y, .. } if *x == 160 => {
                *self = Self::HBlank { dots: *dots, y: *y };
            }
            PpuInner::Drawing {
                bg_fifo,
                dots,
                x,
                y,
                obj_fifo,
            } => {
                *dots += 1;
                bg_fifo.tick(mem);
                let obj_pixel = obj_fifo.pop_pixel();
                if let Some(pixel) = bg_fifo.pop_pixel() {
                    screen[*y as usize][*x as usize] = pixel.mix(obj_pixel, mem);
                    *x += 1;
                    if *x == 160 {
                        *self = Self::HBlank { dots: *dots, y: *y };
                    }
                }
            }
            // This measures the number of ticks after mode 2 because all modes added together is
            // 456 and mode 2 is always 80 dots
            PpuInner::HBlank { dots, y } if *dots == 375 => {
                mem.inc_lcd_y();
                let y = *y + 1;
                *self = if y == 144 {
                    mem.request_vblank_int();
                    Self::VBlank { dots: 0 }
                } else {
                    Self::OamScan { dots: 0, y }
                };
            }
            PpuInner::HBlank { dots, .. } => *dots += 1,
            PpuInner::VBlank { dots } if *dots == 4559 => {
                *self = Self::OamScan { dots: 0, y: 0 };
            }
            PpuInner::VBlank { dots, .. } => *dots += 1,
        }
    }

    fn state(&self) -> PpuMode {
        match self {
            PpuInner::OamScan { .. } => PpuMode::OamScan,
            PpuInner::Drawing { .. } => PpuMode::Drawing,
            PpuInner::HBlank { .. } => PpuMode::HBlank,
            PpuInner::VBlank { .. } => PpuMode::VBlank,
        }
    }
}

// TODO: This needs to track what model it is in (not VBLANK, though) and pass that to the PPU
#[derive(Debug, Hash)]
struct ObjectFiFo {
    pixels: VecDeque<FiFoPixel>,
}

impl ObjectFiFo {
    fn new(y: u8, mem: &MemoryMap) -> Self {
        let mut pixels = std::iter::repeat(FiFoPixel::transparent())
            .take(176)
            .collect::<VecDeque<_>>();
        let y = y + 16;
        let buffer = pixels.make_contiguous();
        let objects = (0..40)
            .map(|i| &mem[OamObjectIndex(i)])
            // TODO: Right now, we are assuming that all objects are 8x8. We need to add a check for
            // 8x16 objects.
            .filter(|[y_pos, ..]| y <= *y_pos && *y_pos < y + 8)
            .map(OamObject::new)
            .take(10)
            .for_each(|obj| obj.populate_buffer(y, buffer, mem));
        // Discard the front and back 8 pixels
        for _ in 0..8 {
            pixels.pop_front();
            pixels.pop_back();
        }
        Self { pixels }
    }

    fn pop_pixel(&mut self) -> FiFoPixel {
        self.pixels.pop_front().unwrap_or(FiFoPixel::transparent())
    }
}

// TODO: This needs to track what model it is in (not VBLANK, though) and pass that to the PPU
#[derive(Debug, Hash)]
struct BackgroundFiFo {
    counter: u8,
    tile_count: u8,
    fetcher: PixelFetcher,
    background: InlineVec<FiFoPixel, 8>,
}

impl BackgroundFiFo {
    fn new(y: u8) -> Self {
        Self {
            counter: 0,
            tile_count: 0,
            fetcher: PixelFetcher::new(y),
            background: InlineVec::new(),
        }
    }

    fn tick(&mut self, mem: &MemoryMap) {
        self.counter += 1;
        let bg = self.background.is_empty().then(|| {
            self.tile_count += 1;
            &mut self.background
        });
        self.fetcher.tick(mem, bg);
    }

    fn pop_pixel(&mut self) -> Option<FiFoPixel> {
        self.background.pop()
    }
}

#[derive(Debug, Default, Hash, Clone, Copy)]
pub struct FiFoPixel {
    /// Which color for the selected palette should be used. Ranges from 0 to 3.
    color: u8,
    /// Color palette to use. Ranges from 0 to 7.
    palette: u8,
    // TODO: For now, this is ignored
    sprite_priority: bool,
    // TODO: For now, this is ignored
    bg_priority: bool,
}

impl FiFoPixel {
    fn new(color: u8, palette: u8) -> Self {
        Self {
            color,
            palette,
            sprite_priority: false,
            bg_priority: false,
        }
    }

    /// This is the same as `Self::default`, but with a clearer name
    #[inline]
    const fn transparent() -> Self {
        Self {
            color: 0,
            palette: 0,
            sprite_priority: false,
            bg_priority: false,
        }
    }

    fn mix(self, obj: Self, mem: &MemoryMap) -> Pixel {
        // TODO: The pixels carry the data to determine this, but that is not being properly
        // determined yet.
        if obj.color == 0 {
            mem[BgPaletteIndex(self.palette)].get_color(self.color)
        } else {
            mem[ObjPaletteIndex(obj.palette)].get_color(obj.color)
        }
        .into()
    }
}

/// The final pixel that is available to the end consumer.
#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Pixel {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Pixel {
    const WHITE: Self = Self {
        r: 0x1F,
        g: 0x1F,
        b: 0x1F,
    };
    const BLACK: Self = Self { r: 0, g: 0, b: 0 };

    const fn new() -> Self {
        Self { r: 0, g: 0, b: 0 }
    }
}

/// Each variant has an x and y value. These are the coordinates into the tile map. These values do
/// not change until the fetcher is reset. Similarly, most variants have a "ticked" value. Very
/// step but the last takes two ticks to complete. That bool tracks if it has been ticked or not.
///
/// Each variant represents the next step that is to be taken. For example, if the fetcher is the
/// `DataLow` variant, that means that it has all of the data it needs to execute that step.
/// Ticking it will move it will move it to the next state by fetching any additional data and
/// performing any calculations.
// TODO: There are various things that can affect how the fetcher indexes into the tile map, the
// tile data, etc. To get a debuggable build, this is all being ignored and will be impl-ed in the
// future.
#[derive(Debug, Hash, Clone, Copy)]
enum PixelFetcher {
    GetTile {
        ticked: bool,
        x: u8,
        y: u8,
    },
    DataLow {
        ticked: bool,
        x: u8,
        y: u8,
        attr: u8,
        index: u8,
    },
    DataHigh {
        ticked: bool,
        x: u8,
        y: u8,
        attr: u8,
        index: u8,
        lo: u8,
    },
    Sleep {
        ticked: bool,
        x: u8,
        y: u8,
        attr: u8,
        lo: u8,
        hi: u8,
    },
    Push {
        x: u8,
        y: u8,
        attr: u8,
        lo: u8,
        hi: u8,
    },
}

impl PixelFetcher {
    fn new(y: u8) -> Self {
        Self::GetTile {
            ticked: false,
            x: 0,
            y,
        }
    }

    fn tick(&mut self, mem: &MemoryMap, pixels_out: Option<&mut InlineVec<FiFoPixel, 8>>) {
        match self {
            PixelFetcher::GetTile { ticked, .. } if !*ticked => *ticked = true,
            PixelFetcher::GetTile { ticked, x, y } => {
                let x = *x;
                let y = *y;
                let index = mem[BgTileMapIndex { x, y }];
                let attr = mem[BgTileMapAttrIndex { x, y }];
                *self = Self::DataLow {
                    ticked: false,
                    x,
                    y,
                    index,
                    attr,
                }
            }
            PixelFetcher::DataLow { ticked, .. } if !*ticked => *ticked = true,
            PixelFetcher::DataLow {
                ticked,
                x,
                y,
                index,
                attr,
            } => {
                *self = Self::DataHigh {
                    ticked: false,
                    x: *x,
                    y: *y,
                    attr: *attr,
                    index: *index,
                    lo: mem[BgTileDataIndex(*index)][(*y % 8) as usize * 2],
                }
            }
            PixelFetcher::DataHigh { ticked, .. } if !*ticked => *ticked = true,
            PixelFetcher::DataHigh {
                ticked,
                x,
                y,
                index,
                lo,
                attr,
            } => {
                *self = Self::Sleep {
                    ticked: false,
                    x: *x,
                    y: *y,
                    attr: *attr,
                    lo: *lo,
                    hi: mem[BgTileDataIndex(*index)][2 * (*y % 8) as usize + 1],
                }
            }
            PixelFetcher::Sleep { ticked, .. } if !*ticked => *ticked = true,
            PixelFetcher::Sleep {
                ticked,
                x,
                y,
                lo,
                hi,
                attr,
            } => {
                *self = Self::Push {
                    x: *x,
                    y: *y,
                    lo: *lo,
                    hi: *hi,
                    attr: *attr,
                }
            }
            PixelFetcher::Push { x, y, lo, hi, attr } => {
                if let Some(out) = pixels_out {
                    *out = form_pixels(*attr, *lo, *hi);
                    *self = Self::GetTile {
                        ticked: false,
                        x: *x + 8,
                        y: *y,
                    };
                }
            }
        }
    }
}

fn form_pixels(attr: u8, lo: u8, hi: u8) -> InlineVec<FiFoPixel, 8> {
    zip_bits(hi, lo)
        .map(|color| FiFoPixel {
            color,
            palette: attr & 0x7,
            sprite_priority: false,
            bg_priority: false,
        })
        .collect()
}

/// Used to generate pixel color indications, which have a color depth of 2.
pub fn zip_bits(hi: u8, lo: u8) -> impl Iterator<Item = u8> {
    (0..8)
        .map(move |i| (check_bit(i, hi), check_bit(i, lo)))
        .map(|(hi, lo)| (hi as u8) << 1 | lo as u8)
}

impl Ppu {
    pub(crate) fn new() -> Self {
        Self {
            inner: PpuInner::default(),
            screen: vec![[Pixel::new(); 160]; 144],
        }
    }

    pub(crate) fn tick(&mut self, mem: &mut MemoryMap) {
        let span = info_span!("Ticking PPU:");
        let _guard = span.enter();
        self.inner.tick(&mut self.screen, mem);
        mem.inc_ppu_status(self.inner.state());
    }

    pub(crate) fn state(&self) -> PpuMode {
        self.inner.state()
    }

    /// Ticks the PPU until it finishes the current screen drawing sequence. After this, the PPU
    /// will be ready to start rendering the next screen.
    pub(crate) fn finish_screen(&mut self, mem: &mut MemoryMap) {
        let mut state = self.state();
        while {
            self.tick(mem);
            let old = std::mem::replace(&mut state, self.state());
            !matches!((old, state), (PpuMode::VBlank, PpuMode::OamScan))
        } {}
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        mem::{
            io::{BgPaletteIndex, Palette},
            vram::PpuMode,
            MemoryMap,
        },
        ppu::{Pixel, Ppu},
    };
    use heapless::Vec as InlineVec;

    use super::{zip_bits, FiFoPixel, PixelFetcher, PpuInner};

    // Test that the fetcher can get the pixel data, populate the buffer at the right time, and
    // reset itself.
    #[test]
    fn single_pass_fetcher() {
        let mem = MemoryMap::construct();
        let mut fetcher = PixelFetcher::new(0);
        let mut counter = 0;
        let mut buffer = InlineVec::new();
        // The buffer should be empty until at least the 7th tick
        for _ in 0..=7 {
            println!("{fetcher:X?}");
            fetcher.tick(&mem, Some(&mut buffer));
            assert!(buffer.is_empty())
        }
        println!("{fetcher:X?}");
        fetcher.tick(&mem, Some(&mut buffer));
        assert!(!buffer.is_empty(), "{fetcher:?}");
    }

    #[test]
    fn delayed_fetcher_push() {
        println!("{}", std::mem::size_of::<PixelFetcher>());
        println!("{}", std::mem::size_of::<FiFoPixel>());
        let mem = MemoryMap::construct();
        let mut fetcher = PixelFetcher::new(0);
        let mut counter = 0;
        let mut buffer = InlineVec::new();
        // The buffer should be empty until at least the 7th tick
        for _ in 0..=7 {
            println!("{fetcher:X?}");
            fetcher.tick(&mem, Some(&mut buffer));
            assert!(buffer.is_empty())
        }
        println!("{fetcher:X?}");
        fetcher.tick(&mem, None);
        assert!(buffer.is_empty(), "{fetcher:?}");
        println!("{fetcher:X?}");
        fetcher.tick(&mem, Some(&mut buffer));
        assert!(!buffer.is_empty(), "{fetcher:?}");
    }

    #[test]
    fn test_scan_line_timing() {
        let mut mem = MemoryMap::construct();
        let mut screen = vec![[Pixel::new(); 160]; 144];
        let mut ppu = PpuInner::default();
        let mut state = ppu.state();
        let mut counter = 0;
        while {
            counter += 1;
            ppu.tick(&mut screen, &mut mem);
            let next_state = ppu.state();
            let digest = !matches!((state, next_state), (PpuMode::HBlank, PpuMode::OamScan));
            state = next_state;
            digest
        } {}
        assert_eq!(counter, 456);
    }

    #[test]
    fn test_frame_render_timing() {
        let mut mem = MemoryMap::construct();
        let mut screen = vec![[Pixel::new(); 160]; 144];
        let mut ppu = PpuInner::default();
        let mut state = ppu.state();
        let mut counter = 0;
        while {
            counter += 1;
            ppu.tick(&mut screen, &mut mem);
            let next_state = ppu.state();
            let digest = !matches!((state, next_state), (PpuMode::VBlank, PpuMode::OamScan));
            state = next_state;
            digest
        } {}
        assert_eq!(counter, 70224);
    }

    #[test]
    fn test_bit_zipper() {
        let out = zip_bits(0, 0).collect::<Vec<_>>();
        assert_eq!(vec![0; 8], out);
        let out = zip_bits(0xFF, 0xFF).collect::<Vec<_>>();
        assert_eq!(vec![0b11; 8], out);
    }

    // This is a simple rendering test. Every tile on the screen will the same, the top line is
    // white and all other lines are black.
    #[test]
    fn basic_rendering_test() {
        let mut mem = MemoryMap::construct();
        mem.io_mut().background_palettes.data[0].colors[3].0 = [0xFF, 0xFF];
        mem[0x9000] = 0xFF;
        mem[0x9001] = 0xFF;
        let mut ppu = Ppu::new();
        ppu.finish_screen(&mut mem);
        let white = vec![Pixel::WHITE; 160];
        let black = vec![Pixel::BLACK; 160];
        for (i, line) in ppu.screen.iter().enumerate() {
            let check = if i % 8 == 0 { &white } else { &black };
            assert_eq!(check, line, "Line {i} was a mismatch!!")
        }
    }
}
