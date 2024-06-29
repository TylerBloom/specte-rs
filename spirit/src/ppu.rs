use std::array;

use heapless::Vec as InlineVec;

use crate::{
    cpu::{check_bit, check_bit_const},
    mem::{
        vram::{PpuMode, VRam},
        MemoryMap,
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
pub enum PpuInner {
    OamScan {
        dots: u8,
        y: u8,
    },
    Drawing {
        fifo: PixelFiFo,
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
                    fifo: PixelFiFo::new(),
                    y: *y,
                    dots: 0,
                    x: 0,
                };
            }
            PpuInner::OamScan { dots, y } => *dots += 1,
            PpuInner::Drawing { dots, x, y, .. } if *x == 160 => {
                *self = Self::HBlank { dots: *dots, y: *y };
            }
            PpuInner::Drawing { fifo, dots, x, y } => {
                *dots += 1;
                fifo.tick(mem);
                if let Some(pixel) = fifo.pop_pixel() {
                    screen[*y as usize][*x as usize] = pixel.to_pixel(mem);
                    *x += 1;
                    if *x == 160 {
                        *self = Self::HBlank { dots: *dots, y: *y };
                    }
                }
            }
            // This measures the number of ticks after mode 2 becasuse all modes added together is
            // 456 and mode 2 is always 80 dots
            PpuInner::HBlank { dots, y } if *dots == 375 => {
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
struct PixelFiFo {
    counter: u8,
    tile_count: u8,
    fetcher: PixelFetcher,
    // This field is ignored until object pixels are impl-ed
    // oam: InlineVec<Pixel, 8>,
    background: InlineVec<FiFoPixel, 8>,
}

impl PixelFiFo {
    fn new() -> Self {
        Self {
            counter: 0,
            tile_count: 0,
            fetcher: PixelFetcher::default(),
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

#[derive(Debug, Hash)]
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

    fn to_pixel(self, mem: &MemoryMap) -> Pixel {
        let [a, b] = mem
            .io
            .background_palettes
            .get_palette(self.palette)
            .get_color(self.color)
            .0;
        let r = a & 0b0001_1111;
        let g = ((a & 0b1110_0000) >> 5) | ((b & 0b0000_0011) << 3);
        let b = (b & 0b0111_1100) >> 2;
        Pixel { r, g, b }
    }
}

/// The final pixel that is available to the end consumer.
#[derive(Debug, Hash, Clone, Copy)]
pub struct Pixel {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Pixel {
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

impl Default for PixelFetcher {
    fn default() -> Self {
        Self::GetTile {
            ticked: false,
            x: 0,
            y: 0,
        }
    }
}

impl PixelFetcher {
    fn tick(&mut self, mem: &MemoryMap, pixels_out: Option<&mut InlineVec<FiFoPixel, 8>>) {
        match self {
            PixelFetcher::GetTile { ticked, .. } if !*ticked => *ticked = true,
            PixelFetcher::GetTile { ticked, x, y } => {
                let addr = *y as usize * 32 + *x as usize;
                // VRAM starts at 0x8000, and we want to access 0x9800..
                let index = mem.vram.vram.0[0x1800 + addr];
                let attr = mem.vram.vram.1[0x1800 + addr];
                *self = Self::DataLow {
                    ticked: false,
                    x: *x,
                    y: *y,
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
                    // TODO: This ignores the variable indexing method used for tile data.
                    lo: mem.vram.vram.0[*index as usize],
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
                    // TODO: This ignores the variable indexing method used for tile data.
                    hi: mem.vram.vram.0[*index as usize + 1],
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
            // TODO: Attempt to push the pixels somewhere, and, on success, reset
            PixelFetcher::Push { x, y, lo, hi, attr } => {
                if let Some(out) = pixels_out {
                    *out = form_pixels(*attr, *lo, *hi);
                    // Once the pixels are pushed, the coordinates need to be updated. The tile
                    // maps are 32x32 indices, so we need to reset x (and maybe y).
                    // Note that % 32 is the same as AND-ing with 0x0F, which might be faster.
                    *x += 1;
                    let x = *x % 32;
                    let y = (*y + ((x & 0x10) >> 4)) % 32;
                    *self = Self::GetTile {
                        ticked: false,
                        x,
                        y,
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
fn zip_bits(hi: u8, lo: u8) -> impl Iterator<Item = u8> {
    (0..7)
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
        self.inner.tick(&mut self.screen, mem);
        mem.vram.inc_status(self.inner.state());
    }

    pub(crate) fn state(&self) -> PpuMode {
        self.inner.state()
    }
}

#[cfg(test)]
mod tests {
    use crate::{mem::{vram::PpuMode, MemoryMap}, ppu::Pixel};
    use heapless::Vec as InlineVec;

    use super::{FiFoPixel, PixelFetcher, PpuInner};

    // Test that the fetcher can get the pixel data, populate the buffer at the right time, and
    // reset itself.
    #[test]
    fn single_pass_fetcher() {
        let mem = MemoryMap::construct();
        let mut fetcher = PixelFetcher::default();
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
        let mut fetcher = PixelFetcher::default();
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
}
