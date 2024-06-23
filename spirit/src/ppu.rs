use heapless::Vec as InlineVec;

use crate::mem::{
    vram::{PpuMode, VRam},
    MemoryMap,
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
    pub h_count: u16, // Max of 456
    pub v_count: u8,  // Max of 153
    // This represents the LCD screen. This will always have a length of 144.
    // Pixels are pushed from the
    screen: Vec<[Pixel; 160]>,
    fifo: PixelFiFo,
}

// TODO: This needs to track what model it is in (not VBLANK, though) and pass that to the PPU
#[derive(Debug, Hash)]
struct PixelFiFo {
    fetcher: PixelFetcher,
    // This field is ignored until object pixels are impl-ed
    // oam: InlineVec<Pixel, 8>,
    background: InlineVec<FiFoPixel, 8>,
}

impl PixelFiFo {
    // TODO: Wrap the pixel into a new enum that also carries the PPU mode
    fn tick(&mut self) -> (PpuMode, Option<FiFoPixel>) {
        self.fetcher.tick();
        let pixel = self.pop_pixel();
        (todo!(), pixel)
    }

    fn pop_pixel(&mut self) -> Option<FiFoPixel> {
        todo!()
    }

    fn push_pixels(&mut self, pixels: &[FiFoPixel; 8]) -> bool {
        todo!()
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

    fn to_pixel(self) -> Pixel {
        todo!()
    }
}

/// The final pixel that is available to the end consumer.
#[derive(Debug, Hash)]
pub struct Pixel {
    r: u8,
    g: u8,
    b: u8,
}

// TODO: There are 5 known states this can be in:
//  - Get tile,
//  - Get tile data low
//  - Get title data high
//  - Sleep
//  - Push
// I think it would make more sense to have this type be an enum that cycles between these states.
// Or, at least, the fetcher should use it internally (like for tracking where to index tile data).
/// Pulls data out of VRAM and OAM
#[derive(Debug, Hash)]
struct PixelFetcher {
    x_coord: u8,
    index: u16,
    state: PixelFetchInner,
}

impl PixelFetcher {
    fn tick(&mut self, mem: &MemoryMap) {
        // TODO: Needs access to memory map and FIFO
        // TODO: Inc the counter, pull data, and attempt to push pixels into the FIFO
        todo!()
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
#[derive(Debug, Hash)]
enum PixelFetchInner {
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
        pixels: [FiFoPixel; 8],
    },
}

impl PixelFetchInner {
    fn tick(&mut self, mem: &MemoryMap) {
        match self {
            PixelFetchInner::GetTile { ticked, .. } if !*ticked => *ticked = true,
            PixelFetchInner::GetTile { ticked, x, y } => {
                let addr = *y as usize * 32 + *x as usize;
                // VRAM starts at 0x8000, and we want to access 0x9800..
                // TODO: This needs to access VRAM bank 1, not 0
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
            PixelFetchInner::DataLow { ticked, .. } if !*ticked => *ticked = true,
            PixelFetchInner::DataLow {
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
            PixelFetchInner::DataHigh { ticked, .. } if !*ticked => *ticked = true,
            PixelFetchInner::DataHigh {
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
            PixelFetchInner::Sleep { ticked, .. } if !*ticked => *ticked = true,
            PixelFetchInner::Sleep {
                ticked,
                x,
                y,
                lo,
                hi,
                attr,
            } => {
                todo!("Generate pixels");
                *self = Self::Push {
                    x: *x,
                    y: *y,
                    pixels: form_pixels(*attr, *lo, *hi, mem),
                }
            }
            PixelFetchInner::Push { x, y, pixels } => {
                // TODO: Attempt to push the pixels somewhere, and, on success, reset
                todo!();
                if todo!() {
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

fn form_pixels(attr: u8, lo: u8, hi: u8, mem: &MemoryMap) -> [FiFoPixel; 8] {
    let palette = mem.io.background_palettes.get_palette(attr & 0x7);
    todo!()
}

impl Ppu {
    pub(crate) fn new() -> Self {
        todo!()
    }

    pub(crate) fn tick(&mut self, mem: &mut MemoryMap) {
        let (state, pixel) = self.fifo.tick();
        if let Some(pixel) = pixel {
            // self.screen[todo!()][self.v_count] = pixel;
            todo!()
        }
        mem.vram.inc_status(state);
        self.h_count += 1;
        if self.h_count == 456 {
            self.h_count = 0;
            self.v_count += 1;
            if self.v_count == 144 {
                mem.vram.inc_status(PpuMode::VBlank);
                mem.request_vblank_int();
            } else if self.v_count == 153 {
                self.v_count = 0;
                mem.vram.reset_status();
            }
        }
    }
}
