use heapless::Vec as InlineVec;

use crate::mem::{
    vram::{PpuMode, VRam},
    MemoryMap,
};

// Plan of attack:
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
    oam: InlineVec<Pixel, 8>,
    background: InlineVec<Pixel, 8>,
}

impl PixelFiFo {
    // TODO: Wrap the pixel into a new enum that also carries the PPU mode
    fn tick(&mut self) -> (PpuMode, Option<Pixel>) {
        self.fetcher.tick();
        let pixel = self.pop_pixel();
        (todo!(), pixel)
    }

    fn pop_pixel(&mut self) -> Option<Pixel> {
        todo!()
    }

    fn push_pixels(&mut self, pixels: &[Pixel; 8]) -> bool {
        todo!()
    }
}

#[derive(Debug, Hash)]
pub struct Pixel {}

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

#[derive(Debug, Hash)]
enum PixelFetchInner {
    GetTile,
    DataLow,
    DataHigh,
    Sleep,
    Push,
}

impl PixelFetchInner {
    fn tick(&mut self) {
    }

    fn reset(&mut self) {
        *self = Self::GetTile;
    }
}

impl PixelFetcher {
    fn tick(&mut self) {
        // TODO: Needs access to memory map and FIFO
        // TODO: Inc the counter, pull data, and attempt to push pixels into the FIFO
        todo!()
    }
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
