use std::{array, collections::VecDeque};

use heapless::Vec as InlineVec;
use tracing::{info_span, trace};

use crate::{
    cpu::{check_bit, check_bit_const},
    mem::{
        io::{BgPaletteIndex, ObjPaletteIndex},
        vram::{PpuMode, VRam},
        BgTileDataIndex, BgTileMapAttrIndex, BgTileMapIndex, MemoryMap, OamObjectIndex,
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
    /// Represents the LCD screen. The length of this will always be 144.
    pub screen: Vec<[Pixel; 160]>,
    /// This FIFO pulls data from the memory map to construct the objects used in a scanline. This
    /// process is controlled by the `PpuInner` state machine.
    obj_fifo: ObjectFiFo,
    /// This FIFO pulls data from the memory map to construct the background and window pixels.
    /// Like the other FIFO, this is controlled by the `PpuInner`.
    bg_fifo: BackgroundFiFo,
    /// The state machine that controls the timings of when memory is locked, data is pulled from
    /// it, and pixels are mixed and written to the screen.
    pub inner: PpuInner,
}

impl Ppu {
    pub(crate) fn new() -> Self {
        Self {
            inner: PpuInner::default(),
            screen: vec![[Pixel::new(); 160]; 144],
            obj_fifo: ObjectFiFo::new(),
            bg_fifo: BackgroundFiFo::new(),
        }
    }

    pub(crate) fn tick(&mut self, mem: &mut MemoryMap) -> bool {
        self.inner
            .tick(&mut self.screen, &mut self.obj_fifo, &mut self.bg_fifo, mem)
    }

    pub(crate) fn state(&self) -> PpuMode {
        self.inner.state()
    }

    /// Ticks the PPU until it finishes the current screen drawing sequence. After this, the PPU
    /// will be ready to start rendering the next screen.
    pub(crate) fn finish_screen(&mut self, mem: &mut MemoryMap) {
        while !self.tick(mem) {}
    }
}

#[derive(Debug, Hash)]
pub enum PpuInner {
    OamScan { dots: u8 },
    Drawing { dots: u16 },
    HBlank { dots: u16 },
    VBlank { dots: u16 },
}

impl Default for PpuInner {
    fn default() -> Self {
        Self::OamScan { dots: 0 }
    }
}

#[derive(Clone, Copy, PartialEq)]
enum StateTransition {
    Nothing,
    EnteringOam,
    EnteringDrawing,
    EnteringHBlank,
    EnteringVBlank,
    ExitingVBlank,
}

impl PpuInner {
    fn tick(
        &mut self,
        screen: &mut [[Pixel; 160]],
        obj: &mut ObjectFiFo,
        bg: &mut BackgroundFiFo,
        mem: &mut MemoryMap,
    ) -> bool {
        match self {
            PpuInner::OamScan { dots } if *dots == 79 => {
                *self = Self::Drawing { dots: 0 };
                mem.inc_ppu_status(self.state());
            }
            PpuInner::OamScan { dots } => *dots += 1,
            PpuInner::Drawing { dots, .. } if bg.x == 160 => {
                *self = Self::HBlank { dots: *dots };
                mem.inc_ppu_status(self.state());
            }
            PpuInner::Drawing { dots } => {
                *dots += 1;
                bg.tick(mem);
                let obj_pixel = obj.pop_pixel();
                if let Some(pixel) = bg.pop_pixel() {
                    screen[bg.y as usize][bg.x as usize] = pixel.mix(obj_pixel, mem);
                    bg.x += 1;
                    if bg.x == 160 {
                        *self = Self::HBlank { dots: *dots };
                        mem.inc_ppu_status(self.state());
                    }
                }
            }
            // This measures the number of ticks after mode 2 because all modes added together is
            // 456 and mode 2 is always 80 dots
            PpuInner::HBlank { dots } if *dots == 375 => {
                mem.inc_lcd_y();
                bg.next_scanline();
                if bg.y == 144 {
                    mem.request_vblank_int();
                    *self = Self::VBlank { dots: 0 };
                    mem.inc_ppu_status(self.state());
                } else {
                    *self = Self::OamScan { dots: 0 };
                    mem.inc_ppu_status(self.state());
                    obj.oam_scan(bg.y, mem);
                }
            }
            PpuInner::HBlank { dots, .. } => *dots += 1,
            PpuInner::VBlank { dots } if *dots == 4559 => {
                *self = Self::OamScan { dots: 0 };
                mem.reset_ppu_status();
                obj.oam_scan(0, mem);
                bg.reset();
                return true;
            }
            PpuInner::VBlank { dots, .. } => *dots += 1,
        }
        false
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
    fn new() -> Self {
        let pixels = std::iter::repeat(FiFoPixel::transparent())
            .take(176)
            .collect::<VecDeque<_>>();
        Self { pixels }
    }

    /// Constructs all of the pixels needed for mixing on this scanline
    fn oam_scan(&mut self, y: u8, mem: &MemoryMap) {
        if check_bit_const::<1>(mem.io().lcd_control) {
            self.pixels
                .extend(std::iter::repeat(FiFoPixel::transparent()).take(176));
            let y = y + 16;
            let buffer = self.pixels.make_contiguous();
            let range = if check_bit_const::<2>(mem.io().lcd_control) {
                16
            } else {
                8
            };
            let objects = (0..40)
                .map(|i| &mem[OamObjectIndex(i)])
                .filter(|[y_pos, ..]| *y_pos <= y && *y_pos + range > y)
                .copied()
                .map(OamObject::new)
                .take(10)
                .for_each(|obj| obj.populate_buffer(y, buffer, mem));
        }
        // Discard the front and back 8 pixels
        for _ in 0..8 {
            self.pixels.pop_front();
            self.pixels.pop_back();
        }
    }

    fn pop_pixel(&mut self) -> FiFoPixel {
        self.pixels.pop_front().unwrap_or(FiFoPixel::transparent())
    }
}

// TODO: This needs to track what model it is in (not VBLANK, though) and pass that to the PPU
#[derive(Debug, Hash)]
struct BackgroundFiFo {
    x: u8,
    y: u8,
    fetcher: PixelFetcher,
    /// Starts as `false` each frame. Once triggered, every scanline will use the window data while
    /// the x position of the pixel is large enough and the window is enabled.
    window_trigger: bool,
    background: InlineVec<FiFoPixel, 8>,
}

impl BackgroundFiFo {
    fn new() -> Self {
        Self {
            fetcher: PixelFetcher::new(),
            background: InlineVec::new(),
            window_trigger: false,
            x: 0,
            y: 0,
        }
    }

    fn next_scanline(&mut self) {
        self.y += 1;
        self.x = 0;
    }

    fn reset(&mut self) {
        self.fetcher.reset();
        self.x = 0;
        self.y = 0;
        self.window_trigger = false;
    }

    fn tick(&mut self, mem: &MemoryMap) {
        let bg = self.background.is_empty().then(|| &mut self.background);
        self.fetcher.tick(self.x, self.y, mem, bg);
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
    },
    DataLow {
        ticked: bool,
        attr: u8,
        index: u8,
    },
    DataHigh {
        ticked: bool,
        attr: u8,
        index: u8,
        lo: u8,
    },
    Sleep {
        ticked: bool,
        attr: u8,
        lo: u8,
        hi: u8,
    },
    Push {
        attr: u8,
        lo: u8,
        hi: u8,
    },
}

impl PixelFetcher {
    fn new() -> Self {
        Self::GetTile { ticked: false }
    }

    /// Sets the fetcher back to the start for the start of the next frame.
    fn reset(&mut self) {
        *self = Self::new()
    }

    fn tick(
        &mut self,
        x: u8,
        y: u8,
        mem: &MemoryMap,
        pixels_out: Option<&mut InlineVec<FiFoPixel, 8>>,
    ) {
        match self {
            PixelFetcher::GetTile { ticked, .. } if !*ticked => *ticked = true,
            PixelFetcher::GetTile { ticked } => {
                let index = mem[BgTileMapIndex { x, y }];
                let attr = mem[BgTileMapAttrIndex { x, y }];
                *self = Self::DataLow {
                    ticked: false,
                    index,
                    attr,
                }
            }
            PixelFetcher::DataLow { ticked, .. } if !*ticked => *ticked = true,
            PixelFetcher::DataLow {
                ticked,
                index,
                attr,
            } => {
                let attr = *attr;
                let index = *index;
                *self = Self::DataHigh {
                    ticked: false,
                    attr,
                    index,
                    lo: mem[BgTileDataIndex { index, attr }][(y % 8) as usize * 2],
                }
            }
            PixelFetcher::DataHigh { ticked, .. } if !*ticked => *ticked = true,
            PixelFetcher::DataHigh {
                ticked,
                index,
                lo,
                attr,
            } => {
                let attr = *attr;
                let index = *index;
                *self = Self::Sleep {
                    ticked: false,
                    attr,
                    lo: *lo,
                    hi: mem[BgTileDataIndex { index, attr }][(y % 8) as usize * 2 + 1],
                }
            }
            PixelFetcher::Sleep { ticked, .. } if !*ticked => *ticked = true,
            PixelFetcher::Sleep {
                ticked,
                lo,
                hi,
                attr,
            } => {
                *self = Self::Push {
                    lo: *lo,
                    hi: *hi,
                    attr: *attr,
                }
            }
            PixelFetcher::Push { lo, hi, attr } => {
                if let Some(out) = pixels_out {
                    *out = form_pixels(*attr, *lo, *hi);
                    *self = Self::GetTile { ticked: false };
                }
            }
        }
    }
}
#[derive(Debug, Hash)]
pub struct OamObject {
    y: u8,
    x: u8,
    tile_index: u8,
    attrs: u8,
}

impl OamObject {
    fn new([y, x, tile_index, attrs]: [u8; 4]) -> Self {
        Self {
            y,
            x,
            tile_index,
            attrs,
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
        // println!("Obj at ({}, {})", self.x, self.y);
        debug_assert!(y >= self.y);
        let obj = &mem[ObjTileDataIndex(self.tile_index, check_bit_const::<3>(self.attrs))];
        assert!([16, 32].contains(&obj.len()), "obj.len () = {}", obj.len());
        debug_assert!(
            (y as usize) < ((self.y as usize) + obj.len()),
            "{y}, {}",
            self.y
        );
        let mut index = y - self.y;
        if check_bit_const::<6>(self.attrs) {
            // If vertically flipped, we need to reverse the order of the bytes in the obj. Or, we
            // need to invert our indices. We know that `index` is between 0 and 8 (or 16), so we
            // can just mask out the upper half of the inverted index to bring it back into range.
            let mask = if obj.len() == 32 { 0xF } else { 0x7 };
            index = (!index) & mask;
        }
        let index = index as usize;
        let lo = obj[2 * index];
        let hi = obj[2 * index + 1];
        let mut digest = form_pixels(self.attrs, lo, hi);
        if !check_bit_const::<5>(self.attrs) {
            digest.reverse();
        }
        digest.into_array().unwrap()
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
        let mut fetcher = PixelFetcher::new();
        let mut counter = 0;
        let mut buffer = InlineVec::new();
        // The buffer should be empty until at least the 7th tick
        for _ in 0..=7 {
            println!("{fetcher:X?}");
            fetcher.tick(0, 0, &mem, Some(&mut buffer));
            assert!(buffer.is_empty())
        }
        println!("{fetcher:X?}");
        fetcher.tick(0, 0, &mem, Some(&mut buffer));
        assert!(!buffer.is_empty(), "{fetcher:?}");
    }

    #[test]
    fn delayed_fetcher_push() {
        println!("{}", std::mem::size_of::<PixelFetcher>());
        println!("{}", std::mem::size_of::<FiFoPixel>());
        let mem = MemoryMap::construct();
        let mut fetcher = PixelFetcher::new();
        let mut counter = 0;
        let mut buffer = InlineVec::new();
        // The buffer should be empty until at least the 7th tick
        for _ in 0..=7 {
            println!("{fetcher:X?}");
            fetcher.tick(0, 0, &mem, Some(&mut buffer));
            assert!(buffer.is_empty())
        }
        println!("{fetcher:X?}");
        fetcher.tick(0, 0, &mem, None);
        assert!(buffer.is_empty(), "{fetcher:?}");
        println!("{fetcher:X?}");
        fetcher.tick(0, 0, &mem, Some(&mut buffer));
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
            // ppu.tick(&mut screen, &mut mem);
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
            // ppu.tick(&mut screen, &mut mem);
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
