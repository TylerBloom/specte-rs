use std::array;
use std::collections::VecDeque;
use std::fmt::Display;
use std::sync::LazyLock;
use std::sync::Mutex;

use heapless::Vec as InlineVec;
use serde::Deserialize;
use serde::Serialize;
use tracing::info_span;
use tracing::trace;

use crate::cpu::check_bit;
use crate::cpu::check_bit_const;
use crate::mem::BgTileDataIndex;
use crate::mem::BgTileMapAttrIndex;
use crate::mem::BgTileMapIndex;
use crate::mem::MemoryMap;
use crate::mem::OamObjectIndex;
use crate::mem::ObjTileDataIndex;
use crate::mem::WindowTileDataIndex;
use crate::mem::WindowTileMapAttrIndex;
use crate::mem::WindowTileMapIndex;
use crate::mem::io::BgPaletteIndex;
use crate::mem::io::ObjPaletteIndex;
use crate::mem::vram::PpuMode;
use crate::mem::vram::VRam;

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
#[derive(Debug, Hash, Serialize, Deserialize)]
pub struct Ppu {
    /// Represents the LCD screen. The length of this will always be 144.
    pub screen: Vec<Vec<Pixel>>,
    /// This FIFO pulls data from the memory map to construct the objects used in a scanline. This
    /// process is controlled by the `PpuInner` state machine.
    obj_fifo: ObjectFiFo,
    /// This FIFO pulls data from the memory map to construct the background and window pixels.
    /// Like the other FIFO, this is controlled by the `PpuInner`.
    bg_fifo: BackgroundFiFo,
    /// The state machine that controls the timings of when memory is locked, data is pulled from
    /// it, and pixels are mixed and written to the screen.
    pub inner: PpuInner,
    /// Signals if the PPU has been ticked sinced created. This is used to track if the PPU needs
    /// to be reset if the LCD is turned off.
    ticked: bool,
}

impl Ppu {
    pub(crate) fn new() -> Self {
        Self {
            inner: PpuInner::default(),
            screen: vec![vec![Pixel::new(); 160]; 144],
            obj_fifo: ObjectFiFo::new(),
            bg_fifo: BackgroundFiFo::new(),
            ticked: false,
        }
    }

    pub(crate) fn tick(&mut self, mem: &mut MemoryMap) -> bool {
        if check_bit_const::<7>(mem.io().lcd_control) {
            self.ticked = true;
            self.inner
                .tick(&mut self.screen, &mut self.obj_fifo, &mut self.bg_fifo, mem)
        } else {
            if self.ticked {
                mem.io_mut().lcd_y = 0;
                *self = Self::new();
            }
            true
        }
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

#[derive(Debug, Hash, Serialize, Deserialize)]
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
        screen: &mut [Vec<Pixel>],
        obj: &mut ObjectFiFo,
        bg: &mut BackgroundFiFo,
        mem: &mut MemoryMap,
    ) -> bool {
        match self {
            PpuInner::OamScan { dots } if *dots == 79 => {
                *self = Self::Drawing { dots: 0 };
                obj.oam_scan(bg.y, mem);
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
                if let Some(pixel) = bg.pop_pixel() {
                    let obj_pixel = obj.pop_pixel();
                    screen[bg.y as usize][bg.x as usize] = pixel.mix(obj_pixel, mem);
                    bg.x += 1;
                    if bg.x == 160 {
                        *self = Self::HBlank { dots: *dots };
                        mem.inc_ppu_status(self.state());
                        bg.fetcher.reset();
                    }
                }
            }
            // This measures the number of ticks after mode 2 because all modes added together is
            // 456 and mode 2 is always 80 dots
            PpuInner::HBlank { dots } if *dots == 375 => {
                bg.next_scanline();
                mem.inc_lcd_y();
                assert_eq!(bg.y, mem.io().lcd_y);
                if bg.y == 144 {
                    mem.request_vblank_int();
                    *self = Self::VBlank { dots: 0 };
                    mem.inc_ppu_status(self.state());
                } else {
                    *self = Self::OamScan { dots: 0 };
                    mem.inc_ppu_status(self.state());
                }
            }
            PpuInner::HBlank { dots, .. } => *dots += 1,
            PpuInner::VBlank { dots } if *dots == 4559 => {
                *self = Self::OamScan { dots: 0 };
                mem.reset_ppu_status();
                // obj.oam_scan(0, mem);
                bg.reset();
                mem.io_mut().lcd_y = 0;
                return true;
            }
            PpuInner::VBlank { dots, .. } => {
                *dots += 1;
                if *dots % 456 == 0 {
                    mem.inc_lcd_y();
                }
            }
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

#[derive(Debug, Hash, Serialize, Deserialize)]
pub struct ObjectFiFo {
    pixels: VecDeque<ObjectPixel>,
}

impl ObjectFiFo {
    fn new() -> Self {
        let pixels = std::iter::repeat(ObjectPixel::transparent())
            .take(176)
            .collect::<VecDeque<_>>();
        Self { pixels }
    }

    pub fn scan_objs(y: u8, mem: &MemoryMap) -> impl Iterator<Item = OamObject> {
        let y = y + 16;
        let range = if check_bit_const::<2>(mem.io().lcd_control) {
            16
        } else {
            8
        };
        let digest = (0..40)
            // std::iter::once(0)
            // .skip(1)
            // .step_by(2)
            .map(|i| &mem[OamObjectIndex(i)])
            .filter(|[y_pos, ..]| *y_pos <= y && *y_pos + range > y)
            .copied()
            .map(OamObject::new)
            .take(10)
            // TODO: This is a better way to do this. We should collect into a heapless::Vec
            // and just call `.rev()` on the iter. Unforetunely, the heapless vec iter doesn't
            // impl this :"(
            .collect::<Vec<_>>();

        digest
            .into_iter()
            // We reverse here because the top objects have priority over the lower ones should
            // they overlap.
            .rev()
    }

    /// Constructs all of the pixels needed for mixing on this scanline
    fn oam_scan(&mut self, y: u8, mem: &MemoryMap) {
        self.pixels.clear();
        self.pixels
            .extend(std::iter::repeat(ObjectPixel::transparent()).take(176));
        if check_bit_const::<1>(mem.io().lcd_control) {
            let buffer = self.pixels.make_contiguous();
            Self::scan_objs(y, mem).for_each(|obj| obj.populate_buffer(y + 16, buffer, mem));
        }
        // Discard the front and back 8 pixels
        for _ in 0..8 {
            self.pixels.pop_front();
            self.pixels.pop_back();
        }
    }

    fn pop_pixel(&mut self) -> ObjectPixel {
        self.pixels
            .pop_front()
            .unwrap_or(ObjectPixel::transparent())
    }
}

#[derive(Debug, Hash, Serialize, Deserialize)]
struct BackgroundFiFo {
    x: u8,
    y: u8,
    fetcher: PixelFetcher,
    /// Starts as `false` each frame. Once triggered, every scanline will use the window data while
    /// the x position of the pixel is large enough and the window is enabled.
    background: InlineVec<BgPixel, 8>,
    window: Window,
}

#[derive(Debug, Default, Hash, Serialize, Deserialize, Clone, Copy)]
struct Window {
    /// Tracks if the window was used in the last scanline (the window could be triggered by
    /// disabled).
    was_used: bool,
    /// Tracks if the top left corner of the window has been hit this frame.
    triggered: bool,
    /// Counts the number of vertical lines that have been rendered.
    line_count: u8,
}

impl BackgroundFiFo {
    fn new() -> Self {
        Self {
            fetcher: PixelFetcher::new(),
            background: InlineVec::new(),
            window: Window::default(),
            x: 0,
            y: 0,
        }
    }

    fn next_scanline(&mut self) {
        self.y += 1;
        self.x = 0;
        self.window.line_count += self.window.was_used as u8;
        self.window.was_used = false;
        // Not needed because it should be ready for the next line.
        // self.fetcher.next_scanline();
    }

    fn reset(&mut self) {
        self.fetcher.reset();
        self.x = 0;
        self.y = 0;
        self.window = Window::default();
    }

    fn tick(&mut self, mem: &MemoryMap) {
        let bg = self.background.is_empty().then(|| &mut self.background);
        self.window.triggered =
            self.y >= mem.io().window_position[0] && check_bit_const::<5>(mem.io().lcd_control);
        let do_window =
            self.window.triggered && self.x >= (mem.io().window_position[1] & !0x7).wrapping_sub(7);
        self.window.was_used |= do_window;
        self.fetcher
            .tick(self.y, mem, bg, do_window.then_some(self.window));
    }

    fn pop_pixel(&mut self) -> Option<BgPixel> {
        self.background.pop()
    }
}

#[derive(Debug, Default, Hash, Clone, Copy, Serialize, Deserialize)]
pub struct BgPixel {
    /// Which color for the selected palette should be used. Ranges from 0 to 3.
    color: u8,
    /// Color palette to use. Ranges from 0 to 7.
    palette: u8,
    priority: bool,
}

#[derive(Debug, Default, Hash, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub struct ObjectPixel {
    /// Which color for the selected palette should be used. Ranges from 0 to 3.
    color: u8,
    /// Color palette to use. Ranges from 0 to 7.
    palette: u8,
    priority: bool,
}

impl BgPixel {
    /// This is the same as `Self::default`, but with a clearer name
    #[inline]
    const fn transparent() -> Self {
        Self {
            color: 0,
            palette: 0,
            priority: false,
        }
    }

    fn mix(self, obj: ObjectPixel, mem: &MemoryMap) -> Pixel {
        if obj.color == 0 {
            return mem[BgPaletteIndex(self.palette)]
                .get_color(self.color)
                .into();
        }
        if !check_bit_const::<0>(mem.io().lcd_control) {
            mem[ObjPaletteIndex(obj.palette)].get_color(obj.color)
        } else {
            if self.priority || obj.priority {
                if self.color > 0 && self.color < 4 {
                    mem[BgPaletteIndex(self.palette)].get_color(self.color)
                } else {
                    mem[ObjPaletteIndex(obj.palette)].get_color(obj.color)
                }
            } else {
                mem[ObjPaletteIndex(obj.palette)].get_color(obj.color)
            }
        }
        .into()
    }
}

impl ObjectPixel {
    /// This is the same as `Self::default`, but with a clearer name
    #[inline]
    const fn transparent() -> Self {
        Self {
            color: 0,
            palette: 0,
            priority: true,
        }
    }

    pub fn as_pixel(self, mem: &MemoryMap) -> Pixel {
        mem[ObjPaletteIndex(self.palette)]
            .get_color(self.color)
            .into()
    }
}

/// The final pixel that is available to the end consumer.
#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct Pixel {
    pub r: u8,
    pub g: u8,
    pub b: u8,
}

impl Display for Pixel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {}, {})", self.r, self.b, self.g)
    }
}

impl Pixel {
    pub const WHITE: Self = Self {
        r: 0x1F,
        g: 0x1F,
        b: 0x1F,
    };

    pub const BLACK: Self = Self { r: 0, g: 0, b: 0 };

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
#[derive(Debug, Hash, Clone, Copy, Serialize, Deserialize)]
enum PixelFetcher {
    GetTile {
        ticked: bool,
        x: u8,
    },
    DataLow {
        ticked: bool,
        attr: u8,
        index: u8,
        x: u8,
    },
    DataHigh {
        ticked: bool,
        x: u8,
        attr: u8,
        index: u8,
        lo: u8,
    },
    Sleep {
        ticked: bool,
        x: u8,
        attr: u8,
        lo: u8,
        hi: u8,
    },
    Push {
        attr: u8,
        x: u8,
        lo: u8,
        hi: u8,
    },
}

impl PixelFetcher {
    fn new() -> Self {
        Self::GetTile {
            ticked: false,
            x: 0,
        }
    }

    /// Sets the fetcher back to the start for the start of the next frame.
    fn reset(&mut self) {
        *self = Self::new()
    }

    fn tick(
        &mut self,
        y: u8,
        mem: &MemoryMap,
        pixels_out: Option<&mut InlineVec<BgPixel, 8>>,
        window: Option<Window>,
    ) {
        match self {
            PixelFetcher::GetTile { ticked, .. } if !*ticked => *ticked = true,
            &mut PixelFetcher::GetTile { x, .. } => {
                let (index, attr) = if let Some(window) = window {
                    (
                        mem[WindowTileMapIndex {
                            x,
                            y: window.line_count,
                        }],
                        mem[WindowTileMapAttrIndex {
                            x,
                            y: window.line_count,
                        }],
                    )
                } else {
                    (
                        mem[BgTileMapIndex { x, y }],
                        mem[BgTileMapAttrIndex { x, y }],
                    )
                };
                *self = Self::DataLow {
                    ticked: false,
                    index,
                    attr,
                    x,
                }
            }
            PixelFetcher::DataLow { ticked, .. } if !*ticked => *ticked = true,
            &mut PixelFetcher::DataLow { index, attr, x, .. } => {
                let mut y = if let Some(window) = window {
                    window.line_count
                } else {
                    y
                };
                y %= 8;
                if check_bit_const::<6>(attr) {
                    y = 7 - y;
                }
                let y = y as usize * 2;
                let lo = if window.is_some() {
                    mem[WindowTileDataIndex { index, attr }][y]
                } else {
                    mem[BgTileDataIndex { index, attr }][y]
                };
                *self = Self::DataHigh {
                    ticked: false,
                    attr,
                    index,
                    x,
                    lo,
                }
            }
            PixelFetcher::DataHigh { ticked, .. } if !*ticked => *ticked = true,
            &mut PixelFetcher::DataHigh {
                ticked,
                index,
                x,
                lo,
                attr,
            } => {
                let mut y = if let Some(window) = window {
                    window.line_count
                } else {
                    y
                };
                y %= 8;
                if check_bit_const::<6>(attr) {
                    y = 7 - y;
                }
                let y = y as usize * 2;
                let hi = if window.is_some() {
                    mem[WindowTileDataIndex { index, attr }][y + 1]
                } else {
                    mem[BgTileDataIndex { index, attr }][y + 1]
                };
                *self = Self::Sleep {
                    ticked: false,
                    attr,
                    x,
                    lo,
                    hi,
                }
            }
            PixelFetcher::Sleep { ticked, .. } if !*ticked => *ticked = true,
            &mut PixelFetcher::Sleep {
                lo, x, hi, attr, ..
            } => *self = Self::Push { x, lo, hi, attr },
            &mut PixelFetcher::Push { lo, hi, attr, x } => {
                if let Some(out) = pixels_out {
                    let mut new_out = zip_bits(hi, lo)
                        .map(|color| BgPixel {
                            color,
                            palette: attr & 0x7,
                            priority: check_bit_const::<7>(attr),
                        })
                        .collect::<InlineVec<_, 8>>();
                    if check_bit_const::<5>(attr) {
                        new_out.reverse();
                    }
                    *out = new_out;
                    *self = Self::GetTile {
                        ticked: false,
                        x: x + 8,
                    };
                }
            }
        }
    }
}
#[derive(Debug, Hash, Clone, Copy)]
pub struct OamObject {
    pub y: u8,
    x: u8,
    tile_index: u8,
    attrs: u8,
}

impl OamObject {
    pub fn new([y, x, tile_index, attrs]: [u8; 4]) -> Self {
        Self {
            y,
            x,
            tile_index,
            attrs,
        }
    }

    fn populate_buffer(self, y: u8, buffer: &mut [ObjectPixel], mem: &MemoryMap) {
        if self.x > 168 || y >= 160 {
            return;
        }
        let x = self.x as usize;
        self.generate_pixels(y, mem)
            .into_iter()
            .enumerate()
            .filter(|(i, pixel)| pixel != &ObjectPixel::transparent())
            .for_each(|(i, pixel)| buffer[x + i] = pixel);
    }

    #[track_caller]
    pub fn generate_pixels(self, y: u8, mem: &MemoryMap) -> [ObjectPixel; 8] {
        // The given Y needs to be within the bounds of the object. This should be the case
        // already.
        // NOTE: This is not entirely true as
        debug_assert!(y >= self.y, "{y} !>= {}", self.y);
        let obj = &mem[ObjTileDataIndex(self.tile_index, check_bit_const::<3>(self.attrs))];
        debug_assert!([16, 32].contains(&obj.len()), "obj.len () = {}", obj.len());
        debug_assert!(
            (y as usize) < ((self.y as usize) + obj.len()),
            "{y}, {}",
            self.y
        );
        let mut index = y - self.y;
        if check_bit_const::<6>(self.attrs) {
            index = if obj.len() == 32 {
                15 - index
            } else {
                7 - index
            };
        }
        let index = index as usize;
        let lo = obj[2 * index];
        let hi = obj[2 * index + 1];
        let mut digest = zip_bits(hi, lo)
            .map(|color| ObjectPixel {
                color,
                palette: self.attrs & 0x7,
                priority: check_bit_const::<7>(self.attrs),
            })
            .collect::<InlineVec<_, 8>>();
        if !check_bit_const::<5>(self.attrs) {
            digest.reverse();
        }
        digest.into_array().unwrap()
    }
}

/// Used to generate pixel color indications, which have a color depth of 2.
pub fn zip_bits(hi: u8, lo: u8) -> impl Iterator<Item = u8> {
    (0..8)
        .map(move |i| (check_bit(i, hi), check_bit(i, lo)))
        .map(|(hi, lo)| (hi as u8) << 1 | lo as u8)
}

#[cfg(test)]
mod tests {
    use crate::mem::MemoryLike;
    use crate::mem::MemoryMap;
    use crate::mem::io::BgPaletteIndex;
    use crate::mem::io::Palette;
    use crate::mem::vram::PpuMode;
    use crate::ppu::ObjectPixel;
    use crate::ppu::Pixel;
    use crate::ppu::Ppu;
    use heapless::Vec as InlineVec;

    use super::PixelFetcher;
    use super::PpuInner;
    use super::zip_bits;

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
            fetcher.tick(0, &mem, Some(&mut buffer), None);
            assert!(buffer.is_empty())
        }
        println!("{fetcher:X?}");
        fetcher.tick(0, &mem, Some(&mut buffer), None);
        assert!(!buffer.is_empty(), "{fetcher:?}");
    }

    #[test]
    fn delayed_fetcher_push() {
        println!("{}", std::mem::size_of::<PixelFetcher>());
        println!("{}", std::mem::size_of::<ObjectPixel>());
        let mem = MemoryMap::construct();
        let mut fetcher = PixelFetcher::new();
        let mut counter = 0;
        let mut buffer = InlineVec::new();
        // The buffer should be empty until at least the 7th tick
        for _ in 0..=7 {
            println!("{fetcher:X?}");
            fetcher.tick(0, &mem, Some(&mut buffer), None);
            assert!(buffer.is_empty())
        }
        println!("{fetcher:X?}");
        fetcher.tick(0, &mem, None, None);
        assert!(buffer.is_empty(), "{fetcher:?}");
        println!("{fetcher:X?}");
        fetcher.tick(0, &mem, Some(&mut buffer), None);
        assert!(!buffer.is_empty(), "{fetcher:?}");
    }

    #[test]
    fn test_scan_line_timing() {
        let mut mem = MemoryMap::construct();
        // Turns on PPU
        mem.write_byte(0xFF40, 0x80);
        let mut ppu = Ppu::new();
        let mut state = ppu.state();
        let mut counter = 0;
        while {
            counter += 1;
            ppu.tick(&mut mem);
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
        // Turns on PPU
        mem.write_byte(0xFF40, 0x80);
        let mut ppu = Ppu::new();
        let mut state = ppu.state();
        let mut counter = 0;
        while {
            counter += 1;
            ppu.tick(&mut mem);
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
        // Turns on PPU
        mem.write_byte(0xFF40, 0x80);
        mem.write_byte(0x9000, 0xFF);
        mem.write_byte(0x9001, 0xFF);
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
