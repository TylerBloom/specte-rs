use std::ops::{Index, IndexMut};

use serde::{Deserialize, Serialize};
use tracing::trace;

use crate::{cpu::check_bit_const, lookup::InterruptOp, ppu::Pixel, utils::Wrapping, ButtonInput};

use super::{
    vram::{PpuMode, VRam},
    MemoryMap,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct IoRegisters {
    /// ADDR FF00
    pub(super) joypad: Joypad,
    /// ADDR FF01, FF02
    serial: (u8, u8),
    /// ADDR FF04, FF05, FF06, FF07
    /// There are the (divider, timer, timer modulo, tac)
    timer_div: [u8; 4],
    /// The counter for the timer controller
    tac: TimerControl,
    /// ADDR FF0F
    pub interrupt_flags: u8,
    /// ADDR FF10-FF26
    audio: [u8; 0x17],
    /// ADDR FF30-FF3F
    wave: [u8; 0x10],
    /// ADDR FF40
    pub lcd_control: u8,
    /// ADDR FF41
    lcd_status: u8,
    /// A duplicate value of the LCD status register. A reference to this byte is given out when
    /// mutably indexing as some of the bytes are read-only
    lcd_status_dup: u8,
    /// ADDR FF42 & FF43
    pub(crate) bg_position: (u8, u8),
    /// ADDR FF44 (set by the PPU)
    pub(crate) lcd_y: u8,
    /// ADDR FF45
    pub(crate) lcd_cmp: u8,
    /// ADDR FF47
    monochrome_bg_palette: u8,
    /// ADDR FF48 & FF49
    monochrome_obj_palettes: [u8; 2],
    /// ADDR FF4A & FF4B
    pub(crate) window_position: [u8; 2],
    /// ADDR FF4F
    // TODO: Only the 0th bit is used. From the docs:
    // """
    // Reading from this register will return the number of the currently loaded VRAM bank in bit
    // 0, and all other bits will be set to 1.
    // """
    pub(super) vram_select: u8,
    /// ADDR FF50
    boot_status: u8,
    /// ADDR FF68 and FF69
    pub background_palettes: ColorPalettes,
    /// ADDR FF6A and FF6B
    pub(crate) object_palettes: ColorPalettes,
    /// ADDR FF70
    wram_select: u8,
    /// There are gaps amount the memory mapped IO registers. Any index into this that hits one of
    /// these gaps resets the value. Notably, this is also used when mutably indexing to the
    /// divider register but not while immutably indexing.
    /// ADDR FF03, FF08-FF0E, FF27-FF29, FF4C-FF4E, FF56-FF67, FF6C-FF6F
    dead_byte: u8,
}

/// This simple wrapper type is used to index into the IO registers by the PPU. Because the PPU
/// need access to all of the palette data at once, it needs its own method of indexing.
pub(crate) struct BgPaletteIndex(pub u8);

impl Index<BgPaletteIndex> for MemoryMap {
    type Output = Palette;

    fn index(&self, index: BgPaletteIndex) -> &Self::Output {
        &self.io[index]
    }
}

/// This simple wrapper type is used to index into the IO registers by the PPU. Because the PPU
/// need access to all of the palette data at once, it needs its own method of indexing.
pub(crate) struct ObjPaletteIndex(pub u8);

impl Index<ObjPaletteIndex> for MemoryMap {
    type Output = Palette;

    fn index(&self, index: ObjPaletteIndex) -> &Self::Output {
        &self.io[index]
    }
}

// FF40 -> LCD control register
// FF41 -> LCD status register
// FF42 & FF43 -> Background viewport position (SCY, SCX)
// FF44 -> LCD Y coordinate (read only) (this is held in the PPU)
// FF45 -> LY compare (controls the STAT iterrupt) (LCD Y is in the PPU, so this should be put
// there too)
// FF46 -> OAM DMA source address and start
// FF47 -> Monochrome BG palette data
// FF48 & FF49 -> Monochrome OBJ palette data
// FF4A & FF4B -> Window position (Y, X + 7)

impl IoRegisters {
    pub(super) fn tick(&mut self) {
        self.joypad.tick();
        self.lcd_status = (0b0000_0111 & self.lcd_status) | (0b0111_1000 & self.lcd_status_dup);
        self.lcd_status_dup = self.lcd_status;
        let byte = &mut self.timer_div[0];
        *byte = byte.wrapping_add(1);
        if self.tac.update_and_tick(self.timer_div[3]) {
            match self.timer_div[1].checked_add(1) {
                Some(b) => self.timer_div[1] = b,
                None => {
                    let b = self.timer_div[1];
                    self.timer_div[0] = b;
                    self.request_timer_int();
                }
            }
        }
    }

    pub(crate) fn set_ppu_status(&mut self, state: PpuMode) {
        self.lcd_status &= 0b1111_1100 | state as u8;
    }

    /// Called by the PPU when it finishes a scan line
    pub(crate) fn inc_lcd_y(&mut self) {
        self.lcd_y = (self.lcd_y + 1) % 154;
        if self.lcd_y == self.lcd_cmp {
            self.request_lcd_int();
        }
    }

    pub(crate) fn reset_lcd_y(&mut self) {
        self.lcd_y = 0;
    }

    pub fn request_vblank_int(&mut self) {
        // self.io.interrupt_flags |= self.ie & 0b1;
        self.interrupt_flags |= 0b1;
    }

    pub fn request_lcd_int(&mut self) {
        // self.io.interrupt_flags |= self.ie & 0b10;
        self.interrupt_flags |= 0b10;
    }

    pub fn request_timer_int(&mut self) {
        // self.io.interrupt_flags |= self.ie & 0b100;
        self.interrupt_flags |= 0b100;
    }

    pub fn request_serial_int(&mut self) {
        // self.io.interrupt_flags |= self.ie & 0b1000;
        self.interrupt_flags |= 0b1000;
    }

    pub fn request_button_int(&mut self, input: ButtonInput) {
        self.joypad.register_input(input);
        // self.io.interrupt_flags |= self.ie & 0b1_0000;
        self.interrupt_flags |= 0b1_0000;
    }

    pub(crate) fn clear_interrupt_req(&mut self, op: InterruptOp) {
        let mask = match op {
            InterruptOp::VBlank => 0b1,
            InterruptOp::LCD => 0b10,
            InterruptOp::Timer => 0b100,
            InterruptOp::Serial => 0b1000,
            InterruptOp::Joypad => 0b1_0000,
        };
        self.interrupt_flags &= !mask;
    }

    pub(crate) fn read_byte(&self, index: u16) -> u8 {
        match index {
            0xFF00 => self.joypad[()],
            0xFF01 => self.serial.0,
            0xFF02 => self.serial.1,
            n @ 0xFF04..=0xFF07 => self.timer_div[(n - 0xFF04) as usize],
            0xFF0F => self.interrupt_flags,
            n @ 0xFF10..=0xFF26 => self.audio[(n - 0xFF10) as usize],
            n @ 0xFF30..=0xFF3F => self.wave[(n - 0xFF30) as usize],
            0xFF40 => self.lcd_control,
            0xFF41 => self.lcd_status,
            0xFF42 => self.bg_position.0,
            0xFF43 => self.bg_position.1,
            0xFF44 => self.lcd_y,
            0xFF45 => self.lcd_cmp,
            0xFF47 => self.monochrome_bg_palette,
            0xFF48 => self.monochrome_obj_palettes[0],
            0xFF49 => self.monochrome_obj_palettes[1],
            0xFF4A => self.window_position[0],
            0xFF4B => self.window_position[1],
            // When reading from this register, all bits expect the first are 1
            0xFF4F => 0xFE | self.vram_select,
            0xFF50 => self.boot_status,
            n @ 0xFF68..=0xFF69 => self.background_palettes[n - 0xFF68],
            n @ 0xFF6A..=0xFF6B => self.object_palettes[n - 0xFF6A],
            0xFF70 => self.wram_select,
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF6F => self.dead_byte,
            ..=0xFEFF | 0xFF51..=0xFF55 | 0xFF46 | 0xFF71.. => unreachable!(
                "The MemoryMap should never index into the IO registers outside of 0xFF00-0xFF70!"
            ),
        }
    }

    pub(crate) fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0xFF00 => self.joypad[()] = value,
            0xFF01 => self.serial.0 = value,
            0xFF02 => self.serial.1 = value,
            0xFF04 => {
                self.timer_div[0] = 0;
                self.dead_byte = 0;
            }
            n @ 0xFF05..=0xFF07 => self.timer_div[(n - 0xFF04) as usize] = value,
            0xFF0F => self.interrupt_flags = value,
            n @ 0xFF10..=0xFF26 => self.audio[(n - 0xFF10) as usize] = value,
            n @ 0xFF30..=0xFF3F => self.wave[(n - 0xFF30) as usize] = value,
            0xFF40 => self.lcd_control = value,
            // TODO: Only part of this register can be written to. Only bits 3-6 can be written to.
            // This register needs to be reset when ticked.
            0xFF41 => self.lcd_status_dup = value,
            0xFF42 => self.bg_position.0 = value,
            0xFF43 => self.bg_position.1 = value,
            0xFF44 => {}
            0xFF45 => self.lcd_cmp = value,
            0xFF47 => self.monochrome_bg_palette = value,
            0xFF48 => self.monochrome_obj_palettes[0] = value,
            0xFF49 => self.monochrome_obj_palettes[1] = value,
            0xFF4A => self.window_position[0] = value,
            0xFF4B => self.window_position[1] = value,
            // Ignore all but the first bit of the value
            0xFF4F => {
                // println!("Selecting VRAM bank to be {value}");
                self.vram_select = 1 & value
            },
            0xFF50 => self.boot_status = value,
            n @ 0xFF68..=0xFF69 => self.background_palettes[n - 0xFF68] = value,
            n @ 0xFF6A..=0xFF6B => self.object_palettes[n - 0xFF6A] = value,
            0xFF70 => self.wram_select = value,
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF7F => {}
            ..=0xFEFF | 0xFF51..=0xFF55 | 0xFF46 | 0xFF80.. => unreachable!(
                "The MemoryMap should never index into the IO registers outside of 0xFF00-0xFF70!"
            ),
        }
    }

    pub(crate) fn update_byte<F: FnOnce(&mut u8)>(&mut self, index: u16, update: F) -> u8 {
        // TODO: Actually implement this...
        let mut value = self.read_byte(index);
        update(&mut value);
        self.write_byte(index, value);
        value
    }
}

impl Index<BgPaletteIndex> for IoRegisters {
    type Output = Palette;

    fn index(&self, BgPaletteIndex(index): BgPaletteIndex) -> &Self::Output {
        &self.background_palettes[index]
    }
}

impl Index<ObjPaletteIndex> for IoRegisters {
    type Output = Palette;

    fn index(&self, ObjPaletteIndex(index): ObjPaletteIndex) -> &Self::Output {
        &self.object_palettes[index]
    }
}

/// In GBC mode, there are extra palettes for the colors
#[derive(Debug, Clone, Hash, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ColorPalettes {
    pub(crate) index: Wrapping<u8>,
    /// This array is indexed into by the index field.
    pub(crate) data: [Palette; 8],
}

impl ColorPalettes {
    fn tick(&mut self) {
        self.index &= 0b1011_1111;
        let (a, b) = self.indices();
        self.data[a as usize].tick(b);
    }

    fn indices(&self) -> (u8, u8) {
        let index = self.index.0 & 0b0011_1111;
        ((index & 0b0011_1000) >> 3, index & 0b111)
    }
}

impl Index<u8> for ColorPalettes {
    type Output = Palette;

    fn index(&self, index: u8) -> &Self::Output {
        &self.data[index as usize]
    }
}

impl Index<u16> for ColorPalettes {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        match index {
            0 => &self.index.0,
            1 => {
                let (a, b) = self.indices();
                &self.data[a as usize][b]
            }
            _ => unreachable!(),
        }
    }
}

impl IndexMut<u16> for ColorPalettes {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        match index {
            0 => &mut self.index.0,
            1 => {
                let (a, b) = self.indices();
                let inc = check_bit_const::<7>(self.index.0) as u8;
                self.index += inc;
                // TODO: If the PPU is in mode 3, this should return a dead byte.
                &mut self.data[a as usize][b]
            }
            _ => unreachable!(),
        }
    }
}

/// All of the date for one of the 8 palettes that can be held in memory.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Palette {
    pub(crate) colors: [PaletteColor; 4],
}

impl Palette {
    fn tick(&mut self, index: u8) {
        self.colors[index as usize].tick();
    }

    pub fn get_color(&self, index: u8) -> PaletteColor {
        self.colors[index as usize]
    }
}

impl Index<u8> for Palette {
    type Output = u8;

    fn index(&self, index: u8) -> &Self::Output {
        debug_assert!(index < 8);
        &self.colors[(index & 0b110) as usize >> 1][index & 0b1]
    }
}

impl IndexMut<u8> for Palette {
    fn index_mut(&mut self, index: u8) -> &mut Self::Output {
        debug_assert!(index < 8);
        &mut self.colors[(index & 0b110) as usize >> 1][index & 0b1]
    }
}

/// The colors inside a palette are a bit odd. Each color takes up two bytes and represents each
/// color with 5 bits (in little-endian). The top bit is not used.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct PaletteColor(pub [u8; 2]);

impl PaletteColor {
    fn tick(&mut self) {
        self.0[1] &= 0xEF;
    }

    pub fn r(&self) -> u8 {
        self.0[0] & 0b0001_1111
    }

    pub fn g(&self) -> u8 {
        ((self.0[0] & 0b1110_0000) >> 5) | ((self.0[1] & 0b0000_0011) << 3)
    }

    pub fn b(&self) -> u8 {
        (self.0[1] & 0b0111_1100) >> 2
    }
}

impl From<PaletteColor> for Pixel {
    fn from(value: PaletteColor) -> Self {
        Pixel {
            r: value.r(),
            g: value.g(),
            b: value.b(),
        }
    }
}

impl Index<u8> for PaletteColor {
    type Output = u8;

    fn index(&self, index: u8) -> &Self::Output {
        debug_assert!(index < 2);
        &self.0[index as usize]
    }
}

impl IndexMut<u8> for PaletteColor {
    fn index_mut(&mut self, index: u8) -> &mut Self::Output {
        debug_assert!(index < 2);
        &mut self.0[index as usize]
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default, Serialize, Deserialize)]
pub enum TimerControl {
    #[default]
    Disabled,
    /// Ticks once per 256 M-cycles
    Slowest(u16),
    /// Ticks once per 64 M-cycles
    Slow(u8),
    /// Ticks once per 16 M-cycles
    Fast(u8),
    /// Ticks once per 4 M-cycles
    Fastest(u8),
}

/// This is a bit messy... The bottom half of this register is read-only via instruction and is
/// set at the same time the joypad interrupt is called. We can't expose this byte when mutably
/// indexing into, so we have to store two bytes. How do we keep them synchronized?
/// Let's say they are in sync and have the state 0b01_0000.
/// Then, the right button is press. An interrupt is fired and can write to both bytes.
/// Great, the current state is 0b010_0001 for both bytes.
/// Then, the CPU indexs to change the select-mode bytes.
/// Now, they are out of sync (because we only expose the byte that contains the select-mode bits,
/// so only the gets mutated). This causes a problem when you next read from this register. We
/// can't expose the byte that holds the read-only nibble, its out of date. But, we can't expose
/// the byte with the select-mode bits because the lower half could have been written to. Moreover,
/// neither byte can be mutated (safely) and we need an actual byte to return a reference to.
///
/// The solution: Both registers will be synchronized when a tick is processed. Currently, this
/// happens immediately before the instruction is actually processed, but this would also work if
/// it happened immediately after too. As long as the tick is applied in the same order everywhere.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Serialize, Deserialize)]
pub(super) struct Joypad {
    main: u8,
    dup: u8,
}

impl TimerControl {
    /// Updates the inner counter that control when the timer register is inc-ed. Note that this
    /// method is called during every *clock* cycle.
    fn update_and_tick(&mut self, state: u8) -> bool {
        fn checked_reset<const R: u8>(byte: &mut u8) -> bool {
            *byte = byte.checked_add(1).unwrap_or(R);
            *byte == R
        }
        let other = Self::from_byte(state);
        if std::mem::discriminant(self) != std::mem::discriminant(&other) {
            // TODO: There are a few goofy things that can happen when changing the timer control,
            // which are discussed in the pandocs. Those need to be implemented here.
            *self = other;
        }
        match self {
            TimerControl::Disabled => false,
            TimerControl::Slowest(count) => {
                const RESET: u16 = u16::MAX - 256 * 4;
                *count = count.checked_add(1).unwrap_or(RESET);
                *count == RESET
            }
            TimerControl::Slow(count) => checked_reset::<0>(count),
            TimerControl::Fast(count) => checked_reset::<191>(count), // u8::MAX - 16*4
            TimerControl::Fastest(count) => checked_reset::<239>(count), // u8:::MAX - 16
        }
    }

    fn from_byte(byte: u8) -> Self {
        if check_bit_const::<2>(byte) {
            match byte & 0b11 {
                0b00 => Self::Slowest(0),
                0b01 => Self::Fastest(0),
                0b10 => Self::Fast(0),
                0b11 => Self::Slow(0),
                _ => unreachable!(),
            }
        } else {
            Self::Disabled
        }
    }
}

impl Joypad {
    /// Method is called when the memory is ticked. Because we have to virtualize the read-only
    /// nibble, we have to ensure the main and duplicate bits are synced. Additionally, there is
    /// this bit from the pandocs:
    /// > If neither buttons nor d-pad is selected ($30 was written), then the low nibble reads $F (all buttons released).
    /// Accounting for this is done here.
    fn tick(&mut self) {
        // The main byte is the byte that gets referenced when indexing. This means that any
        // changes to its lower half (the read-only nibble) must be ignored. These bits are stored
        // in the dup nibble. This method brings them in sync by overwriting the lower nibble of
        // the main byte with the lower nibble of the dup byte. At the same time, it is calculated
        // if both or neither of the select-mode bits are set (see the quote above). In this case,
        // the main bit is ORed with 0x0F.
        // NOTE: dup is never updated except when input is registered.
        self.main &= 0x30; // Take away everything but the select-mode bits
        let mut mask = self.main;
        mask = 0x10 & !(((mask & 0x20) >> 1) ^ (mask & 0x10)); // tmp is now either 0x10 or 0x00
        mask = mask.saturating_sub(1); // tmp is now either 0x0F or 0x00
        self.main |= mask | self.dup;
    }

    pub(super) fn register_input(&mut self, input: ButtonInput) {
        let byte = match input {
            ButtonInput::Joypad(button)
                if check_bit_const::<4>(self.main) && !check_bit_const::<5>(self.main) =>
            {
                button as u8
            }
            ButtonInput::Ssab(button)
                if !check_bit_const::<4>(self.main) && check_bit_const::<5>(self.main) =>
            {
                button as u8
            }
            _ => return,
        };
        debug_assert_eq!(byte.count_ones(), 1);
        debug_assert!(byte < 0xF0);
        self.dup &= byte;
        self.main &= byte;
    }
}

impl Index<()> for Joypad {
    type Output = u8;

    fn index(&self, index: ()) -> &Self::Output {
        &self.main
    }
}

impl IndexMut<()> for Joypad {
    fn index_mut(&mut self, index: ()) -> &mut Self::Output {
        &mut self.main
    }
}

#[cfg(test)]
mod tests {
    use super::{ColorPalettes, Palette, PaletteColor};

    #[test]
    fn indexing_into_palettes() {
        let mut palettes = ColorPalettes {
            index: crate::utils::Wrapping(0x80),
            data: std::array::from_fn(|i| Palette {
                colors: std::array::from_fn(|j| {
                    PaletteColor([
                        8 * (i as u8) + 2 * (j as u8),
                        8 * (i as u8) + 2 * (j as u8) + 1,
                    ])
                }),
            }),
        };
        for i in 0..64u16 {
            println!("{}", palettes.index);
            println!("{:?}", palettes.data[0]);
            let data = &mut palettes[1];
            assert_eq!(i as u8, *data);
        }
    }
}
