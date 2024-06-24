use std::ops::{Index, IndexMut};

use tracing::trace;

use crate::{cpu::check_bit_const, ButtonInput};

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
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
    /// ADDR FF40-FF4B
    lcd: LcdRegisters,
    /// ADDR FF4F
    vram_select: u8,
    /// ADDR FF50
    boot_status: u8,
    /// ADDR FF51-FF55
    vram_dma: [u8; 5],
    /// ADDR FF68 and FF69
    pub(crate) background_palettes: ColorPalettes,
    /// ADDR FF6A and FF6B
    object_palettes: ColorPalettes,
    /// ADDR FF70
    wram_select: u8,
    /// There are gaps amount the memory mapped IO registers. Any index into this that hits one of
    /// these gaps resets the value. Notably, this is also used when mutably indexing to the
    /// divider register but not while immutably indexing.
    /// ADDR FF03, FF08-FF0E, FF27-FF29, FF4C-FF4E, FF56-FF67, FF6C-FF6F
    dead_byte: u8,
}

/// In GBC mode, there are extra palettes for the colors
#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
pub struct ColorPalettes {
    // TODO: When the palette is mutably indexed into, increment this value iff the top bit is set.
    index: u8,
    /// This array is indexed into by the index field.
    data: [Palette; 8],
}

impl ColorPalettes {
    fn tick(&mut self) {
        self.index &= 0x9F;
        let (a, b) = self.indices();
        self.data[(a) as usize].tick(b);
    }

    pub(crate) fn get_palette(&self, index: u8) -> &Palette {
        debug_assert!(index < 8);
        &self.data[index as usize]
    }

    fn indices(&self) -> (u8, u8) {
        let index = self.index & 0x1F;
        (index / 8, index % 8)
    }
}

impl Index<u16> for ColorPalettes {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        match index {
            0 => &self.index,
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
            0 => &mut self.index,
            1 => {
                let (a, b) = self.indices();
                self.index += self.index >> 7;
                &mut self.data[a as usize][b]
            }
            _ => unreachable!(),
        }
    }
}

/// All of the date for one of the 8 palettes that can be held in memory.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
pub struct Palette {
    colors: [PaletteColor; 4],
}

impl Palette {
    fn tick(&mut self, index: u8) {
        self.colors[index as usize].tick();
    }

    fn get_color(&self, index: u8) -> PaletteColor {
        self.colors[index as usize]
    }
}

impl Index<u8> for Palette {
    type Output = u8;

    fn index(&self, index: u8) -> &Self::Output {
        debug_assert!(index < 8);
        &self.colors[(index / 2) as usize][index % 2]
    }
}

impl IndexMut<u8> for Palette {
    fn index_mut(&mut self, index: u8) -> &mut Self::Output {
        debug_assert!(index < 8);
        &mut self.colors[(index / 2) as usize][index % 2]
    }
}

/// The colors inside a palette are a bit odd. Each color takes up two bytes and represents each
/// color with 5 bits (in little-endian). The top bit is not used.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default)]
pub struct PaletteColor([u8; 2]);

impl PaletteColor {
    fn tick(&mut self) {
        self.0[1] &= 0xEF;
    }

    fn r(&self) -> u8 {
        self.0[0] & 0x1F
    }

    fn g(&self) -> u8 {
        ((self.0[0] & 0xD0) >> 5) | ((self.0[1] & 0x03) << 3)
    }

    fn b(&self) -> u8 {
        self.0[1] & 0x7C
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

/// Addressed between 0xFF40 and FF4B, this type encapsulates
#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
pub struct LcdRegisters {
    /// Monochrome palettes: FF47
    bgp: u8,
    /// Monochrome palette data: FF48, FF49
    obgp: [u8; 2],
}

impl Index<u16> for LcdRegisters {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        match index {
            0xFF47 => &self.bgp,
            0xFF48 => &self.obgp[0],
            0xFF49 => &self.obgp[1],
            _ => unreachable!(),
        }
    }
}

impl IndexMut<u16> for LcdRegisters {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        match index {
            0xFF47 => &mut self.bgp,
            0xFF48 => &mut self.obgp[0],
            0xFF49 => &mut self.obgp[1],
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Default)]
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
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone)]
pub(super) struct Joypad {
    main: u8,
    dup: u8,
}

impl IoRegisters {
    pub(super) fn tick(&mut self) {
        self.joypad.tick();
        let byte = &mut self.timer_div[0];
        *byte = byte.wrapping_add(1);
        if self.tac.update_and_tick(self.timer_div[3]) {
            match self.timer_div[1].checked_add(1) {
                Some(b) => self.timer_div[1] = b,
                None => {
                    let b = self.timer_div[1];
                    self.timer_div[0] = b;
                    self.interrupt_flags |= 0b100;
                }
            }
        }
    }
}

impl Index<u16> for IoRegisters {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        match index {
            0xFF00 => &self.joypad[()],
            0xFF01 => &self.serial.0,
            0xFF02 => &self.serial.1,
            n @ 0xFF04..=0xFF07 => &self.timer_div[(n - 0xFF04) as usize],
            0xFF0F => &self.interrupt_flags,
            n @ 0xFF10..=0xFF26 => &self.audio[(n - 0xFF10) as usize],
            n @ 0xFF30..=0xFF3F => &self.wave[(n - 0xFF30) as usize],
            n @ 0xFF40..=0xFF4B => &self.lcd[n],
            0xFF4F => &self.vram_select,
            0xFF50 => &self.boot_status,
            n @ 0xFF51..=0xFF55 => &self.vram_dma[(n - 0xFF51) as usize],
            n @ 0xFF68..=0xFF69 => &self.background_palettes[n - 0xFF68],
            n @ 0xFF6A..=0xFF6B => &self.object_palettes[n - 0xFF68],
            0xFF70 => &self.wram_select,
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF6F => &self.dead_byte,
            ..=0xFEFF | 0xFF71.. => unreachable!(
                "The MemoryMap should never index into the IO registers outside of 0xFF00-0xFF70!"
            ),
        }
    }
}

impl IndexMut<u16> for IoRegisters {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        trace!("Index: 0x{index:0>4X}");
        match index {
            0xFF00 => &mut self.joypad[()],
            0xFF01 => &mut self.serial.0,
            0xFF02 => &mut self.serial.1,
            0xFF04 => {
                self.timer_div[0] = 0;
                self.dead_byte = 0;
                &mut self.dead_byte
            }
            n @ 0xFF05..=0xFF07 => &mut self.timer_div[(n - 0xFF04) as usize],
            0xFF0F => &mut self.interrupt_flags,
            n @ 0xFF10..=0xFF26 => &mut self.audio[(n - 0xFF10) as usize],
            n @ 0xFF30..=0xFF3F => &mut self.wave[(n - 0xFF30) as usize],
            n @ 0xFF40..=0xFF4B => &mut self.lcd[n],
            0xFF4F => &mut self.vram_select,
            0xFF50 => &mut self.boot_status,
            n @ 0xFF51..=0xFF55 => &mut self.vram_dma[(n - 0xFF51) as usize],
            n @ 0xFF68..=0xFF69 => &mut self.background_palettes[n - 0xFF68],
            n @ 0xFF6A..=0xFF6B => &mut self.object_palettes[n - 0xFF6A],
            0xFF70 => &mut self.wram_select,
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF7F => &mut self.dead_byte,
            ..=0xFEFF | 0xFF80.. => unreachable!(
                "The MemoryMap should never index into the IO registers outside of 0xFF00-0xFF70!"
            ),
        }
    }
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
