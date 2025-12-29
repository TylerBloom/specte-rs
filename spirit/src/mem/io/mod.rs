use std::ops::Index;
use std::ops::IndexMut;

use serde::Deserialize;
use serde::Serialize;
use tracing::debug;
use tracing::trace;

use crate::ButtonInput;
use crate::cpu::check_bit_const;
use crate::lookup::InterruptOp;
use crate::ppu::Pixel;
use crate::utils::Wrapping;

use super::MemoryMap;
use super::vram::PpuMode;
use super::vram::VRam;

pub mod timers;

use timers::TimerRegisters;

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct IoRegisters {
    /// ADDR FF00
    pub(super) joypad: Joypad,
    /// ADDR FF01, FF02
    serial: (u8, u8),
    /// ADDR FF04, FF05, FF06, FF07
    /// There are the (divider, timer, timer modulo, tac)
    /// The counter for the timer controller
    pub tac: TimerRegisters,
    /// ADDR FF0F
    pub interrupt_flags: u8,
    /// ADDR FF10-FF26
    audio: AudioRegisters,
    /// ADDR FF40
    pub lcd_control: u8,
    /// ADDR FF41
    lcd_status: u8,
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
    pub boot_status: u8,
    /// The boot status register is written to exactly once (during the boot process). After that,
    /// it is inaccessible
    boot_status_disabled: bool,
    /// ADDR FF68 and FF69
    pub background_palettes: ColorPalettes,
    /// ADDR FF6A and FF6B
    pub(crate) object_palettes: ColorPalettes,
    /// ADDR FF70
    wram_select: u8,
    undoc_registers: [u8; 4],
    /// There are gaps amount the memory mapped IO registers. Any index into this that hits one of
    /// these gaps resets the value. Notably, this is also used when mutably indexing to the
    /// divider register but not while immutably indexing.
    /// ADDR FF03, FF08-FF0E, FF27-FF29, FF4C-FF4E, FF56-FF67, FF6C-FF6F
    dead_byte: u8,
}

impl Default for IoRegisters {
    fn default() -> Self {
        Self {
            undoc_registers: [0, 0, 0, 0b1000_1111],
            joypad: Default::default(),
            serial: Default::default(),
            tac: TimerRegisters::new(),
            interrupt_flags: Default::default(),
            audio: Default::default(),
            lcd_control: Default::default(),
            lcd_status: Default::default(),
            bg_position: Default::default(),
            lcd_y: Default::default(),
            lcd_cmp: Default::default(),
            monochrome_bg_palette: Default::default(),
            monochrome_obj_palettes: Default::default(),
            window_position: Default::default(),
            vram_select: Default::default(),
            boot_status: Default::default(),
            background_palettes: Default::default(),
            object_palettes: Default::default(),
            wram_select: Default::default(),
            dead_byte: Default::default(),
            boot_status_disabled: false,
        }
    }
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
struct AudioRegisters {
    /// The control for the CH1 sweep
    /// This register is at FF10
    ch1_sweep: u8,

    /// The control for the CH1 length timer and duty cycle.
    /// This register is at FF11
    ch1_length_and_duty: u8,

    /// The control for the CH1 volume and envelope.
    /// This register is at FF12
    ch1_vol_and_env: u8,

    /// The CH1 period low bits.
    /// This register is at FF13
    ///
    /// NOTE: This register is write only. Reads return 0xFF.
    ch1_period_low: u8,

    /// The control for the CH1 volume and envelope.
    /// This register is at FF14
    ///
    /// NOTE: Bits 3, 4, and 5 are unused. Also, Bits 0, 1, 2, and 7 are write only.
    ch1_period_and_control: u8,

    /// The register between CH1 and CH2 is not used, but the JSON CPU tests use it for some
    /// reason...
    ///
    /// ADDR: FF15
    unused_15: u8,

    /// The control for the CH2 length timer and duty cycle.
    /// This register is at FF16
    ch2_length_and_duty: u8,

    /// The control for the CH2 volume and envelope.
    /// This register is at FF17
    ch2_vol_and_env: u8,

    /// The CH2 period low bits.
    /// This register is at FF18
    ///
    /// NOTE: This register is write only. Reads return 0xFF.
    ch2_period_low: u8,

    /// The control for the CH2 volume and envelope.
    /// This register is at FF19
    ///
    /// NOTE: Bits 3, 4, and 5 are unused. Also, Bits 0, 1, 2, and 7 are write only.
    ch2_period_and_control: u8,

    /// The control for the CH3 DAC. Only one bit is used, bit 7.
    /// This register is at FF1A
    ch3_dac_enable: u8,

    /// The control for the CH3 length timer.
    /// This register is at FF1B
    ///
    /// NOTE: This register is write-only.
    ch3_length_timer: u8,

    /// The control for the CH3 output level. Only the 5th and 6th bits are used.
    /// This register is at FF1C
    ch3_output_level: u8,

    /// The control for the CH3 period control.
    /// This register is at FF1D
    ///
    /// NOTE: This register is write-only.
    ch3_period_low: u8,

    /// The control for the CH3 period control.
    /// This register is at FF1E.
    ///
    /// NOTE: Bits 3, 4, and 5 are unused. Also, Bits 0, 1, 2, and 7 are write only.
    ch3_period_and_control: u8,

    /// The register between CH3 and CH4 is not used, but the JSON CPU tests use it for some
    /// reason...
    ///
    /// ADDR: FF1F
    unused_1f: u8,

    /// The control for the CH4 length timer
    /// This register is at FF20
    ///
    /// NOTE: This register is write-only, and the top two bits are not used.
    ch4_length_timer: u8,

    /// The control for the CH4 volume and envelope.
    /// This register is at FF21
    ch4_vol_and_env: u8,

    /// The control for the CH4 frequency and randomness
    /// This register is at FF22
    ch4_freq_and_rand: u8,

    /// The CH4 control
    /// This register is at FF23
    ch4_control: u8,

    /// The volume control for the amplifier
    /// This register is at FF24
    master_control_vin_planning: u8,

    /// The control for the panning of each channel
    /// This register is at FF25
    panning: u8,

    /// The master control for audio.
    /// This register is at FF26
    master_control: u8,

    /// The registers between FF23 and FF30 aren't used, but the JSON CPU tests use them for some
    /// reason...
    ///
    /// ADDRS: FF27-FF29
    unused_gap: [u8; 9],

    /// FF30-FF3F
    /// Accessing this region of memory while the channel is active, accessing this region will
    /// yield in whatever byte the channel is currently reading.
    /// NOTE: This is more than the DAC being enabled/disabled.
    ch3_wave_form: [u8; 0x10],
}

impl AudioRegisters {
    fn read_byte(&self, index: u16) -> u8 {
        match index {
            /* Channel 1 */
            0xFF10 => self.ch1_sweep,
            0xFF11 => self.ch1_length_and_duty,
            0xFF12 => self.ch1_vol_and_env,
            // NOTE: self.ch1_period_low is write-only
            0xFF13 => 0xFF,
            // NOTE: Only the 6th bit is readable
            0xFF14 => self.ch1_period_and_control & 0b0010_0000,
            0xFF15 => self.unused_15,
            /* Channel 2 */
            0xFF16 => self.ch2_length_and_duty,
            0xFF17 => self.ch2_vol_and_env,
            // NOTE: self.ch2_period_low is write-only
            0xFF18 => 0xFF,
            // NOTE: Only the 6th bit is readable
            0xFF19 => self.ch2_period_and_control & 0b0010_0000,
            /* Channel 3 */
            // NOTE: Only the topmost bit is used
            0xFF1A => self.ch3_dac_enable & 0b1000_0000,
            // NOTE: self.ch3_length_timer is write-only
            0xFF1B => 0xFF,
            // NOTE: Only bits 5 and 6 are used
            0xFF1C => self.ch3_output_level & 0b0110_0000,
            // NOTE: self.ch3_period_low is write-only
            0xFF1D => 0xFF,
            // NOTE: Only bit 6 is readable
            0xFF1E => self.ch3_period_and_control & 0b0100_0000,
            0xFF1F => self.unused_1f,
            /* Channel 4 */
            // NOTE: self.ch4_length_timer is write-only
            0xFF20 => 0xFF,
            0xFF21 => self.ch4_vol_and_env,
            0xFF22 => self.ch4_freq_and_rand,
            // NOTE: Only bit 6 is readable
            0xFF23 => self.ch4_control & 0b0100_0000,
            /* Controls */
            0xFF24 => self.master_control_vin_planning,
            0xFF25 => self.panning,
            // NOTE: Bits 4, 5, 6 are not used
            0xFF26 => self.master_control & 0b1000_1111,
            0xFF27..=0xFF2F => self.unused_gap[(index as usize) - 0xFF27],
            n @ 0xFF30..=0xFF3F => self.ch3_wave_form[(n - 0xFF30) as usize],
            /* Oops... */
            ..0xFF10 | 0xFF40.. => {
                unreachable!("There was an attemped read from an unused bit @ 0x{index:0>4x}")
            }
        }
    }

    fn write_byte(&mut self, index: u16, mut val: u8) {
        match index {
            /* Channel 1 */
            // NOTE: The top bit is not used
            0xFF10 => self.ch1_sweep = val & 0b0111_1111,
            0xFF11 => self.ch1_length_and_duty = val,
            0xFF12 => self.ch1_vol_and_env = val,
            0xFF13 => self.ch1_period_low = val,
            0xFF15 => self.unused_15 = val,
            // NOTE: Bits 3, 4, 5 are not used
            0xFF14 => self.ch1_period_and_control = val & 0b1100_0111,
            /* Channel 2 */
            0xFF16 => self.ch2_length_and_duty = val,
            0xFF17 => self.ch2_vol_and_env = val,
            0xFF18 => self.ch2_period_low = val & 0b1100_0111,
            // NOTE: Bits 3, 4, 5 are not used
            0xFF19 => self.ch2_period_and_control = val & 0b1100_0111,
            /* Channel 3 */
            // NOTE: Only the topmost bit is used
            0xFF1A => self.ch3_dac_enable = val & 0b1000_0000,
            0xFF1B => self.ch3_length_timer = val,
            // NOTE: Only bits 5 and 6 are usd
            0xFF1C => self.ch3_output_level = val & 0b0110_0000,
            0xFF1D => self.ch3_period_low = val,
            // NOTE: Bits 3, 4, 5 are not used
            0xFF1E => self.ch3_period_and_control = val & 0b1100_0111,
            0xFF1F => self.unused_1f = val,
            /* Channel 4 */
            // NOTE: The top 2 bits are not used
            0xFF20 => self.ch4_length_timer = val & 0b0011_1111,
            0xFF21 => self.ch4_vol_and_env = val,
            0xFF22 => self.ch4_freq_and_rand = val,
            // NOTE: Only the top 2 bits are used
            0xFF23 => self.ch4_control = val & 0b1100_0000,
            /* Controls */
            // NOTE: Only the topmost bit is writeable
            0xFF24 => self.master_control_vin_planning = val,
            0xFF25 => self.panning = val,
            0xFF26 => self.master_control = val & 0b1000_0000,
            0xFF27..=0xFF2F => self.unused_gap[(index as usize) - 0xFF27] = val,
            n @ 0xFF30..=0xFF3F => self.ch3_wave_form[(n - 0xFF30) as usize] = val,
            /* Oops... */
            ..0xFF10 | 0xFF40.. => {
                unreachable!("There was an attemped read from an unused bit @ 0x{index:0>4x}")
            }
        }
    }
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
        if self.tac.tick() {
            self.request_timer_int();
        }
    }

    pub(crate) fn set_ppu_status(&mut self, state: PpuMode) {
        // Clear the bottom two bits
        self.lcd_status &= 0b1111_1100;
        // Update the bottom two bits to be the state
        self.lcd_status |= state as u8;
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
            0xFF04..=0xFF07 => self.tac.read_byte(index),
            0xFF0F => self.interrupt_flags,
            0xFF10..=0xFF3F => self.audio.read_byte(index),
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
            0xFF50 => {
                if self.boot_status_disabled {
                    0xFF
                } else {
                    self.boot_status
                }
            }
            n @ 0xFF68..=0xFF69 => self.background_palettes[n - 0xFF68],
            n @ 0xFF6A..=0xFF6B => self.object_palettes[n - 0xFF6A],
            0xFF70 => self.wram_select,
            0xFF72 => self.undoc_registers[0],
            0xFF73 => self.undoc_registers[1],
            0xFF74 => self.undoc_registers[2],
            0xFF75 => self.undoc_registers[3],
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF6F
            | 0xFF71
            | 0xFF76.. => 0xFF,
            ..=0xFEFF | 0xFF51..=0xFF55 | 0xFF46 => unreachable!(
                "The MemoryMap should never index into the IO registers outside of 0xFF00-0xFF70! Got read index 0x{index:0>4x}"
            ),
        }
    }

    pub(crate) fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0xFF00 => self.joypad[()] = value,
            0xFF01 => self.serial.0 = value,
            0xFF02 => self.serial.1 = value,
            0xFF04..=0xFF07 => {
                if self.tac.write_byte(index, value) {
                    self.request_timer_int();
                }
            }
            // Top three bits are ignored because there are only 5 types of interrupts
            0xFF0F => self.interrupt_flags = 0x1F & value,
            0xFF10..=0xFF3F => self.audio.write_byte(index, value),
            0xFF40 => self.lcd_control = value,
            // TODO: Only part of this register can be written to. Only bits 3-6 can be written to.
            // This register needs to be reset when ticked.
            0xFF41 => self.lcd_status = 0b0111_1000 & value,
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
            0xFF4F => self.vram_select = 1 & value,
            0xFF50 => {
                self.boot_status_disabled = true;
                self.boot_status = value
            }
            n @ 0xFF68..=0xFF69 => self.background_palettes[n - 0xFF68] = value,
            n @ 0xFF6A..=0xFF6B => self.object_palettes[n - 0xFF6A] = value,
            0xFF70 => self.wram_select = value,
            0xFF72 => self.undoc_registers[0] = value,
            0xFF73 => self.undoc_registers[1] = value,
            0xFF74 => self.undoc_registers[2] = value,
            0xFF75 => {
                self.undoc_registers[3] = value | 0b1000_1111;
            }
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF6F
            | 0xFF71
            | 0xFF76.. => {}
            ..=0xFEFF | 0xFF51..=0xFF55 | 0xFF46 | 0xFF80.. => unreachable!(
                "The MemoryMap should never index into the IO registers outside of 0xFF00-0xFF70! Got write index 0x{index:0>4x}"
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

impl Joypad {
    /// Method is called when the memory is ticked. Because we have to virtualize the read-only
    /// nibble, we have to ensure the main and duplicate bits are synced. Additionally, there is
    /// this bit from the pandocs:
    /// > If neither buttons nor d-pad is selected ($30 was written), then the low nibble reads $F (all buttons released).
    /// > Accounting for this is done here.
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
    use super::ColorPalettes;
    use super::Palette;
    use super::PaletteColor;

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
            let data = &mut palettes[1];
            assert_eq!(i as u8, *data);
        }
    }
}
