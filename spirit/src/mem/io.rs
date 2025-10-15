use std::ops::Index;
use std::ops::IndexMut;

use serde::Deserialize;
use serde::Serialize;
use tracing::trace;

use crate::ButtonInput;
use crate::cpu::check_bit_const;
use crate::lookup::InterruptOp;
use crate::ppu::Pixel;
use crate::utils::Wrapping;

use super::MemoryMap;
use super::vram::PpuMode;
use super::vram::VRam;

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct IoRegisters {
    /// ADDR FF00
    pub(super) joypad: Joypad,
    /// ADDR FF01, FF02
    serial: (u8, u8),
    /// ADDR FF04, FF05, FF06, FF07
    /// There are the (divider, timer, timer modulo, tac)
    /// The counter for the timer controller
    tac: TimerRegisters,
    /// ADDR FF0F
    pub interrupt_flags: u8,
    /// ADDR FF10-FF26
    audio: AudioRegisters,
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
            lcd_status_dup: Default::default(),
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
    /// The volume control for the amplifier
    /// This register is at FF24
    master_control_vin_planning: u8,

    /// The control for the panning of each channel
    /// This register is at FF26
    panning: u8,

    /// The master control for audio.
    /// This register is at FF26
    master_control: u8,

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
    /// This register is at FF12
    ///
    /// NOTE: This register is write only. Reads return 0xFF.
    ch1_period_low: u8,

    /// The control for the CH1 volume and envelope.
    /// This register is at FF12
    ///
    /// NOTE: Bits 3, 4, and 5 are unused. Also, Bits 0, 1, 2, and 7 are write only.
    ch1_period_and_control: u8,

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

    /// FF30-FF3F
    /// Accessing this region of memory while the channel is active, accessing this region will
    /// yield in whatever byte the channel is currently reading.
    /// NOTE: This is more than the DAC being enabled/disabled.
    ch3_wave_form: [u8; 0x10],

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
}

impl AudioRegisters {
    fn read_byte(&self, index: u16) -> u8 {
        match index {
            /* Controls */
            0xFF24 => self.master_control_vin_planning,
            0xFF25 => self.panning,
            // NOTE: Bits 4, 5, 6 are not used
            0xFF26 => self.master_control & 0b1000_1111,
            /* Channel 1 */
            0xFF10 => self.ch1_sweep,
            0xFF11 => self.ch1_length_and_duty,
            0xFF12 => self.ch1_vol_and_env,
            // NOTE: self.ch1_period_low is write-only
            0xFF13 => 0xFF,
            // NOTE: Only the 6th bit is readable
            0xFF14 => self.ch1_period_and_control & 0b0100_000,
            /* Channel 2 */
            0xFF16 => self.ch2_length_and_duty,
            0xFF17 => self.ch2_vol_and_env,
            // NOTE: self.ch2_period_low is write-only
            0xFF18 => 0xFF,
            // NOTE: Only the 6th bit is readable
            0xFF19 => self.ch2_period_and_control & 0b0100_000,
            /* Channel 3 */
            // NOTE: Only the topmost bit is used
            0xFF1A => self.ch3_dac_enable & 0b1000_000,
            // NOTE: self.ch3_length_timer is write-only
            0xFF1B => 0xFF,
            // NOTE: Only bits 5 and 6 are used
            0xFF1C => self.ch3_output_level & 0b0110_0000,
            // NOTE: self.ch3_period_low is write-only
            0xFF1D => 0xFF,
            // NOTE: Only bit 6 is readable
            0xFF1E => self.ch3_period_and_control & 0b0100_0000,
            n @ 0xFF30..=0xFF3F => self.ch3_wave_form[(n - 0xFF30) as usize],
            /* Channel 4 */
            // NOTE: self.ch4_length_timer is write-only
            0xFF20 => 0xFF,
            0xFF21 => self.ch4_vol_and_env,
            0xFF22 => self.ch4_freq_and_rand,
            // NOTE: Only bit 6 is readable
            0xFF23 => self.ch4_control & 0b0100_0000,
            /* Oops... */
            idx => unreachable!("There was an attemped read from an unused bit @ 0x{idx:0>4x}"),
        }
    }

    fn write_byte(&mut self, index: u16, mut val: u8) {
        match index {
            /* Controls */
            // NOTE: Only the topmost bit is writeable
            0xFF24 => self.master_control_vin_planning = val,
            0xFF25 => self.panning = val,
            0xFF26 => self.master_control = val & 0b1000_0000,
            /* Channel 1 */
            // NOTE: The top bit is not used
            0xFF10 => self.ch1_sweep = val & 0b0111_1111,
            0xFF11 => self.ch1_length_and_duty = val,
            0xFF12 => self.ch1_vol_and_env = val,
            0xFF13 => self.ch1_period_low = val,
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
            n @ 0xFF30..=0xFF3F => self.ch3_wave_form[(n - 0xFF30) as usize] = val,
            /* Channel 4 */
            // NOTE: The top 2 bits are not used
            0xFF20 => self.ch4_length_timer = val & 0b0011_1111,
            0xFF21 => self.ch4_vol_and_env = val,
            0xFF22 => self.ch4_freq_and_rand = val,
            // NOTE: Only the top 2 bits are used
            0xFF23 => self.ch4_control = val & 0b1100_0000,
            /* Oops... */
            idx => unreachable!("There was an attemped read from an unused bit @ 0x{idx:0>4x}"),
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
        self.lcd_status = (0b0000_0111 & self.lcd_status) | (0b0111_1000 & self.lcd_status_dup);
        self.lcd_status_dup = self.lcd_status;
        if self.tac.tick() {
            self.request_timer_int();
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
            0xFF75 => {
                println!(
                    "Reading from address 0xFF75, value = 0b{:0>8b}",
                    self.undoc_registers[3]
                );
                self.undoc_registers[3]
            }
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4C..=0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF6F
            | 0xFF71
            | 0xFF76.. => 0xFF,
            ..=0xFEFF | 0xFF51..=0xFF55 | 0xFF46 => unreachable!(
                "The MemoryMap should never index into the IO registers outside of 0xFF00-0xFF80! Got index 0x{index:0>4x}"
            ),
        }
    }

    pub(crate) fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0xFF00 => self.joypad[()] = value,
            0xFF01 => self.serial.0 = value,
            0xFF02 => self.serial.1 = value,
            0xFF04..=0xFF07 => self.tac.write_byte(index, value),
            0xFF0F => self.interrupt_flags = value,
            0xFF10..=0xFF3F => self.audio.write_byte(index, value),
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
            }
            0xFF50 => {
                println!("Writing to boot status: 0x{value:0>2X}");
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
                println!("Writing to address 0xFF75, value = 0b{value:0>8b}");
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

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct TimerRegisters {
    /// ADDR FF04
    divider_reg: u8,
    // The divider reg is incremented every 64 machine cycles. This tracks those cycles. When the
    // divider is reset, this counter is reset.
    divider_counter: u8,
    /// ADDR FF05
    /// The counter that is updated at the frequency specified by the TAC. Overflows trigger resets
    /// to the value in the timer modulo and then an interupt is requested.
    timer_counter: u8,
    /// ADDR FF06
    /// When the timer counter overflows, it resets to the value in this register.
    timer_modulo: u8,
    /// ADDR FF07
    timer_control: TimerControl,
}

/// Models the timer control (TAC) register, which controls how frequently the timer counter is
/// incremented. There is a variant for each frequency and the disabled state.
///
/// All the variants besides `Disabled` hold a counter that tracks the number of ticks since the
/// last increment.
/// The data in `Disabled` tracks the data that was written to the register when disabled. Without
/// it, the register can not be properly read as part of the memory map.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub enum TimerControl {
    Disabled(u8),
    /// Ticks once per 256 M-cycles
    Slowest(u8),
    /// Ticks once per 64 M-cycles
    Slow(u8),
    /// Ticks once per 16 M-cycles
    Fast(u8),
    /// Ticks once per 4 M-cycles
    Fastest(u8),
}

impl TimerRegisters {
    fn new() -> Self {
        Self {
            divider_reg: 0,
            divider_counter: 0,
            timer_counter: 0,
            timer_modulo: 0,
            timer_control: TimerControl::Disabled(0),
        }
    }

    fn tick(&mut self) -> bool {
        self.divider_counter += 1;
        if self.divider_counter == 64 {
            self.divider_counter = 0;
            self.divider_reg = self.divider_reg.wrapping_add(1);
        }
        let inc = match &mut self.timer_control {
            TimerControl::Disabled(_) => false,
            TimerControl::Slowest(counter) => {
                *counter = counter.wrapping_add(1);
                *counter == 0
            }
            TimerControl::Slow(counter) => {
                *counter += 1;
                let digest = *counter == 64;
                if digest {
                    *counter = 0;
                }
                digest
            }
            TimerControl::Fast(counter) => {
                *counter += 1;
                let digest = *counter == 16;
                if digest {
                    *counter = 0;
                }
                digest
            }
            TimerControl::Fastest(counter) => {
                *counter += 1;
                let digest = *counter == 4;
                if digest {
                    *counter = 0;
                }
                digest
            }
        };
        let val = self.timer_counter.checked_add(inc as u8);
        // `None` indicates ther was an overflow, so we need to request an interupt
        self.timer_counter = val.unwrap_or(self.timer_modulo);
        val.is_none()
    }

    // FIXME: IoRegisters needs to call this method when STOP is called.
    // NOTE: This method is also called when the STOP instruction is called.
    fn reset_divider(&mut self) {
        self.divider_reg = 0;
        self.divider_counter = 0;
    }

    fn read_byte(&self, index: u16) -> u8 {
        match index {
            0xFF04 => self.divider_reg,
            0xFF05 => self.timer_counter,
            0xFF06 => self.timer_modulo,
            0xFF07 => self.timer_control.as_byte(),
            _ => unreachable!("How did you get here??"),
        }
    }

    fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0xFF04 => self.reset_divider(),
            0xFF05 => self.timer_counter = value,
            0xFF06 => self.timer_modulo = value,
            0xFF07 => self.timer_control = TimerControl::from_byte(value),
            _ => unreachable!("How did you get here??"),
        }
    }
}

impl TimerControl {
    fn from_byte(byte: u8) -> Self {
        // We only care about the first 3 bits.
        match byte & 0b0000_0111 {
            0b000 => Self::Slowest(0),
            0b001 => Self::Fastest(0),
            0b010 => Self::Fast(0),
            0b011 => Self::Slow(0),
            byte => Self::Disabled(byte),
        }
    }

    fn as_byte(&self) -> u8 {
        match self {
            Self::Slowest(_) => 0b000,
            Self::Fastest(_) => 0b001,
            Self::Fast(_) => 0b010,
            Self::Slow(_) => 0b011,
            Self::Disabled(byte) => *byte,
        }
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
    use std::u8;

    use crate::mem::io::TimerControl;
    use crate::mem::io::TimerRegisters;

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
            println!("{}", palettes.index);
            println!("{:?}", palettes.data[0]);
            let data = &mut palettes[1];
            assert_eq!(i as u8, *data);
        }
    }

    #[test]
    fn divider_register() {
        let mut regs = TimerRegisters::new();
        assert!((0..64).all(|_| !regs.tick()));
        assert_eq!(regs.divider_reg, 1);
        assert_eq!(regs.divider_counter, 0);
        let mut regs = TimerRegisters::new();
        // We want to tick right up until the register resets. At no point should there be an
        // interupt request since the timer counter is disabled
        let digest = std::iter::repeat_n(0..64u8, u8::MAX as usize)
            .chain(std::iter::once(0..63u8))
            .flatten()
            .all(|_| !regs.tick());
        assert!(digest);
        assert_eq!(regs.divider_reg, 0xFF);
        assert_eq!(regs.divider_counter, 63);
        assert!(!regs.tick());
        assert_eq!(regs.divider_reg, 0);
        assert_eq!(regs.divider_counter, 0);
    }

    #[test]
    fn timer_registers_slowest() {
        const TICKS: usize = 0x100;
        let mut regs = TimerRegisters::new();
        regs.timer_control = TimerControl::Slowest(0);
        regs.timer_modulo = 10;
        // Initial tick
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slowest(1));

        // Tick up until counter increments
        assert!((0..TICKS - 2).all(|_| !regs.tick()));
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slowest((TICKS - 1) as u8));

        // Tick and the counter should inc
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 1);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slowest(0));

        // Tick up until the counter is triggered
        let mut regs = TimerRegisters::new();
        regs.timer_modulo = 10;
        regs.timer_control = TimerControl::Slowest(0);
        let digest = std::iter::repeat_n(0..0x100, 0xFF)
            .flatten()
            .chain(0..TICKS - 1)
            .all(|_| !regs.tick());
        assert!(digest);
        assert_eq!(regs.timer_counter, (TICKS - 1) as u8);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slowest((TICKS - 1) as u8));

        // Tick and verify the reset
        assert!(regs.tick());
        assert_eq!(regs.timer_counter, 10);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slowest(0));
    }

    #[test]
    fn timer_registers_slow() {
        const TICKS: u8 = 64;
        let mut regs = TimerRegisters::new();
        regs.timer_modulo = 10;
        regs.timer_control = TimerControl::Slow(0);
        // Initial tick
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slow(1));

        // Tick up until counter increments
        assert!((0..TICKS - 2).all(|_| !regs.tick()));
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slow(TICKS - 1));

        // Tick and the counter should inc
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 1);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slow(0));

        // Tick up until the counter is triggered
        let mut regs = TimerRegisters::new();
        regs.timer_modulo = 10;
        regs.timer_control = TimerControl::Slow(0);
        let digest = std::iter::repeat_n(0..TICKS, 0xFF)
            .flatten()
            .chain(0..TICKS - 1)
            .all(|_| !regs.tick());
        assert!(digest);
        assert_eq!(regs.timer_counter, 0xFF);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slow(TICKS - 1));

        // Tick and verify the reset
        assert!(regs.tick());
        assert_eq!(regs.timer_counter, 10);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Slow(0));
    }

    #[test]
    fn timer_registers_fast() {
        const TICKS: u8 = 16;
        let mut regs = TimerRegisters::new();
        regs.timer_modulo = 10;
        regs.timer_control = TimerControl::Fast(0);
        // Initial tick
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fast(1));

        // Tick up until counter increments
        assert!((0..TICKS - 2).all(|_| !regs.tick()));
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fast(TICKS - 1));

        // Tick and the counter should inc
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 1);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fast(0));

        // Tick up until the counter is triggered
        let mut regs = TimerRegisters::new();
        regs.timer_control = TimerControl::Fast(0);
        regs.timer_modulo = 10;
        let digest = std::iter::repeat_n(0..TICKS, 0xFF)
            .flatten()
            .chain(0..TICKS - 1)
            .all(|_| !regs.tick());
        assert!(digest);
        assert_eq!(regs.timer_counter, 0xFF);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fast(TICKS - 1));

        // Tick and verify the reset
        assert!(regs.tick());
        assert_eq!(regs.timer_counter, 10);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fast(0));
    }

    #[test]
    fn timer_registers_fastest() {
        const TICKS: u8 = 4;
        let mut regs = TimerRegisters::new();
        regs.timer_modulo = 10;
        regs.timer_control = TimerControl::Fastest(0);
        // Initial tick
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fastest(1));

        // Tick up until counter increments
        assert!((0..TICKS - 2).all(|_| !regs.tick()));
        assert_eq!(regs.timer_counter, 0);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fastest(TICKS - 1));

        // Tick and the counter should inc
        assert!(!regs.tick());
        assert_eq!(regs.timer_counter, 1);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fastest(0));

        // Tick up until the counter is triggered
        let mut regs = TimerRegisters::new();
        regs.timer_control = TimerControl::Fastest(0);
        regs.timer_modulo = 10;
        let digest = std::iter::repeat_n(0..TICKS, 0xFF)
            .flatten()
            .chain(0..TICKS - 1)
            .all(|_| !regs.tick());
        assert!(digest);
        assert_eq!(regs.timer_counter, 0xFF);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fastest(TICKS - 1));

        // Tick and verify the reset
        assert!(regs.tick());
        assert_eq!(regs.timer_counter, 10);
        assert_eq!(regs.timer_modulo, 10);
        assert_eq!(regs.timer_control, TimerControl::Fastest(0));
    }
}
