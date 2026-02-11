use std::ops::Index;
use std::ops::IndexMut;

use serde::Deserialize;
use serde::Serialize;
use tracing::debug;
use tracing::trace;

use crate::ButtonInput;
use crate::cpu::check_bit_const;
use crate::instruction::InterruptOp;
use crate::ppu::Pixel;
use crate::utils::Wrapping;

use super::MemoryMap;
use super::vram::PpuMode;
use super::vram::VRam;

pub mod timers;

use timers::TimerRegisters;

/// Used to write a new byte into an existing byte where one or more of the bits are read-only.
/// The `mask` should have writable bits set and read-only bits unset. E.g. if the bottom input is
/// read-only, the mask should be 0xF0.
pub fn selective_write(existing: &mut u8, mask: u8, new: u8) {
    let masked_existing = *existing & (!mask);
    let masked_new = new & mask;
    *existing = masked_existing | masked_new;
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct IoRegisters {
    /// ADDR FF00
    pub(super) joypad: u8,
    /// ADDR FF01, FF02
    // FIXME: Not impl-ed at all
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
    pub lcd_status: u8,
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
    /// ADDR FF4C
    /// CPU mode selection. Only bit 2 is used. If that bit is set, the system is ran in DMG comp
    /// mode. Locked after boot.
    pub cpu_mode: u8,
    /// ADDR FF4D
    pub speed_switch: u8,
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
    undoc_registers: [u8; 4],
    /// There are gaps amount the memory mapped IO registers. Any index into this that hits one of
    /// these gaps resets the value. Notably, this is also used when mutably indexing to the
    /// divider register but not while immutably indexing.
    /// ADDR FF03, FF08-FF0E, FF27-FF29, FF4E, FF56-FF67, FF6C-FF6F
    dead_byte: u8,
}

impl Default for IoRegisters {
    fn default() -> Self {
        Self {
            undoc_registers: [0, 0, 0xFF, 0b1000_1111],
            joypad: 0xFF,
            serial: (0x00, 0x7E),
            tac: TimerRegisters::new(),
            interrupt_flags: Default::default(),
            audio: Default::default(),
            lcd_control: Default::default(),
            lcd_status: Default::default(),
            bg_position: Default::default(),
            lcd_y: Default::default(),
            lcd_cmp: Default::default(),
            monochrome_bg_palette: Default::default(),
            monochrome_obj_palettes: [0xFF; 2],
            window_position: Default::default(),
            vram_select: Default::default(),
            boot_status: Default::default(),
            background_palettes: Default::default(),
            object_palettes: Default::default(),
            dead_byte: Default::default(),
            boot_status_disabled: false,
            cpu_mode: Default::default(),
            speed_switch: Default::default(),
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
            0xFF14 => self.ch1_period_and_control & 0b0010_0000,
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
            n @ 0xFF30..=0xFF3F => self.ch3_wave_form[(n - 0xFF30) as usize],
            /* Channel 4 */
            // NOTE: self.ch4_length_timer is write-only
            0xFF20 => 0xFF,
            0xFF21 => self.ch4_vol_and_env,
            0xFF22 => self.ch4_freq_and_rand,
            // NOTE: Only bit 6 is readable
            0xFF23 => self.ch4_control & 0b0100_0000,
            /* Oops... */
            idx => {
                tracing::error!("There was an attemped read from an unused byte 0x{idx:0>4X}");
                0xFF
            }
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
            idx => {
                tracing::error!("There was an attemped write to an unused byte @ 0x{idx:0>4x}");
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
        if self.tac.tick() {
            self.request_timer_int();
        }
    }

    pub(crate) fn disable_lcd(&mut self) {
        println!("Disabling LCD");
        self.lcd_status &= 0b1111_1100;
        self.lcd_y = 0;
        if self.lcd_y == self.lcd_cmp {
            self.lcd_status |= 0b100;
        } else {
            // Reset the LYC = LY bit
            self.lcd_status &= 0b1111_1011;
        }
    }

    pub(crate) fn enable_lcd(&mut self) {
        println!("Enabling LCD");
        self.set_ppu_status(PpuMode::OamScan);
    }

    pub(crate) fn set_ppu_status(&mut self, state: PpuMode) {
        let old_state = match self.lcd_status & 0b11 {
            2 => PpuMode::OamScan,
            3 => PpuMode::Drawing,
            0 => PpuMode::HBlank,
            1 => PpuMode::VBlank,
            _ => panic!(),
        };
        tracing::warn!(
            "Updating PPU status from {old_state:?} ({}) to {state:?} ({})",
            old_state as u8,
            state as u8
        );
        tracing::warn!("Inital status register value: 0x{:0>2X}", self.lcd_status);
        // Clear the bottom two bits
        self.lcd_status &= 0b1111_1100;
        // Update the bottom two bits to be the state
        self.lcd_status |= state as u8;
        tracing::warn!("Updated status register value: 0x{:0>2X}", self.lcd_status);
    }

    /// Called by the PPU when it finishes a scan line
    pub(crate) fn inc_lcd_y(&mut self) {
        self.lcd_y = (self.lcd_y + 1) % 154;
        if self.lcd_y == self.lcd_cmp {
            self.lcd_status |= 0b100;
            self.request_lcd_int();
        } else {
            // Reset the LYC = LY bit
            self.lcd_status &= 0b1111_1011;
        }
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

    pub fn register_button_input(&mut self, input: ButtonInput) {
        let byte = match input {
            ButtonInput::Joypad(button) if !check_bit_const::<4>(self.joypad) => button as u8,
            ButtonInput::Ssab(button) if !check_bit_const::<5>(self.joypad) => button as u8,
            _ => return,
        };
        self.joypad &= !byte;
        // self.io.interrupt_flags |= self.ie & 0b1_0000;
        self.interrupt_flags |= 0b1_0000;
    }

    pub fn register_button_release(&mut self, input: ButtonInput) {
        let byte = match input {
            ButtonInput::Joypad(button) if !check_bit_const::<4>(self.joypad) => button as u8,
            ButtonInput::Ssab(button) if !check_bit_const::<5>(self.joypad) => button as u8,
            _ => return,
        };
        self.joypad |= byte;
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

    /// Updates the LYC == LY bit in the LCD Stat register
    fn check_y_cmp(&mut self) {
        if self.lcd_cmp == self.lcd_y {
            self.lcd_status |= 0b100;
        } else {
            self.lcd_status &= 0b1111_1011;
        }
    }

    pub(crate) fn read_byte(&self, index: u16) -> u8 {
        match index {
            0xFF00 => self.joypad,
            0xFF01 => self.serial.0,
            0xFF02 => self.serial.1,
            0xFF04..=0xFF07 => self.tac.read_byte(index),
            0xFF0F => 0xE0 | self.interrupt_flags,
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
            // This can not be directly accessed
            0xFF4C => 0xFF,
            0xFF4D => self.speed_switch,
            // When reading from this register, all bits expect the first are 1
            0xFF4F => 0xFE | self.vram_select,
            0xFF50 => {
                if self.boot_status_disabled {
                    0xFF
                } else {
                    self.boot_status
                }
            }
            0xFF68 => self.background_palettes.read_index(),
            0xFF69 => self.background_palettes.read_palette_byte(),
            0xFF6A => self.object_palettes.read_index(),
            0xFF6B => self.object_palettes.read_palette_byte(),
            0xFF72 => self.undoc_registers[0],
            0xFF73 => self.undoc_registers[1],
            0xFF74 => self.undoc_registers[2],
            0xFF75 => self.undoc_registers[3],
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF6F
            | 0xFF71
            | 0xFF76.. => 0xFF,
            ..=0xFEFF | 0xFF51..=0xFF55 | 0xFF70 | 0xFF46 => unreachable!(
                "The MemoryMap should never index into the IO registers outside of 0xFF00-0xFF80! Got index 0x{index:0>4x}"
            ),
        }
    }

    pub(crate) fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0xFF00 => selective_write(&mut self.joypad, 0b0011_0000, value),
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
            0xFF40 => {
                println!(
                    "Changing LCD Control register from 0x{:0>2X} to 0x{value:0>2X}",
                    self.lcd_control
                );
                let was_on = check_bit_const::<7>(self.lcd_control);
                self.lcd_control = value;
                let is_on = check_bit_const::<7>(self.lcd_control);
                match (was_on, is_on) {
                    (true, false) => self.disable_lcd(),
                    (false, true) => self.enable_lcd(),
                    _ => {}
                }
            }
            // TODO: Only part of this register can be written to. Only bits 3-6 can be written to.
            // This register needs to be reset when ticked.
            0xFF41 => {
                println!(
                    "Changing LCD Stat register from 0x{:0>2X} to 0x{value:0>2X}",
                    self.lcd_status
                );
                self.lcd_status = 0b1000_0000 | (0b0111_1000 & value);
                self.check_y_cmp();
            }
            0xFF42 => self.bg_position.0 = value,
            0xFF43 => self.bg_position.1 = value,
            0xFF44 => {}
            0xFF45 => {
                self.lcd_cmp = value;
                self.check_y_cmp();
            }
            0xFF47 => self.monochrome_bg_palette = value,
            0xFF48 => self.monochrome_obj_palettes[0] = value,
            0xFF49 => self.monochrome_obj_palettes[1] = value,
            0xFF4A => self.window_position[0] = value,
            0xFF4B => self.window_position[1] = value,
            0xFF4C => self.cpu_mode = value,
            0xFF4D => self.speed_switch = value,
            // Ignore all but the first bit of the value
            0xFF4F => self.vram_select = 1 & value,
            0xFF50 => {
                self.boot_status_disabled = true;
                self.boot_status = value
            }
            0xFF68 => {
                tracing::warn!("Writing to background palettes index: 0x{value:0>2X}");
                self.background_palettes.write_index(value);
            }
            0xFF69 => {
                tracing::warn!("Writing to background palettes: 0x{value:0>2X}");
                self.background_palettes.write_palette_byte(value);
            }
            0xFF6A => {
                tracing::warn!("Writing to object palettes index: 0x{value:0>2X}");
                self.object_palettes.write_index(value);
            }
            0xFF6B => {
                tracing::warn!("Writing to object palettes: 0x{value:0>2X}");
                self.object_palettes.write_palette_byte(value);
            }
            0xFF72 => self.undoc_registers[0] = value,
            0xFF73 => self.undoc_registers[1] = value,
            0xFF74 => self.undoc_registers[2] = value,
            0xFF75 => {
                self.undoc_registers[3] = value | 0b1000_1111;
            }
            0xFF03
            | 0xFF08..=0xFF0E
            | 0xFF27..=0xFF2F
            | 0xFF4E
            | 0xFF56..=0xFF67
            | 0xFF6C..=0xFF6F
            | 0xFF71
            | 0xFF76.. => {}
            ..=0xFEFF | 0xFF51..=0xFF55 | 0xFF70 | 0xFF46 | 0xFF80.. => unreachable!(
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
        &self.background_palettes.data[index as usize]
    }
}

impl Index<ObjPaletteIndex> for IoRegisters {
    type Output = Palette;

    fn index(&self, ObjPaletteIndex(index): ObjPaletteIndex) -> &Self::Output {
        &self.object_palettes.data[index as usize]
    }
}

// Notes:
// FF69 — BCPD/BGPD can not be accessed during OAM Scan and Drawing

/// In GBC mode, there are extra palettes for the colors
#[derive(Debug, Clone, Hash, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct ColorPalettes {
    pub(crate) index: Wrapping<u8>,
    /// This array is indexed into by the index field.
    pub(crate) data: [Palette; 8],
}

impl ColorPalettes {
    /// Palette data is stored as an array of 4-color arrays where each color is 2 bytes.
    /// This method returns the index into the top array and middle array
    pub(crate) fn indices(&self) -> (u8, u8) {
        let index = self.index.0 & 0b0011_1111;
        ((index & 0b0011_1000) >> 3, index & 0b111)
    }

    /// Reads from the "index" (aka "address") register
    pub(crate) fn read_index(&self) -> u8 {
        self.index.0
    }

    /// Writes to the "index" (aka "address") register
    pub(crate) fn write_index(&mut self, value: u8) {
        self.index.0 = value & 0b1011_1111;
    }

    /// Using the address register, indexes into the palette data and returns the indexed byte
    pub(crate) fn read_palette_byte(&self) -> u8 {
        let (a, b) = self.indices();
        println!("A = 0x{a:0>X}, B = 0x{b:0>X}");
        self.data[a as usize].read_byte(b)
    }

    /// Using the address register, indexes into the palette data writes over that byte
    pub(crate) fn write_palette_byte(&mut self, value: u8) {
        let (a, b) = self.indices();
        self.data[a as usize].write_byte(b, value);
        if check_bit_const::<7>(self.index.0) {
            self.index.0 = (self.index.0 + 1) & 0b1011_1111;
        }
    }
}

/// All of the date for one of the 8 palettes that can be held in memory.
#[derive(Debug, Clone, Hash, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct Palette {
    pub(crate) colors: [PaletteColor; 4],
}

impl Palette {
    /// A palette contains an array of 4 two-byte arrays. The give index should be between 0-7 and
    /// acts as the index for both the inner and outer arrays.
    pub(crate) fn read_byte(&self, index: u8) -> u8 {
        let outer = (index >> 1) as usize;
        let inner = (index & 1) as usize;
        println!("index = 0b{index:0>3b}, Outer = 0b{outer:0>3b}, Inner = 0b{inner:0>3b}");
        self.colors[outer].0[inner]
    }

    pub(crate) fn write_byte(&mut self, index: u8, mut value: u8) {
        let outer = (index >> 1) as usize;
        let inner = (index & 1) as usize;
        // The top bit of the set byte is ignored. If read, that bit should be read as set.
        if inner == 1 {
            value |= 0x80;
        }
        self.colors[outer].0[inner] = value;
    }

    pub fn get_color(&self, index: u8) -> PaletteColor {
        self.colors[index as usize]
    }
}

/// The colors inside a palette are a bit odd. Each color takes up two bytes and represents each
/// color with 5 bits (in little-endian). The top bit is not used.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct PaletteColor(pub [u8; 2]);

impl PaletteColor {
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

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use crate::ButtonInput;
    use crate::JoypadInput;
    use crate::SsabInput;

    use super::ColorPalettes;
    use super::IoRegisters;
    use super::Palette;
    use super::PaletteColor;

    /// Tests that the internal indexing logic for reading and writing palette data works as
    /// expected. Since this logic is only used by the CPU (via read/write_byte), only byte-level
    /// (not color-level) indexing is tested.
    ///  - Indexes into the expected palette byte
    ///  - When the top bit of the index is set, writing to a byte increments the index
    ///  - Indexes wrap around
    #[test]
    fn indexing_into_palettes() {
        // Palette data will be values 0..64, in order.
        let mut iter = 0..64u8;
        // 8, 4, 2
        let mut incrementing_palettes = ColorPalettes {
            index: 0x80.into(),
            data: std::array::from_fn(|_| Palette {
                colors: std::array::from_fn(|_| {
                    PaletteColor([iter.next().unwrap(), iter.next().unwrap()])
                }),
            }),
        };
        println!("{incrementing_palettes:#?}");
        let mut non_incrementing_palettes = incrementing_palettes.clone();
        non_incrementing_palettes.index = 0.into();
        // Indexing tests
        for i in 0..64u8 {
            {
                let byte = incrementing_palettes.read_palette_byte();
                println!("Inc    :  byte = 0x{byte:0>2X}, i = 0x{i:0>2X}");
                assert_eq!(byte, i);
                incrementing_palettes.write_palette_byte(0x08 + i);
                let index = incrementing_palettes.read_index();
                println!("Inc    : index = 0x{index:0>2X}, i = 0x{i:0>2X}");
                // If the index was the 64th byte, it will wrap around
                let expected = 0x80 + (i + 1) % 64;
                assert_eq!(index, expected);
            }

            {
                let byte = non_incrementing_palettes.read_palette_byte();
                println!("Non-inc:  byte = 0x{byte:0>2X}, i = 0x{i:0>2X}");
                assert_eq!(byte, i);
                non_incrementing_palettes.write_palette_byte(0x08 + i);
                let index = non_incrementing_palettes.read_index();
                println!("Non-inc: index = 0x{index:0>2X}, i = 0x{i:0>2X}");
                assert_eq!(index, i);
                non_incrementing_palettes.write_index(i + 1);
                let index = non_incrementing_palettes.read_index();
                let expected = (i + 1) % 64;
                assert_eq!(index, expected);
            }
            println!();
        }
        incrementing_palettes.index = 0x80.into();
        non_incrementing_palettes.index = 0.into();
        println!("{incrementing_palettes:#?}");
        // Test that writes in prior loop worked
        for i in 0..64u8 {
            {
                let byte = incrementing_palettes.read_palette_byte();
                println!("Inc    :  byte = 0x{byte:0>2X}, i = 0x{i:0>2X}");
                let expected = if i % 2 == 0 { i } else { 0x80 | i };
                assert_eq!(byte, 0x08 + expected);
                incrementing_palettes.write_palette_byte(i);
                let index = incrementing_palettes.read_index();
                println!("Inc    : index = 0x{index:0>2X}, i = 0x{i:0>2X}");
                // If the index was the 64th byte, it will wrap around
                let expected = 0x80 + (i + 1) % 64;
                assert_eq!(index, expected);
            }

            {
                let byte = non_incrementing_palettes.read_palette_byte();
                println!("Non-inc:  byte = 0x{byte:0>2X}, i = 0x{i:0>2X}");
                let expected = if i % 2 == 0 { i } else { 0x80 | i };
                assert_eq!(byte, 0x08 + expected);
                non_incrementing_palettes.write_palette_byte(i + 1);
                let index = non_incrementing_palettes.read_index();
                println!("Non-inc: index = 0x{index:0>2X}, i = 0x{i:0>2X}");
                assert_eq!(index, i);
                non_incrementing_palettes.write_index(i + 1);
                let index = non_incrementing_palettes.read_index();
                let expected = (i + 1) % 64;
                assert_eq!(index, expected);
            }
            println!();
        }
    }

    // Tests the various traits of the joypad.
    //  - The bottom nibble is read only
    //  - The default position of the bottom 4 bits is 1
    //  - Pressing an allowed button should switch the cooresponding bit to 0
    //  - "mode" selection (for bits 4 and 5) works similarly, 0 == selected
    //   - If no mode is selected, the bottom nibble is read as 0xF
    //  - The top two bits are ignored.
    #[test]
    fn joypad_tests() {
        fn button_iter() -> impl Iterator<Item = ButtonInput> {
            JoypadInput::iter()
                .map(ButtonInput::Joypad)
                .chain(SsabInput::iter().map(ButtonInput::Ssab))
        }

        let mut io = IoRegisters::default();
        // Initially, no input mode should be selected
        assert_eq!(io.read_byte(0xFF00), 0xFF);
        // Top two bits should be unaffected, both modes should be on, bottom nibble should be
        // unaffected
        io.write_byte(0xFF00, 0);
        assert_eq!(io.read_byte(0xFF00), 0xCF);

        // Select no modes and ensure that no button press is registered
        io.write_byte(0xFF00, 0xFF);
        assert_eq!(io.read_byte(0xFF00), 0xFF);
        for input in button_iter() {
            io.register_button_input(input);
            assert_eq!(io.read_byte(0xFF00), 0xFF);
            io.register_button_release(input);
        }
        println!();

        // Select only Joypad modes and ensure that only those button presses are registered
        println!("Testing joypad only...");
        io.write_byte(0xFF00, 0xEF);
        assert_eq!(io.read_byte(0xFF00), 0xEF);
        for input in button_iter() {
            println!("Testing input: {input:?}");
            io.register_button_input(input);
            let expected = match input {
                ButtonInput::Joypad(input) => 0x0F & !(input as u8),
                ButtonInput::Ssab(_) => 0x0F,
            };
            assert_eq!(io.read_byte(0xFF00), 0xE0 | expected,);
            // Ensure that, when other buttons are pressed, it leaves this button unaffected
            for other in button_iter().filter(|other| *other != input) {
                io.register_button_input(other);
                let inner_expected = match other {
                    ButtonInput::Joypad(input) => 0x0F & !(input as u8),
                    ButtonInput::Ssab(_) => 0x0F,
                };
                assert_eq!(io.read_byte(0xFF00), 0xE0 | (inner_expected & expected));
                io.register_button_release(other);
                assert_eq!(io.read_byte(0xFF00), 0xE0 | expected);
            }
            io.register_button_release(input);
        }
        println!();

        // Select only SSAB modes and ensure that only those button presses are registered
        println!("Testing SSAB only...");
        io.write_byte(0xFF00, 0xDF);
        assert_eq!(io.read_byte(0xFF00), 0xDF);
        for input in button_iter() {
            println!("Testing input: {input:?}");
            io.register_button_input(input);
            let expected = match input {
                ButtonInput::Ssab(input) => 0x0F & !(input as u8),
                ButtonInput::Joypad(_) => 0x0F,
            };
            assert_eq!(io.read_byte(0xFF00), 0xD0 | expected);
            for other in button_iter().filter(|other| *other != input) {
                io.register_button_input(other);
                let inner_expected = match other {
                    ButtonInput::Ssab(input) => 0x0F & !(input as u8),
                    ButtonInput::Joypad(_) => 0x0F,
                };
                assert_eq!(
                    io.read_byte(0xFF00),
                    0xD0 | (inner_expected & expected),
                    "Actual: 0x{:0>2X}, Expected: 0x{:0>2X}",
                    io.read_byte(0xFF00),
                    0xD0 | (inner_expected & expected)
                );
                io.register_button_release(other);
                assert_eq!(io.read_byte(0xFF00), 0xD0 | expected);
            }
            io.register_button_release(input);
        }
        println!();

        // Select both modes and ensure that all button presses are registered
        println!("Testing both modes...");
        io.write_byte(0xFF00, 0xCF);
        assert_eq!(io.read_byte(0xFF00), 0xCF);
        for input in button_iter() {
            println!("Testing input: {input:?}");
            io.register_button_input(input);
            let expected = match input {
                ButtonInput::Ssab(input) => 0x0F & !(input as u8),
                ButtonInput::Joypad(input) => 0x0F & !(input as u8),
            };
            assert_eq!(io.read_byte(0xFF00), 0xC0 | expected);
            io.register_button_release(input);
        }
        println!();
    }
}
