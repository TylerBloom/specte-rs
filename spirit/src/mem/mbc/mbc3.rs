use std::{
    array,
    ops::{Index, IndexMut},
};

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum ClockLatchState {
    /// State is reached by writing 0x00 to 0x6000..0x8000
    Primed,
    /// State is reached by writing any value other than 0x00 to 0x6000..0x8000
    Unprimed,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum RamAndClockIndex {
    Ram(u8),
    Clock(ClockIndex),
}

/// Used to index into the clock data. Note that each value maps to its index into the data array
/// not to the actual value that is written to the register in order to select the value.
#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(usize)]
enum ClockIndex {
    Seconds = 0x00,
    Minutes = 0x01,
    Hours = 0x02,
    DayLower = 0x03,
    DayUpper = 0x04,
}

// TODO: How should clocks be handled...
#[derive(Debug, Hash, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MBC3 {
    ram_and_timer_enable: bool,
    /// The raw value for this register needs to be stored rather than just the bool because
    /// operations need to be able to operate on it.
    raw_enable_reg: u8,
    rom_bank_zero: Vec<u8>, // Box<[u8; 0x4000]>,
    rom: Vec<Vec<u8>>,
    rom_bank: u8,
    ram_clock_index: RamAndClockIndex,
    ram: [Vec<u8>; 4],
    clock_data: [u8; 5],
    latch_state: ClockLatchState,
    last_latch: DateTime<Utc>,
}

impl MBC3 {
    pub fn new(rom_count: usize, ram_count: usize, cart: &[u8]) -> Self {
        println!(
            "Expecting {rom_count} many ROM banks, requiring {} many bytes from {} many bytes",
            rom_count * 0x4000,
            cart.len()
        );
        let rom_bank_zero = cart[0..0x4000].to_owned();
        let rom = (1..rom_count)
            .map(|i| i * 0x4000)
            .map(|i| cart[i..i + 0x4000].to_owned())
            .collect();
        Self {
            ram_and_timer_enable: false,
            raw_enable_reg: 0,
            rom_bank_zero,
            rom,
            rom_bank: 0,
            ram_clock_index: RamAndClockIndex::Ram(0),
            ram: array::from_fn(|_| vec![0; 0x2000]),
            clock_data: [0; 5],
            latch_state: ClockLatchState::Unprimed,
            last_latch: Utc::now(),
        }
    }

    pub(super) fn overwrite_rom_zero(&mut self, index: u16, val: &mut u8) {
        std::mem::swap(&mut self.rom_bank_zero[index as usize], val)
    }

    pub(super) fn read_byte(&self, index: u16) -> u8 {
        match index {
            0x0000..0x4000 => self.rom_bank_zero[index as usize],
            index @ 0x4000..0x8000 => {
                // println!("Reading from ROM BANK {} @ 0x{index:0>4X}", self.rom_bank + 1);
                self.rom[self.rom_bank as usize][(index - 0x4000) as usize]
            }
            0x8000..0xA000 => unreachable!("How did you get here??"),
            index @ 0xA000..0xC000 => {
                let digest = match self.ram_clock_index {
                    RamAndClockIndex::Ram(bank) => {
                        println!("Reading from MBC3 RAM bank {bank} at 0x{index:0>4X}");
                        self.ram[bank as usize][(index - 0xA000) as usize]
                    }
                    RamAndClockIndex::Clock(index) => {
                        println!("Reading from MBC3 clock at {index:?}");
                        self.clock_data[index as usize]
                    }
                };
                (!(!self.ram_and_timer_enable as u8)) & digest
            }
            0xC000.. => unreachable!("How did you get here??"),
        }
    }

    fn sync_enablement(&mut self) {
        if self.raw_enable_reg == 0 {
            println!("Disabling RAM/clock");
            self.ram_and_timer_enable = false;
        } else if self.raw_enable_reg == 0x0A {
            println!("Enabling RAM/clock");
            self.ram_and_timer_enable = true
        }
    }

    pub(super) fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0x0000..0x2000 => {
                self.raw_enable_reg = value;
                self.sync_enablement();
            }
            0x2000..0x4000 => {
                // We ignore the top bit. If 0 if written in, we treat it as 1.
                // We then subtract one in order to use `rom_bank` directly as an index. This is
                // fine because it is never directly read via indexing.
                self.rom_bank = std::cmp::max(1, value & 0x7F) - 1;
            }
            0x4000..0x6000 => {
                match value {
                    0..4 => self.ram_clock_index = RamAndClockIndex::Ram(value),
                    0x8 => self.ram_clock_index = RamAndClockIndex::Clock(ClockIndex::Seconds),
                    0x9 => self.ram_clock_index = RamAndClockIndex::Clock(ClockIndex::Minutes),
                    0xA => self.ram_clock_index = RamAndClockIndex::Clock(ClockIndex::Hours),
                    0xB => self.ram_clock_index = RamAndClockIndex::Clock(ClockIndex::DayLower),
                    0xC => self.ram_clock_index = RamAndClockIndex::Clock(ClockIndex::DayUpper),
                    // NOTE: It is unclear to me what happen if anything else is written into this
                    // register. For now, we are just going to ignore it.
                    _ => {}
                }
                println!("Set RAM/clock index to {:?}", self.ram_clock_index);
            }
            0x6000..0x8000 => {
                if value == 0 {
                    println!("Priming clock latch...");
                    self.latch_state = ClockLatchState::Primed;
                } else {
                    if value == 1 && matches!(self.latch_state, ClockLatchState::Primed) {
                        self.latch_clock();
                    }
                    self.latch_state = ClockLatchState::Unprimed;
                }
            }
            0x8000..0xA000 => unreachable!("How did you get here??"),
            0xA000..0xC000 => {
                if !self.ram_and_timer_enable {
                    println!("Not enabled...");
                    return;
                }
                match self.ram_clock_index {
                    RamAndClockIndex::Ram(bank) => {
                        self.ram[bank as usize][(index - 0xA000) as usize] = value
                    }
                    RamAndClockIndex::Clock(index) => {
                        println!("Writing to clock: {value}");
                        self.clock_data[index as usize] = value
                    }
                }
            }
            0xC000.. => unreachable!("How did you get here??"),
        }
    }

    fn latch_clock(&mut self) {
        println!("Latching time: Init clock data {:?}", self.clock_data);
        let now = Utc::now();
        let elapsed = (now - self.last_latch).to_std().unwrap();
        if elapsed.as_secs() == 0 {
            return;
        }
        self.last_latch = now;
        let secs = self.clock_data[0] + ((elapsed.as_secs() % 60) as u8);
        self.clock_data[0] = secs % 60;
        let carry = secs / 60;

        let mins = self.clock_data[1] + carry + (((elapsed.as_secs() / 60) % 60) as u8);
        let carry = mins / 60;
        self.clock_data[1] = mins % 60;

        let hours = self.clock_data[2] + carry + (((elapsed.as_secs() / 3600) % 24) as u8);
        let carry = hours / 24;
        self.clock_data[2] = carry % 24;

        let days = self.clock_data[3] as u16
            + carry as u16
            + (((elapsed.as_secs() / 86_400) % 0x2FF) as u16);
        let [hi, lo] = days.to_be_bytes();
        self.clock_data[3] = lo;
        println!("Updated clock data {:?}", self.clock_data);
        // TODO: Handle the high bits...
    }

    #[track_caller]
    pub(super) fn update_byte(&mut self, index: u16, update: impl FnOnce(&mut u8)) -> u8 {
        match index {
            0x0000..0x2000 => {
                update(&mut self.raw_enable_reg);
                self.sync_enablement();
                return self.raw_enable_reg;
            }
            _ => return 0,
            /*
            index @ 0x4000..0x8000 => self.rom[self.rom_bank as usize][(index - 0x4000) as usize],
            0x8000..0xA000 => unreachable!("How did you get here??"),
            index @ 0xA000..0xC000 => {
                let digest = match self.ram_clock_index {
                    RamAndClockIndex::Ram(bank) => {
                        self.ram[bank as usize][(index - 0xA000) as usize]
                    }
                    RamAndClockIndex::Clock(index) => self.clock_data[index as usize],
                };
                (!(!self.ram_and_timer_enable as u8)) & digest
            }
            0xC000.. => unreachable!("How did you get here??"),
            */
        }
    }
}
