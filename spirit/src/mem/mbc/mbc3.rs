use std::{
    array,
    ops::{Index, IndexMut},
};

use serde::{Deserialize, Serialize};

#[derive(Hash, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum ClockLatchState {
    /// State is reached by writing 0x00 to 0x6000..0x8000
    Primed,
    /// State is reached by writing any value other than 0x00 to 0x6000..0x8000
    Unprimed,
}

#[derive(Hash, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum RamAndClockIndex {
    Ram(u8),
    Clock(ClockIndex),
}

/// Used to index into the clock data. Note that each value maps to its index into the data array
/// not to the actual value that is written to the register in order to select the value.
#[derive(Hash, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[repr(usize)]
enum ClockIndex {
    Seconds = 0x01,
    Minutes = 0x02,
    Hours = 0x03,
    DayLower = 0x04,
    DayUpper = 0x05,
}

// TODO: How should clocks be handled...
#[derive(Hash, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MBC3 {
    ram_and_timer_enable: bool,
    rom_bank_zero: Vec<u8>, // Box<[u8; 0x4000]>,
    rom: Vec<Vec<u8>>,
    rom_bank: u8,
    ram_clock_index: RamAndClockIndex,
    ram: [Vec<u8>; 4],
    clock_data: [u8; 5],
    latch_state: ClockLatchState,
}

impl MBC3 {
    pub fn new(rom_count: usize, ram_count: usize, cart: &[u8]) -> Self {
        println!(
            "Expecting {rom_count} many ROM banks, requiring {} many bytes from {} many bytes",
            rom_count * 0x4000,
            cart.len()
        );
        let rom_bank_zero = cart[0..0x4000].to_owned();
        let rom = (1..rom_count - 1)
            .map(|i| i * 0x4000)
            .map(|i| cart[i..i + 0x4000].to_owned())
            .collect();
        Self {
            ram_and_timer_enable: false,
            rom_bank_zero,
            rom,
            rom_bank: 0,
            ram_clock_index: RamAndClockIndex::Ram(0),
            ram: array::from_fn(|_| vec![0; 0x2000]),
            clock_data: [0; 5],
            latch_state: ClockLatchState::Unprimed,
        }
    }

    pub(super) fn overwrite_rom_zero(&mut self, index: u16, val: &mut u8) {
        std::mem::swap(&mut self.rom_bank_zero[index as usize], val)
    }

    pub(super) fn read_byte(&self, index: u16) -> u8 {
        match index {
            0x0000..0x4000 => self.rom_bank_zero[index as usize],
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
        }
    }

    pub(super) fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0x0000..0x2000 => {
                if value == 0 {
                    self.ram_and_timer_enable = false;
                } else if value == 0x0A {
                    self.ram_and_timer_enable = true
                }
            }
            0x2000..0x4000 => {
                // We ignore the top bit. If 0 if written in, we treat it as 1.
                // We then subtract one in order to use `rom_bank` directly as an index. This is
                // fine because it is never directly read via indexing.
                self.rom_bank = std::cmp::min(1, value & 0x7F) - 1;
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
            }
            0x6000..0x8000 => {
                if value == 0 {
                    self.latch_state = ClockLatchState::Primed;
                } else {
                    if value == 1 && matches!(self.latch_state, ClockLatchState::Primed) {
                        // Get current time
                        // Convert current time into required data
                        // Store data
                        todo!()
                    }
                    self.latch_state = ClockLatchState::Unprimed;
                }
            }
            0x8000..0xA000 => unreachable!("How did you get here??"),
            0xA000..0xC000 => {
                if self.ram_and_timer_enable {
                    return;
                }
                match self.ram_clock_index {
                    RamAndClockIndex::Ram(bank) => self.ram[bank as usize][index as usize] = value,
                    RamAndClockIndex::Clock(index) => self.clock_data[index as usize] = value,
                }
            }
            0xC000.. => unreachable!("How did you get here??"),
        }
    }

    pub(super) fn update_byte(&self, index: u16, update: impl FnOnce(&mut u8)) -> u8 {
        todo!()
    }
}
