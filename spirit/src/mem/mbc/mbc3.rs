use std::array;
use std::ops::Index;
use std::ops::IndexMut;

use chrono::DateTime;
use chrono::Utc;
use serde::Deserialize;
use serde::Serialize;
use tracing::info;

use crate::mem::RamBank;
use crate::mem::RomBank;
use crate::mem::mbc::RAM_BANK_SIZE;
use crate::mem::mbc::ROM_BANK_SIZE;

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
    ram_and_reg_enabled: u8,
    pub rom: Box<[RomBank]>,
    rom_bank: u8,
    ram_clock_index: RamAndClockIndex,
    ram: [RamBank; 4],
    clock_data: [u8; 5],
    latch_state: ClockLatchState,
    last_latch: DateTime<Utc>,
}

impl MBC3 {
    pub fn new(rom_size: usize, ram_size: usize, cart: &[u8]) -> Self {
        info!(
            "MBC3 is expecting {} many ROM banks, requiring {} many bytes from {} many bytes",
            rom_size / ROM_BANK_SIZE,
            rom_size,
            cart.len()
        );
        info!(
            "MBC3 is expecting {} many RAM banks, requiring {} many bytes",
            ram_size / RAM_BANK_SIZE,
            ram_size,
        );

        let rom_count = rom_size / ROM_BANK_SIZE;
        let rom = (0..rom_count)
            .map(|i| i * ROM_BANK_SIZE)
            .map(|i| cart[i..i + ROM_BANK_SIZE].iter().copied().collect())
            .collect();

        Self {
            ram_and_reg_enabled: 0,
            rom,
            rom_bank: 1,
            ram_clock_index: RamAndClockIndex::Ram(0),
            ram: array::from_fn(|_| RamBank::new()),
            clock_data: [0; 5],
            latch_state: ClockLatchState::Unprimed,
            last_latch: Utc::now(),
        }
    }

    pub(super) fn overwrite_rom_zero(&mut self, index: u16, val: &mut u8) {
        std::mem::swap(&mut self.rom[0][index as usize], val)
    }

    pub(super) fn read_byte(&self, index: u16) -> u8 {
        match index {
            0x0000..0x4000 => self.rom[0][index as usize],
            index @ 0x4000..0x8000 => {
                // info!("Reading from ROM BANK {} @ 0x{index:0>4X}", self.rom_bank + 1);
                self.rom[self.rom_bank as usize][(index - 0x4000) as usize]
            }
            0x8000..0xA000 => unreachable!("How did you get here??"),
            index @ 0xA000..0xC000 => {
                if !self.is_ram_accessable() {
                    return 0;
                }
                match self.ram_clock_index {
                    RamAndClockIndex::Ram(bank) => {
                        // info!("Reading from MBC3 RAM bank {bank} at 0x{index:0>4X}");
                        self.ram[bank as usize][(index - 0xA000) as usize]
                    }
                    RamAndClockIndex::Clock(index) => {
                        info!("Reading from MBC3 clock at {index:?}");
                        self.clock_data[index as usize]
                    }
                }
            }
            0xC000.. => unreachable!("How did you get here??"),
        }
    }

    fn is_ram_accessable(&self) -> bool {
        // Any value of the form 0xXA enables the RAM/registers
        (self.ram_and_reg_enabled & 0x0F) == 0x0A
    }

    pub(super) fn write_byte(&mut self, index: u16, value: u8) {
        info!("Writing to MBC3 @ 0x{index:0>4X}");
        match index {
            0x0000..0x2000 => self.ram_and_reg_enabled = value,
            // We ignore the top bit. If 0 if written in, we treat it as 1.
            0x2000..0x4000 => self.rom_bank = std::cmp::max(1, value & 0x7F),
            0x4000..0x6000 => {
                match value {
                    0..=7 => self.ram_clock_index = RamAndClockIndex::Ram(value),
                    0x8 => self.ram_clock_index = RamAndClockIndex::Clock(ClockIndex::Seconds),
                    0x9 => self.ram_clock_index = RamAndClockIndex::Clock(ClockIndex::Minutes),
                    0xA => self.ram_clock_index = RamAndClockIndex::Clock(ClockIndex::Hours),
                    0xB => self.ram_clock_index = RamAndClockIndex::Clock(ClockIndex::DayLower),
                    0xC => self.ram_clock_index = RamAndClockIndex::Clock(ClockIndex::DayUpper),
                    // NOTE: It is unclear to me what happen if anything else is written into this
                    // register. For now, we are just going to ignore it.
                    _ => info!("Received unclear RAM/Clock index"),
                }
                info!("Set RAM/clock index to {:?}", self.ram_clock_index);
            }
            0x6000..0x8000 => {
                if value == 0 {
                    info!("Priming clock latch...");
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
                if !self.is_ram_accessable() {
                    info!("Not enabled...");
                    return;
                }
                match self.ram_clock_index {
                    RamAndClockIndex::Ram(bank) => {
                        self.ram[bank as usize][(index - 0xA000) as usize] = value
                    }
                    RamAndClockIndex::Clock(index) => {
                        info!("Writing to clock: {value}");
                        self.clock_data[index as usize] = value
                    }
                }
            }
            0xC000.. => unreachable!("How did you get here??"),
        }
    }

    fn latch_clock(&mut self) {
        info!("Latching time: Init clock data {:?}", self.clock_data);
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
        info!("Updated clock data {:?}", self.clock_data);
        // TODO: Handle the high bits...
    }

    #[track_caller]
    pub(super) fn update_byte(&mut self, index: u16, update: impl FnOnce(&mut u8)) -> u8 {
        info!("Updating byte in MBC3 @ 0x{index:0>4X}");
        match index {
            0x0000..0x2000 => {
                update(&mut self.ram_and_reg_enabled);
                self.ram_and_reg_enabled
            }
            _ => 0,
            index @ 0x4000..0x8000 => self.rom[self.rom_bank as usize][(index - 0x4000) as usize],
            0x8000..0xA000 => unreachable!("How did you get here??"),
            index @ 0xA000..0xC000 => {
                if !self.is_ram_accessable() {
                    update(&mut 0);
                    return 0;
                }
                let digest = match self.ram_clock_index {
                    RamAndClockIndex::Ram(bank) => {
                        &mut self.ram[bank as usize][(index - 0xA000) as usize]
                    }
                    RamAndClockIndex::Clock(index) => &mut self.clock_data[index as usize],
                };
                update(digest);
                *digest
            }
            0xC000.. => unreachable!("How did you get here??"),
        }
    }
}
