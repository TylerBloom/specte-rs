use std::ops::Range;

use serde::{Deserialize, Serialize};

#[derive(Debug, Hash, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MBC1 {
    kind: MBC1Kind,
    rom: Vec<Vec<u8>>,
    rom_bank: u8,
    ram: Vec<Vec<u8>>,
    ram_bank: u8,
    /// Determines if RAM can be read from and written to. The actual hardware uses an 8-bit
    /// register, so RAM is enabled when the lower 4 bits are set.
    ///
    /// Initially set to `false`, any writes to the memory addresses 0x0000 through 0x1FFF write to
    /// this register.
    ram_enabled: bool,
    /// Determines the banking mode. The actual hardware uses an 8-bit
    /// register, so RAM is enabled when the lower 4 bits are set.
    ///
    /// Initially set to `false`, any writes to the memory addresses 0x0000 through 0x1FFF write to
    /// this register.
    banking_mode: BankingMode,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MBC1Kind {
    Standard,
    Rewired,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BankingMode {
    Simple = 0,
    Advanced = 1,
}

impl MBC1 {
    pub fn new(rom_size: usize, ram_size: usize, cart: &[u8]) -> Self {
        // All MBC1 cartridges with 1 MiB of ROM or more use this alternate wiring
        let kind = if rom_size > 1024 * 1024 {
            todo!()
        } else {
            MBC1Kind::Standard
        };
        let rom_bank_count = rom_size / (16 * 1024);
        let bank_count = rom_size / 0x4000;
        let rom = (0..bank_count)
            .map(|i| cart[0x4000 * i..0x4000 * (i + 1)].to_owned())
            .collect();
        let ram_bank_count = ram_size / (16 * 1024);
        let ram = vec![vec![0; 0x2000]; ram_size / 0x2000];
        Self {
            kind,
            rom,
            rom_bank: 1,
            ram,
            ram_bank: 0,
            ram_enabled: false,
            banking_mode: BankingMode::Simple,
        }
    }

    pub(super) fn overwrite_rom_zero(&mut self, index: u16, val: &mut u8) {
        std::mem::swap(&mut self.rom[0][index as usize], val)
    }

    pub fn read_byte(&self, index: u16) -> u8 {
        match index {
            0x0000..0x4000 => self.rom[0][index as usize],
            i @ 0x4000..0x8000 => {
                self.rom[self.rom_bank as usize][(i - 0x4000) as usize]
            },
            i @ 0xA000..0xC000 => self.ram[self.ram_bank as usize][(i - 0xA000) as usize],
            i => unreachable!("Memory controller is unable to read from memory address: 0x{i:0>4X}"),
        }
    }

    /// Writes to a register or RAM bank
    pub fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0x0000..0x2000 => self.ram_enabled = (value & 0x0F) == 0x0A,
            // TODO: Implement logic for if `value` > # of ROM banks.
            0x2000..0x4000 => self.rom_bank = std::cmp::max(1, value & 0x1F),
            0x4000..0x6000 => self.ram_bank = value & 0x3,
            0x6000..0x8000 if (value & 1) == 0 => self.banking_mode = BankingMode::Advanced,
            0x6000..0x8000 => self.banking_mode = BankingMode::Simple,
            i @ 0xA000..0xC000 => self.ram[self.ram_bank as usize][(i - 0xA000) as usize] = value,
            _ => unreachable!("Memory controller is unable to write to memory address: 0x{index:0>4X}"),
        }
    }
}

// TODO:
// Write testing for this sections of the Pandocs:
// https://gbdev.io/pandocs/MBC1.html#20003fff--rom-bank-number-write-only
//
// Also, add tests for reads and writes from smaller carts.
