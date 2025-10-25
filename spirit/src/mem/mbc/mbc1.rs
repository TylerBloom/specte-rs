use std::ops::Range;

use serde::Deserialize;
use serde::Serialize;

use crate::mem::mbc::RAM_BANK_SIZE;
use crate::mem::mbc::ROM_BANK_SIZE;

#[derive(Debug, Hash, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MBC1 {
    kind: MBC1Kind,
    rom: Vec<Vec<u8>>,
    ram: Vec<Vec<u8>>,
    bank_index_one: u8,
    bank_index_two: u8,
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

    /// Calculated on construction and does not represent a register. Rather, it models the wiring
    /// to the banks on a cart. For example, if a cart only has four ROM banks, the highest needed
    /// index to a bank is three. This is particularly useful for advanced mode rom indexing.
    ///
    /// This relies on the number of ROM banks being a multiple of two; otherwise, a simple bit
    /// mask would not work.
    index_mask: u8,
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

impl BankingMode {
    fn from_byte(value: u8) -> Self {
        if (value & 0x1) == 0 {
            Self::Simple
        } else {
            Self::Advanced
        }
    }
}

impl MBC1 {
    pub fn new(rom_size: usize, ram_size: usize, cart: &[u8]) -> Self {
        println!(
            "MBC1 is expecting {} many ROM banks, requiring {} many bytes from {} many bytes",
            rom_size / ROM_BANK_SIZE,
            rom_size,
            cart.len()
        );
        println!(
            "MBC1 is expecting {} many RAM banks, requiring {} many bytes",
            ram_size / RAM_BANK_SIZE,
            ram_size,
        );
        // All MBC1 cartridges with 1 MiB of ROM or more use this alternate wiring
        let kind = if rom_size > 1024 * 1024 {
            todo!()
        } else {
            MBC1Kind::Standard
        };
        let bank_count = rom_size / ROM_BANK_SIZE;

        let index_mask = (bank_count - 1) as u8;
        // I.e. bank_count is a multiple of two.
        debug_assert_eq!(
            (bank_count as u8).leading_zeros(),
            index_mask.leading_zeros() - 1
        );

        let rom = (0..bank_count)
            .map(|i| i * ROM_BANK_SIZE)
            .map(|i| cart[i..i + ROM_BANK_SIZE].to_owned())
            .collect();
        let ram = vec![vec![0; RAM_BANK_SIZE]; ram_size / RAM_BANK_SIZE];
        Self {
            kind,
            rom,
            bank_index_one: 1,
            ram,
            bank_index_two: 0,
            ram_enabled: false,
            banking_mode: BankingMode::Simple,
            index_mask,
        }
    }

    pub(super) fn overwrite_rom_zero(&mut self, index: u16, val: &mut u8) {
        std::mem::swap(&mut self.rom[0][index as usize], val)
    }

    #[inline]
    fn first_rom_bank(&self) -> usize {
        let base = matches!(self.banking_mode, BankingMode::Advanced)
            .then(|| self.bank_index_two << 5)
            .unwrap_or_default();
        (base & self.index_mask) as usize
    }

    #[inline]
    fn rom_bank(&self) -> usize {
        let bank = self.bank_index_two << 5 | self.bank_index_one;
        (bank & self.index_mask) as usize
    }

    // TODO: Write a small test for this
    /// NOTE: This does *not* take RAM enablement into consideration.
    #[inline]
    fn ram_bank(&self) -> usize {
        matches!(self.banking_mode, BankingMode::Advanced)
            .then_some(self.bank_index_two as usize)
            .unwrap_or_default()
    }

    #[inline]
    pub fn read_byte(&self, index: u16) -> u8 {
        match index {
            0x0000..0x4000 => self.rom[self.first_rom_bank()][index as usize],
            0x4000..0x8000 => self.rom[self.rom_bank()][(index - 0x4000) as usize],
            0xA000..0xC000 => self
                .ram_enabled
                .then(|| self.ram[self.ram_bank()][(index - 0xA000) as usize])
                .unwrap_or_default(),
            index => {
                unreachable!(
                    "Memory controller is unable to read from memory address: 0x{index:0>4X}"
                )
            }
        }
    }

    /// Writes to a register or RAM bank
    #[inline]
    pub fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0x0000..0x2000 => self.ram_enabled = (value & 0x0F) == 0b0101,
            0x2000..0x4000 => {
                // println!("Setting ROM bank one to {value}");
                self.bank_index_one = 0x1F & value;
            }
            0x4000..0x6000 => self.bank_index_two = 0x3 & value,
            0x6000..0x8000 => self.banking_mode = BankingMode::from_byte(value),
            0xA000..0xC000 => {
                if self.ram_enabled {
                    let bank = self.ram_bank();
                    self.ram[bank][(index - 0xA000) as usize] = value
                }
            }
            _ => unreachable!(
                "Memory controller is unable to write to memory address: 0x{index:0>4X}"
            ),
        }
    }
}

// TODO:
// Write testing for this sections of the Pandocs:
// https://gbdev.io/pandocs/MBC1.html#20003fff--rom-bank-number-write-only
//
// Also, add tests for reads and writes from smaller carts.

#[cfg(test)]
mod tests {
    use crate::mem::mbc::BankingMode;
    use crate::mem::mbc::ROM_BANK_SIZE;

    use super::MBC1;
    use super::MBC1Kind;

    // This test comes from the complete technical reference
    #[test]
    fn rom_bank_example_one() {
        let rom = (0..128).map(|i| vec![i; ROM_BANK_SIZE]).collect();
        let mut mbc = MBC1 {
            kind: MBC1Kind::Standard,
            rom,
            ram: Vec::new(),
            bank_index_one: 0x12,
            bank_index_two: 0x01,
            ram_enabled: false,
            banking_mode: BankingMode::Simple,
            index_mask: u8::MAX >> 1,
        };

        // While in simple mode
        let bank_one = mbc.read_byte(0x0000);
        assert_eq!(bank_one, 0);
        let bank_one = mbc.read_byte(0x3FFF);
        assert_eq!(bank_one, 0);

        let bank_two = mbc.read_byte(0x4000);
        assert_eq!(bank_two, 0x32);
        let bank_two = mbc.read_byte(0x7FFF);
        assert_eq!(bank_two, 0x32);

        // While in advanced mode
        mbc.banking_mode = BankingMode::Advanced;
        let bank_one = mbc.read_byte(0x0000);
        assert_eq!(bank_one, 0x20);
        let bank_one = mbc.read_byte(0x3FFF);
        assert_eq!(bank_one, 0x20);

        let bank_two = mbc.read_byte(0x4000);
        assert_eq!(bank_two, 0x32);
        let bank_two = mbc.read_byte(0x7FFF);
        assert_eq!(bank_two, 0x32);
    }

    // This test comes from the complete technical reference
    #[test]
    fn rom_bank_example_two() {
        let rom = (0..128).map(|i| vec![i; ROM_BANK_SIZE]).collect();
        let mut mbc = MBC1 {
            kind: MBC1Kind::Standard,
            rom,
            ram: Vec::new(),
            bank_index_one: 0x12,
            bank_index_two: 0x01,
            ram_enabled: false,
            banking_mode: BankingMode::Simple,
            index_mask: u8::MAX >> 1,
        };

        // While in simple mode
        mbc.bank_index_one = 0b00100;
        mbc.bank_index_two = 0b10;
        assert_eq!(mbc.rom_bank(), 0x44);

        let bank_one = mbc.read_byte(0x0000);
        assert_eq!(bank_one, 0);
        let bank_one = mbc.read_byte(0x3FFF);
        assert_eq!(bank_one, 0);

        let bank_two = mbc.read_byte(0x4000);
        assert_eq!(bank_two, 0x44);
        let bank_two = mbc.read_byte(0x7FFF);
        assert_eq!(bank_two, 0x44);
    }

    #[test]
    fn bank_mode_creation() {
        (0..u8::MAX).for_each(|i| assert_eq!(BankingMode::from_byte(i) as u8, 0x1 & i))
    }
}
