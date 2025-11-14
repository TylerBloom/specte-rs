use std::fmt::Display;

use serde::Deserialize;
use serde::Serialize;
use tracing::info;

use crate::mem::RamBank;
use crate::mem::RomBank;
use crate::mem::mbc::RAM_BANK_SIZE;
use crate::mem::mbc::ROM_BANK_SIZE;

#[derive(Debug, Hash, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MBC1 {
    kind: MBC1Kind,
    rom: Box<[RomBank]>,
    ram: Box<[RamBank]>,
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
    rom_index_mask: u8,
    ram_index_mask: u8,
}

impl Display for MBC1 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            kind,
            rom: _,
            ram: _,
            bank_index_one,
            bank_index_two,
            ram_enabled,
            banking_mode,
            rom_index_mask,
            ram_index_mask,
        } = self;
        writeln!(f, "MBC1 {{")?;
        writeln!(f, "  MODE:  {}", self.banking_mode)?;
        writeln!(f, "  RAMG:  {}", self.ram_enabled)?;
        writeln!(f, "  BANK1: 0b{:0>8b}", self.bank_index_one)?;
        writeln!(f, "  BANK2: 0b{:0>8b}", self.bank_index_two)?;
        writeln!(f, "  ROM mask: 0b{:0>8b}", self.rom_index_mask)?;
        writeln!(f, "  RAM mask: 0b{:0>8b}", self.ram_index_mask)?;
        writeln!(f, "  rom_bank: 0x{:0>2X}", self.rom_bank())?;
        writeln!(f, "}}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MBC1Kind {
    Standard,
    Rewired,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, derive_more::Display)]
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
        info!(
            "MBC1 is expecting {} many ROM banks, requiring {} many bytes from {} many bytes",
            rom_size / ROM_BANK_SIZE,
            rom_size,
            cart.len()
        );
        info!(
            "MBC1 is expecting {} many RAM banks, requiring {} many bytes",
            ram_size / RAM_BANK_SIZE,
            ram_size,
        );
        // All MBC1 cartridges with 1 MiB of ROM or more use this alternate wiring
        let kind = if rom_size > 1024 * 1024 {
            // TODO: This should be Advanced, but it would require reworking the indexing logic at
            // bit more...
            MBC1Kind::Standard
        } else {
            MBC1Kind::Standard
        };

        let rom_bank_count = rom_size / ROM_BANK_SIZE;
        let rom_index_mask = (rom_bank_count - 1) as u8;
        // I.e. bank_count is a multiple of two.
        debug_assert_eq!(
            (rom_bank_count as u8).leading_zeros(),
            rom_index_mask.leading_zeros() - 1
        );

        let ram_bank_count = ram_size / RAM_BANK_SIZE;
        let ram_index_mask = ram_bank_count.saturating_sub(1) as u8;
        // I.e. bank_count is a multiple of two.
        if ram_bank_count > 0 {
            debug_assert_eq!(
                (ram_bank_count as u8).leading_zeros(),
                ram_index_mask.leading_zeros() - 1
            );
        }

        let rom = (0..rom_bank_count)
            .map(|i| i * ROM_BANK_SIZE)
            .map(|i| cart[i..i + ROM_BANK_SIZE].iter().copied().collect())
            .collect();
        let ram = vec![RamBank::new(); std::cmp::max(ram_size / RAM_BANK_SIZE, 1)].into();
        Self {
            kind,
            rom,
            bank_index_one: 1,
            ram,
            bank_index_two: 0,
            ram_enabled: false,
            banking_mode: BankingMode::Simple,
            rom_index_mask,
            ram_index_mask,
        }
    }

    pub fn first_rom(&self) -> &[u8; ROM_BANK_SIZE] {
        &self.rom[self.first_rom_bank()].0
    }

    pub fn rom(&self) -> &[u8; ROM_BANK_SIZE] {
        &self.rom[self.rom_bank()].0
    }

    pub fn ram(&self) -> &[u8; RAM_BANK_SIZE] {
        &self.ram[self.ram_bank()].0
    }

    pub(super) fn overwrite_rom_zero(&mut self, index: u16, val: &mut u8) {
        std::mem::swap(&mut self.rom[0][index as usize], val)
    }

    #[inline]
    fn first_rom_bank(&self) -> usize {
        let base = matches!(self.banking_mode, BankingMode::Advanced)
            .then(|| self.bank_index_two << 5)
            .unwrap_or_default();

        (base & self.rom_index_mask) as usize
    }

    #[inline]
    fn rom_bank(&self) -> usize {
        let bank = self.bank_index_two << 5 | self.bank_index_one;

        (bank & self.rom_index_mask) as usize
    }

    /// NOTE: This does *not* take RAM enablement into consideration.
    #[inline]
    fn ram_bank(&self) -> usize {
        (matches!(self.banking_mode, BankingMode::Advanced)
            .then_some(self.bank_index_two)
            .unwrap_or_default()
            & self.ram_index_mask) as usize
    }

    #[inline]
    pub fn read_byte(&self, index: u16) -> u8 {
        match index {
            0x0000..0x4000 => self.rom[self.first_rom_bank()][index as usize],
            0x4000..0x8000 => self.rom[self.rom_bank()][(index - 0x4000) as usize],
            0xA000..0xC000 => {
                if self.ram_enabled {
                    self.ram[self.ram_bank()][(index - 0xA000) as usize]
                } else {
                    0xFF
                }
            }
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
            0x0000..0x2000 => self.ram_enabled = (value & 0x0F) == 0b1010,
            0x2000..0x4000 => {
                self.bank_index_one = std::cmp::max(0x1F & value, 1);
            }
            0x4000..0x6000 => {
                self.bank_index_two = 0x3 & value;
            }
            0x6000..0x8000 => {
                self.banking_mode = BankingMode::from_byte(value);
            }
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
    use crate::mem::RomBank;
    use crate::mem::mbc::BankingMode;
    use crate::mem::mbc::ROM_BANK_SIZE;

    use super::MBC1;
    use super::MBC1Kind;

    // This test comes from the complete technical reference
    #[test]
    fn rom_bank_example_one() {
        let rom = (0..128)
            .map(|i| std::iter::repeat(i).collect())
            .collect();
        let mut mbc = MBC1 {
            kind: MBC1Kind::Standard,
            rom,
            ram: Vec::new().into(),
            bank_index_one: 0x12,
            bank_index_two: 0x01,
            ram_enabled: false,
            banking_mode: BankingMode::Simple,
            rom_index_mask: u8::MAX >> 1,
            ram_index_mask: 3,
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
        let rom = (0..128)
            .map(|i| RomBank::from_iter(std::iter::repeat(i)))
            .collect();
        let mut mbc = MBC1 {
            kind: MBC1Kind::Standard,
            rom,
            ram: Vec::new().into(),
            bank_index_one: 0x12,
            bank_index_two: 0x01,
            ram_enabled: false,
            banking_mode: BankingMode::Simple,
            rom_index_mask: u8::MAX >> 1,
            ram_index_mask: 3,
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
