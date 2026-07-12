use serde::Deserialize;
use serde::Serialize;
use tracing::info;

use crate::mem::RamBank;
use crate::mem::RomBank;
use crate::mem::mbc::RAM_BANK_SIZE;
use crate::mem::mbc::ROM_BANK_SIZE;

/// The MBC5 controller supports up to 8 MiB of ROM (512 banks) and up to 128 KiB of RAM (16
/// banks). It is the first controller guaranteed to work with GBC double-speed mode. Unlike MBC1,
/// bank 0 is genuinely bank 0 (writing 0 to the ROM bank register selects bank 0), and there is no
/// banking-mode register.
///
/// See the spec [here](https://gbdev.io/pandocs/MBC5.html).
#[derive(Hash, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MBC5 {
    rom: Box<[RomBank]>,
    ram: Box<[RamBank]>,
    /// The selected ROM bank for the 0x4000..0x8000 region. The controller uses a 9-bit register:
    /// the lower 8 bits are written to 0x2000..0x3000 and the 9th bit to 0x3000..0x4000.
    rom_bank: u16,
    /// The selected RAM bank for the 0xA000..0xC000 region. Only the lower 4 bits are used. On
    /// rumble cartridges bit 3 drives the rumble motor instead of RAM banking; we mask it out and
    /// do not model rumble.
    ram_bank: u8,
    /// Determines if the RAM can be read from and written to. RAM is enabled when the lower 4 bits
    /// of the value written to the enable register are 0xA. Disabled by default.
    ram_enabled: bool,
    /// Calculated on construction and does not represent a register. It models the wiring to the
    /// ROM banks so that out-of-range bank selections wrap. This relies on the number of ROM banks
    /// being a power of two.
    rom_index_mask: u16,
    /// Calculated on construction. Models the wiring to the RAM banks. Zero when there is no RAM.
    ram_index_mask: u8,
}

impl std::fmt::Debug for MBC5 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MBC5")
            .field("rom_banks", &self.rom.len())
            .field("ram_banks", &self.ram.len())
            .field("rom_bank", &self.rom_bank)
            .field("ram_bank", &self.ram_bank)
            .field("ram_enabled", &self.ram_enabled)
            .finish_non_exhaustive()
    }
}

impl MBC5 {
    pub fn new(rom_size: usize, ram_size: usize, cart: &[u8]) -> Self {
        info!(
            "MBC5 is expecting {} many ROM banks, requiring {} many bytes from {} many bytes",
            rom_size / ROM_BANK_SIZE,
            rom_size,
            cart.len()
        );
        info!(
            "MBC5 is expecting {} many RAM banks, requiring {} many bytes",
            ram_size / RAM_BANK_SIZE,
            ram_size,
        );

        let rom_bank_count = rom_size / ROM_BANK_SIZE;
        let rom_index_mask = (rom_bank_count - 1) as u16;

        let ram_bank_count = ram_size / RAM_BANK_SIZE;
        let ram_index_mask = ram_bank_count.saturating_sub(1) as u8;

        let rom = (0..rom_bank_count)
            .map(|i| i * ROM_BANK_SIZE)
            .map(|i| cart[i..i + ROM_BANK_SIZE].iter().copied().collect())
            .collect();
        let ram = vec![RamBank::new(); std::cmp::max(ram_bank_count, 1)].into();

        Self {
            rom,
            ram,
            rom_bank: 1,
            ram_bank: 0,
            ram_enabled: false,
            rom_index_mask,
            ram_index_mask,
        }
    }

    pub(super) fn overwrite_rom_zero(&mut self, index: u16, val: &mut u8) {
        std::mem::swap(&mut self.rom[0][index as usize], val)
    }

    pub(super) fn read_byte(&self, index: u16) -> u8 {
        match index {
            0x0000..0x4000 => self.rom[0][index as usize],
            0x4000..0x8000 => {
                let bank = (self.rom_bank & self.rom_index_mask) as usize;
                self.rom[bank][(index - 0x4000) as usize]
            }
            0xA000..0xC000 => {
                if self.ram_enabled {
                    let bank = (self.ram_bank & self.ram_index_mask) as usize;
                    self.ram[bank][(index - 0xA000) as usize]
                } else {
                    0xFF
                }
            }
            _ => 0xFF,
        }
    }

    pub(super) fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0x0000..0x2000 => self.ram_enabled = (value & 0x0F) == 0x0A,
            // The lower 8 bits of the ROM bank number.
            0x2000..0x3000 => self.rom_bank = (self.rom_bank & 0x0100) | value as u16,
            // The 9th bit of the ROM bank number.
            0x3000..0x4000 => self.rom_bank = (self.rom_bank & 0x00FF) | ((value as u16 & 0x1) << 8),
            // The RAM bank number. Bit 3 drives the rumble motor on rumble carts, so we mask to the
            // lower 4 bits and rely on the RAM index mask to discard unused bits.
            0x4000..0x6000 => self.ram_bank = value & 0x0F,
            // MBC5 has no banking-mode register; writes here are ignored.
            0x6000..0x8000 => {}
            0xA000..0xC000 => {
                if self.ram_enabled {
                    let bank = (self.ram_bank & self.ram_index_mask) as usize;
                    self.ram[bank][(index - 0xA000) as usize] = value;
                }
            }
            _ => {}
        }
    }
}
