use serde::Deserialize;
use serde::Serialize;
use serde_with::serde_as;
use tracing::info;

use crate::mem::RomBank;
use crate::mem::mbc::ROM_BANK_SIZE;

/// The MBC2 controller supports up to 256 KiB of ROM (16 banks) and includes 512 half-bytes (4-bit
/// values) of built-in RAM. Unlike the other controllers, it does not support external RAM; the
/// built-in RAM is battery-backable instead.
///
/// See the spec [here](https://gbdev.io/pandocs/MBC2.html).
#[serde_as]
#[derive(Hash, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MBC2 {
    rom: Box<[RomBank]>,
    /// The selected ROM bank for the 0x4000..0x8000 region. Only the lower 4 bits are used, and a
    /// value of 0 is treated as 1.
    rom_bank: u8,
    /// The built-in RAM. Each byte only uses its lower 4 bits; the upper 4 bits are undefined.
    #[serde_as(as = "serde_with::Bytes")]
    ram: Box<[u8; 512]>,
    /// Determines if the built-in RAM can be read from and written to. RAM is enabled when the
    /// lower 4 bits of the value written to the enable register are 0xA. Disabled by default.
    ram_enabled: bool,
    /// Calculated on construction and does not represent a register. It models the wiring to the
    /// ROM banks so that out-of-range bank selections wrap. This relies on the number of ROM banks
    /// being a power of two.
    rom_index_mask: u8,
}

impl std::fmt::Debug for MBC2 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("MBC2")
            .field("rom_banks", &self.rom.len())
            .field("rom_bank", &self.rom_bank)
            .field("ram_enabled", &self.ram_enabled)
            .finish_non_exhaustive()
    }
}

impl MBC2 {
    pub fn new(rom_size: usize, cart: &[u8]) -> Self {
        info!(
            "MBC2 is expecting {} many ROM banks, requiring {} many bytes from {} many bytes",
            rom_size / ROM_BANK_SIZE,
            rom_size,
            cart.len()
        );

        let rom_bank_count = rom_size / ROM_BANK_SIZE;
        let rom_index_mask = (rom_bank_count - 1) as u8;

        let rom = (0..rom_bank_count)
            .map(|i| i * ROM_BANK_SIZE)
            .map(|i| cart[i..i + ROM_BANK_SIZE].iter().copied().collect())
            .collect();

        Self {
            rom,
            rom_bank: 1,
            ram: Box::new([0; 512]),
            ram_enabled: false,
            rom_index_mask,
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
                    // Only the bottom 9 bits of the address index into the built-in RAM, so the
                    // region echoes every 512 bytes. Only the lower 4 bits of each byte are
                    // meaningful; the upper 4 bits are undefined and are returned as set.
                    self.ram[(index & 0x01FF) as usize] | 0xF0
                } else {
                    0xFF
                }
            }
            _ => 0xFF,
        }
    }

    pub(super) fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0x0000..0x4000 => {
                // Bit 8 of the address (the LSB of the upper address byte) selects between the RAM
                // enable register and the ROM bank register.
                if index & 0x0100 == 0 {
                    self.ram_enabled = (value & 0x0F) == 0x0A;
                } else {
                    // Only the lower 4 bits select the bank, and a value of 0 is treated as 1.
                    self.rom_bank = std::cmp::max(value & 0x0F, 1);
                }
            }
            0xA000..0xC000 => {
                if self.ram_enabled {
                    self.ram[(index & 0x01FF) as usize] = value & 0x0F;
                }
            }
            _ => {}
        }
    }
}
