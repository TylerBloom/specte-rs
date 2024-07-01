use std::ops::{Index, IndexMut};

use crate::cpu::u16_check_bit_const;

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct MBC2 {
    rom: Vec<u8>,
    // TODO: This can probably be replaced with NonZeroUsize
    rom_bank: u8,
    // TODO: The built-in RAM only uses the lower 4-bits. There is no good way to model this via
    // indexing, so we are going to rely on the ROM writers to obey this. It might be the case
    // that this needs to be changed.
    ram: Box<[u8; 512]>,
    ram_enabled: u8,
    dead_byte: u8,
}

impl Index<u16> for MBC2 {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        match index {
            0x0000..=0x3FFF => todo!("read from rom bank 0"),
            0x4000..=0x7FFF => todo!("read from rom bank 1-F"),
            i @ 0xA000..=0xBFFF => {
                &self.ram[(i & 0x01FF) as usize]
            },
            _ => unreachable!(),
        }
    }
}

impl IndexMut<u16> for MBC2 {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        match index {
            i @ 0x0000..=0x3FFF if u16_check_bit_const::<8>(i) => &mut self.rom_bank,
            i @ 0x0000..=0x3FFF => &mut self.ram_enabled,
            i @ 0xA000..=0xBFFF if self.ram_enabled == 0x0A => {
                todo!("Index into RAM")
            }
            0xA000..=0xBFFF if self.ram_enabled == 0x0A => {
                &mut self.dead_byte
            }
            _ => unreachable!(),
        }
    }
}
