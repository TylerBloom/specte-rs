#![allow(dead_code, unused)]

use std::ops::{Index, IndexMut};

mod mbc1;
use std::borrow::Cow;

pub use mbc1::*;
mod mbc2;
pub use mbc2::*;
mod mbc3;
pub use mbc3::*;
mod mbc5;
pub use mbc5::*;

use crate::instruction::Instruction;

pub static NINTENDO_LOGO: &[u8] = &[
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

pub enum MemoryBankController {
    /// There is no external MBC. The game ROM is mapped into the 32 KiB that starts at 0x0000 and
    /// extends to 0x7FFF. An additional 8 KiB of RAM could be connected. This 8 KiB starts at
    /// 0xA000 and extends to 0xBFFF.
    ///
    /// See the spec [here](https://gbdev.io/pandocs/nombc.html).
    Direct {
        // TODO: It is certainly possible that operations that are 2-bytes wide might need to
        // retrieve data over the ROM/RAM boundary. In that case, these vecs should be combined,
        // though this is might be a cornercase.
        /// A vec that holds 32 KiB (4,096 bytes).
        rom: Vec<u8>,
        /// A vec that holds 8 KiB (1,024 bytes).
        ram: Vec<u8>,
    },
    /// This memory controller is the first MBC chip and might be wired in two different ways.
    /// By default, this controller supports 512 KiB of ROM and 32 KiB of RAM.
    /// Alternatively, the controller can support up to 2 MiB of ROM and only 8 KiB of RAM.
    ///
    /// See the spec [here](https://gbdev.io/pandocs/MBC1.html).
    // TODO:
    // The spec says that any cartridges with 1 MiB or more of ROM use the second wiring, but what
    // about carts that use, say, 600 KiB? Should we just assume that larger carts use the
    // alternate wiring too?
    //
    // Looks like the answer is "yes". From the docs:
    // "Available RAM sizes are 8 KiB (at $A000–BFFF) and 32 KiB (in form of four 8K banks at
    // $A000–BFFF). 32 KiB is only available in cartridges with ROM <= 512 KiB."
    MBC1(MBC1),
    ///
    /// See the spec [here](https://gbdev.io/pandocs/MBC2.html).
    MBC2,
    ///
    /// See the spec [here](https://gbdev.io/pandocs/MBC3.html).
    MBC3,
    ///
    /// See the spec [here](https://gbdev.io/pandocs/MBC5.html).
    MBC5,
}

impl MemoryBankController {
    pub fn new<'a, C: Into<Cow<'a, [u8]>>>(cart: C) -> Self {
        let cart = cart.into();
        assert_eq!(&cart[0x0104..=0x133], NINTENDO_LOGO);
        let title: String = cart[0x0134..=0x0143].iter().map(|&b| b as char).collect();
        println!("Cartridge title: {title}");
        let is_cgb = match cart[0x143] {
            0x80 => true,
            0xC0 => false,
            // FIXME: Is this actually needed?
            b => {
                eprintln!("Unknown CGB code: {b}");
                false
            }
        };
        let rom_size = match cart[0x0148] {
            0x00 => 32,
            0x01 => 64,
            0x02 => 128,
            0x03 => 256,
            0x04 => 512,
            0x05 => 1024,
            0x06 => 2 * 1024,
            0x07 => 4 * 1024,
            0x08 => 8 * 1024,
            0x52 => todo!(), // 1.1 * 1024 * 1024,
            0x53 => todo!(), // 1.2 * 1024 * 1024,
            0x54 => todo!(), // 1.5 * 1024 * 1024,
            n => panic!("Unknown ROM size: {n}"),
        } * 1024;
        println!("Cartridge ROM size: {rom_size}");
        let ram_size: usize = match cart[0x0149] {
            0x00 => 0,
            0x02 => 8 * 1024,
            0x03 => 32 * 1024,
            0x04 => 128 * 1024,
            0x05 => 64 * 1024,
            n => panic!("Unknown RAM size: {n}"),
        };
        println!("Cartridge RAM size: {ram_size}");
        let head_check = cart[0x014D];
        assert_eq!(
            head_check,
            cart[0x0134..=0x014C]
                .iter()
                .copied()
                .fold(0u8, |acc, b| acc.wrapping_sub(b).wrapping_sub(1))
        );
        // Check cartridge type
        match cart[0x0147] {
            0x00 => {
                println!("ROM size: {rom_size:x}");
                let rom = Vec::from(&cart[0x0000..=0x7FFF]);
                assert_eq!(rom_size, rom.len());
                let ram = vec![0; ram_size];
                Self::Direct { rom, ram }
            }
            0x01 => Self::MBC1(MBC1::new(rom_size, ram_size as usize, &cart)),
            // TODO: Does the info of this bit need to be passed to the MBC1 constructor?
            0x02 => Self::MBC1(MBC1::new(rom_size, ram_size as usize, &cart)),
            // TODO: We need to communicate that the cart has RAM that is maintained by a battery.
            0x03 => Self::MBC1(MBC1::new(rom_size, ram_size as usize, &cart)),
            0x05 => todo!(),
            0x06 => todo!(),
            0x08 => todo!(),
            0x09 => todo!(),
            0x0B => todo!(),
            0x0C => todo!(),
            0x0D => todo!(),
            0x0F => todo!(),
            0x10 => todo!(),
            0x11 => todo!(),
            0x12 => todo!(),
            0x13 => todo!(),
            0x19 => todo!(),
            0x1A => todo!(),
            0x1B => todo!(),
            0x1C => todo!(),
            0x1D => todo!(),
            0x1E => todo!(),
            0x20 => todo!(),
            0x22 => todo!(),
            0xFC => todo!(),
            0xFD => todo!(),
            0xFE => todo!(),
            0xFF => todo!(),
            n => panic!("Unknown cartridge type: {n}"),
        }
    }

    pub fn read_op(&self, index: u16) -> Instruction {
        Instruction::parse(self.read_from(index))
    }

    fn read_from(&self, index: u16) -> &[u8] {
        let index = index as usize;
        match self {
            MemoryBankController::Direct { rom, ram } => {
                if index < rom.len() {
                    &rom[index..]
                } else {
                    &ram[index + 1 - rom.len()..]
                }
            }
            MemoryBankController::MBC1(_) => todo!(),
            MemoryBankController::MBC2 => todo!(),
            MemoryBankController::MBC3 => todo!(),
            MemoryBankController::MBC5 => todo!(),
        }
    }

    pub fn read_mut_from(&mut self, index: u16) -> &mut [u8] {
        let index = index as usize;
        match self {
            // TODO: This should probably panic (or something) if index < rom.len(), i.e. they are
            // trying to write to ROM.
            MemoryBankController::Direct { rom, ram } => &mut ram[index - rom.len()..],
            MemoryBankController::MBC1(_) => todo!(),
            MemoryBankController::MBC2 => todo!(),
            MemoryBankController::MBC3 => todo!(),
            MemoryBankController::MBC5 => todo!(),
        }
    }
}

impl Index<u16> for MemoryBankController {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        &self.read_from(index)[0]
    }
}

impl IndexMut<u16> for MemoryBankController {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        &mut self.read_mut_from(index)[0]
    }
}
