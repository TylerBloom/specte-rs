use std::borrow::Cow;
use std::fmt::Debug;
use std::ops::Index;
use std::ops::IndexMut;

mod direct;
mod mbc1;
mod mbc2;
mod mbc3;
mod mbc5;

pub use direct::*;
pub use mbc1::*;
pub use mbc2::*;
pub use mbc3::*;
pub use mbc5::*;
use serde::Deserialize;
use serde::Serialize;
use tracing::error;

/// The size of a ROM banks, 16 KiB.
pub const ROM_BANK_SIZE: usize = 16 * 1024;

/// The size of a RAM banks, 8 KiB.
pub const RAM_BANK_SIZE: usize = 8 * 1024;

#[derive(Hash, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum MemoryBankController {
    /// There is no external MBC. The game ROM is mapped into the 32 KiB that starts at 0x0000 and
    /// extends to 0x7FFF. An additional 8 KiB of RAM could be connected. This 8 KiB starts at
    /// 0xA000 and extends to 0xBFFF.
    ///
    /// See the spec [here](https://gbdev.io/pandocs/nombc.html).
    Direct {
        /// A vec that holds 32 KiB (4,096 bytes).
        rom: Vec<u8>,
        /// A vec that holds 8 KiB (1,024 bytes).
        ram: Vec<u8>,
        dead_byte: u8,
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
    MBC2(MBC2),
    ///
    /// See the spec [here](https://gbdev.io/pandocs/MBC3.html).
    MBC3(MBC3),
    ///
    /// See the spec [here](https://gbdev.io/pandocs/MBC5.html).
    MBC5(MBC5),
}

impl MemoryBankController {
    pub fn new(mut cart: Vec<u8>) -> Self {
        let title: String = cart[0x0134..=0x0143].iter().map(|&b| b as char).collect();
        let is_cgb = match cart[0x143] {
            0x80 => true,
            0xC0 => false,
            // FIXME: Is this actually needed?
            b => {
                error!("Unknown CGB code: {b}");
                false
            }
        };
        let rom_count = match cart[0x0148] {
            0x00 => 2,
            0x01 => 4,
            0x02 => 8,
            0x03 => 16,
            0x04 => 32,
            0x05 => 64,
            0x06 => 128,
            0x07 => 256,
            0x08 => 512,
            0x52 => todo!(), // 1.1 * 1024 * 1024,
            0x53 => todo!(), // 1.2 * 1024 * 1024,
            0x54 => todo!(), // 1.5 * 1024 * 1024,
            n => panic!("Unknown ROM size: {n}"),
        };
        let rom_size = rom_count * ROM_BANK_SIZE;
        if rom_size > cart.len() {
            let len = rom_size - cart.len();
            cart.extend(std::iter::repeat_n(0, len));
        }

        let ram_count: usize = match cart[0x0149] {
            0x00 => 0,
            0x01 | 0x02 => 1,
            0x03 => 4,
            0x04 => 16,
            0x05 => 8,
            n => panic!("Unknown RAM size: {n}"),
        };
        println!("The RAM count: {ram_count}");
        let ram_size = ram_count * RAM_BANK_SIZE;

        let head_check = cart[0x014D];
        assert_eq!(
            head_check,
            cart[0x0134..=0x014C]
                .iter()
                .copied()
                .fold(0u8, |acc, b| acc.wrapping_sub(b).wrapping_sub(1))
        );
        // Check cartridge type
        println!("Cartridge type: {}", cart[0x0147]);
        match cart[0x0147] {
            0x00 => {
                let rom = Vec::from(&cart[0x0000..=0x7FFF]);
                assert_eq!(rom_size, rom.len());
                let ram = vec![0; ram_size];
                Self::Direct {
                    rom,
                    ram,
                    dead_byte: 0,
                }
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
            0x0F => Self::MBC3(MBC3::new(rom_size, ram_size, &cart)),
            0x10 => Self::MBC3(MBC3::new(rom_size, ram_size, &cart)),
            0x11 => Self::MBC3(MBC3::new(rom_size, ram_size, &cart)),
            0x12 => Self::MBC3(MBC3::new(rom_size, ram_size, &cart)),
            0x13 => Self::MBC3(MBC3::new(rom_size, ram_size, &cart)),
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

    pub(super) fn direct_overwrite(&mut self, index: u16, val: &mut u8) {
        match self {
            MemoryBankController::Direct { rom, ram, .. } => {
                if index as usize >= rom.len() {
                    std::mem::swap(&mut ram[(index as usize) - rom.len()], val)
                } else {
                    std::mem::swap(&mut rom[index as usize], val)
                }
            }
            MemoryBankController::MBC1(controller) => controller.overwrite_rom_zero(index, val),
            MemoryBankController::MBC2(_) => todo!("MBC2 not yet impl-ed"),
            MemoryBankController::MBC3(controller) => controller.overwrite_rom_zero(index, val),
            MemoryBankController::MBC5(_) => todo!("MBC5 not yet impl-ed"),
        }
    }

    pub(super) fn read_byte(&self, index: u16) -> u8 {
        match self {
            MemoryBankController::Direct { rom, ram, .. } => {
                let index = index as usize;
                if index < rom.len() {
                    rom[index]
                } else {
                    ram[index + 1]
                }
            }
            MemoryBankController::MBC1(controller) => controller.read_byte(index),
            MemoryBankController::MBC2(_) => todo!("MBC2 not yet impl-ed"),
            MemoryBankController::MBC3(controller) => controller.read_byte(index),
            MemoryBankController::MBC5(_) => todo!("MBC5 not yet impl-ed"),
        }
    }

    pub(super) fn write_byte(&mut self, index: u16, value: u8) {
        match self {
            MemoryBankController::Direct {
                rom,
                ram,
                dead_byte,
            } => match index {
                0xA000..0xC000 => ram[index as usize - rom.len()] = value,
                // NOTE: This shouldn't happen
                _ => {}
            },
            MemoryBankController::MBC1(controller) => controller.write_byte(index, value),
            MemoryBankController::MBC2(_) => todo!("MBC2 not yet impl-ed"),
            MemoryBankController::MBC3(controller) => controller.write_byte(index, value),
            MemoryBankController::MBC5(_) => todo!("MBC5 not yet impl-ed"),
        }
    }

    #[track_caller]
    pub(super) fn update_byte(&mut self, index: u16, update: impl FnOnce(&mut u8)) -> u8 {
        match self {
            MemoryBankController::Direct {
                rom,
                ram,
                dead_byte,
            } => match index {
                0xA000..0xC000 => {
                    let ptr = &mut ram[index as usize - rom.len()];
                    update(ptr);
                    *ptr
                }
                // NOTE: This shouldn't happen
                _ => 0,
            },
            MemoryBankController::MBC1(_) => todo!("MBC1 not yet impl-ed"),
            MemoryBankController::MBC2(_) => todo!("MBC2 not yet impl-ed"),
            MemoryBankController::MBC3(controller) => controller.update_byte(index, update),
            MemoryBankController::MBC5(_) => todo!("MBC5 not yet impl-ed"),
        }
    }
}

impl Debug for MemoryBankController {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryBankController::Direct { rom, ram, .. } => write!(
                f,
                "Direct {{ rom_size: {}, ram_size: {} }}",
                rom.len(),
                ram.len()
            ),
            MemoryBankController::MBC1(_) => todo!("MBC1 not yet impl-ed"),
            MemoryBankController::MBC2(_) => todo!("MBC2 not yet impl-ed"),
            MemoryBankController::MBC3(_) => todo!("MBC3 not yet impl-ed"),
            MemoryBankController::MBC5(_) => todo!("MBC5 not yet impl-ed"),
        }
    }
}
