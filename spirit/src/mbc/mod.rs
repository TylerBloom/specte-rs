#![allow(dead_code, unused)]

use std::fmt::Debug;
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

use crate::lookup::{parse_instruction, Instruction};

pub static START_UP_HEADER: &[u8; 0x900] = include_bytes!("../cgb.bin");

pub type StartUpHeaders = ([u8; 0x100], [u8; 0x700]);

static NINTENDO_LOGO: &[u8] = &[
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MemoryMap {
    // The MBC
    mbc: MemoryBankController,
    // The video RAM
    vram: [u8; 0x2000],
    // The working RAM
    wram: ([u8; 0x1000], [u8; 0x1000]),
    // The Object attribute map
    oam: [u8; 0x100],
    // IO registers
    io: [u8; 0x80],
    // High RAM
    pub(crate) hr: [u8; 0x7F],
    // Interrupt reg
    interrupt: u8,
}


impl MemoryMap {
    pub fn new<'a, C: Into<Cow<'a, [u8]>>>(cart: C) -> Self {
        Self {
            mbc: MemoryBankController::new(cart),
            vram: [0; 0x2000],
            wram: ([0; 0x1000], [0; 0x1000]),
            oam: [0; 0x100],
            io: [0; 0x80],
            hr: [0; 0x7F],
            interrupt: 0,
        }
    }

    pub(crate) fn start_up_remap(&mut self) -> StartUpHeaders {
        let mut digest = ([0; 0x100], [0; 0x700]);
        for i in 0..=0xFF {
            digest.0[i] = START_UP_HEADER[i];
            self.mbc.direct_overwrite(i as u16, &mut digest.0[i]);
        }
        for i in 0..=0x6FF {
            digest.1[i] = START_UP_HEADER[i + 0x200];
            self.mbc
                .direct_overwrite((i + 0x200) as u16, &mut digest.1[i]);
        }
        digest
    }

    pub(crate) fn start_up_unmap(&mut self, mut headers: StartUpHeaders) {
        for i in 0..=0xFF {
            self.mbc.direct_overwrite(i as u16, &mut headers.0[i]);
        }
        for i in 0..=0x6FF {
            self.mbc
                .direct_overwrite((i + 0x200) as u16, &mut headers.1[i]);
        }
    }

    pub fn read_op(&self, index: u16) -> Instruction {
        parse_instruction(self, index)
    }

    /// Creates a dummy memory map that should only be used for testing. Notably, this will not
    /// have a ROM header, so it is not bootable.
    pub fn construct() -> Self {
        let rom = vec![0; 32000];
        let ram = vec![0; 4000];
        let mbc = MemoryBankController::Direct { rom, ram };
        Self {
            mbc,
            vram: [0; 0x2000],
            wram: ([0; 0x1000], [0; 0x1000]),
            oam: [0; 0x100],
            io: [0; 0x80],
            hr: [0; 0x7F],
            interrupt: 0,
        }
    }
}

// #[cfg(test)]
impl MemoryMap {
    pub fn rom_mut(&mut self) -> &mut [u8] {
        let MemoryBankController::Direct { rom, .. } = &mut self.mbc else {
            panic!("Do not call MemoryMap::rom unless you called MemoryMap::construct");
        };
        rom.as_mut_slice()
    }
}

impl Index<u16> for MemoryMap {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        match index {
            0x0000..=0x7FFF => &self.mbc[index],
            n @ 0x8000..=0x9FFF => &self.vram[n as usize - 0x8000],
            0xA000..=0xBFFF => todo!(),
            n @ 0xC000..=0xCFFF => &self.wram.0[n as usize - 0xC000],
            n @ 0xD000..=0xDFFF => &self.wram.1[n as usize - 0xD000],
            0xE000..=0xFDFF => todo!(),
            n @ 0xFE00..=0xFE9F => &self.oam[n as usize - 0xFE00],
            0xFEA0..=0xFEFF => unreachable!("No ROM should attempt to access this region"),
            index @ 0xFF00..=0xFF7F => &self.io[(index & !0xFF00) as usize],
            index @ 0xFF80..=0xFFFE => &self.hr[(index & !0xFF80) as usize],
            0xFFFF => &self.interrupt,
        }
    }
}

impl IndexMut<u16> for MemoryMap {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        match index {
            0x0000..=0x7FFF => &mut self.mbc[index],
            n @ 0x8000..=0x9FFF => &mut self.vram[n as usize - 0x8000],
            0xA000..=0xBFFF => todo!(),
            n @ 0xC000..=0xCFFF => &mut self.wram.0[n as usize - 0xC000],
            n @ 0xD000..=0xDFFF => &mut self.wram.1[n as usize - 0xD000],
            0xE000..=0xFDFF => todo!(),
            n @ 0xFE00..=0xFE9F => &mut self.oam[n as usize - 0xFE00],
            0xFEA0..=0xFEFF => unreachable!("No ROM should attempt to access this region"),
            0xFF00..=0xFF7F => &mut self.io[(index - 0xFF00) as usize],
            0xFF80..=0xFFFE => &mut self.hr[(index - 0xFF80) as usize],
            0xFFFF => &mut self.interrupt,
        }
    }
}

#[derive(Hash, Clone, PartialEq, Eq)]
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
        let ram_size: usize = match cart[0x0149] {
            0x00 => 0,
            0x02 => 8 * 1024,
            0x03 => 32 * 1024,
            0x04 => 128 * 1024,
            0x05 => 64 * 1024,
            n => panic!("Unknown RAM size: {n}"),
        };
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
            MemoryBankController::Direct { rom, ram } => {
                debug_assert!(index + 1 >= rom.len(), "Could not index into {index:X} because ROM ends as {:?}", rom.len());
                &mut ram[index + 1 - rom.len()..]
            },
            MemoryBankController::MBC1(_) => todo!(),
            MemoryBankController::MBC2 => todo!(),
            MemoryBankController::MBC3 => todo!(),
            MemoryBankController::MBC5 => todo!(),
        }
    }

    fn direct_overwrite(&mut self, index: u16, val: &mut u8) {
        match self {
            MemoryBankController::Direct { rom, ram } => {
                if index as usize >= rom.len() {
                    std::mem::swap(&mut ram[(index as usize) - rom.len()], val)
                } else {
                    std::mem::swap(&mut rom[index as usize], val)
                }
            }
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

impl Debug for MemoryBankController {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MemoryBankController::Direct { rom, ram } => write!(
                f,
                "Direct {{ rom_size: {}, ram_size: {} }}",
                rom.len(),
                ram.len()
            ),
            MemoryBankController::MBC1(_) => todo!(),
            MemoryBankController::MBC2 => todo!(),
            MemoryBankController::MBC3 => todo!(),
            MemoryBankController::MBC5 => todo!(),
        }
    }
}
