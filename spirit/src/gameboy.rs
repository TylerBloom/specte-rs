// TODO:
// Does it make sense to implement `Iterator`, `Stream`, or even `Future` for the gameboy?
// Conceptually, it maps fairly well... TBD
#![allow(clippy::large_enum_variant)]

use crate::cpu::Cpu;

/// This is the core emulation primative. It contains the entire state machine of the emulated
/// handheld and is agnostic to usecase and how it is rendered (if at all). Notably, the `Gameboy`
/// does not provide a `run` or analogous method. It must be ticked forward.
///
/// This allows managing tick rate, processing IO, and more to be done externally.
pub struct Gameboy {
    cpu: Cpu,
    mem: MemoryBankController,
}

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

pub struct MBC1 {
    mem: MBC1Bank,
    /// Determines if RAM can be read from and written to. The actual hardware uses an 8-bit
    /// register, so RAM is enabled when the lower 4 bits are set.
    ///
    /// Initially set to `false`, any writes to the memory addresses 0x0000 through 0x1FFF write to
    /// this register.
    ram_enabled: bool,
    /// Determines which ROM bank to read from when addressing into the address space 0x4000
    /// through 0x7FFF. If 0 or 1, the first memory bank is used. Also, this regsiter is a 5-bit
    /// register, so the top 3 bits are ignored when determining what bank to use.
    ///
    /// Initially set to 0, any writes to the memory addresses 0x2000 through 0x3FFF write to this
    /// register.
    // If this number of greater than the number of banks, the higher bits are masked until the
    // number is small enough
    rom_bank: u8,
    /// Determines which RAM bank to read from when addressing into the address space 0xA000
    /// through 0xBFFF. If 0 or 1, the first memory bank is used.
    ///
    /// Initially set to 0, any writes to the memory addresses 0x2000 through 0x3FFF write to this
    /// register.
    ram_bank: u8,
}

impl MBC1 {
    /// Retrieves a byte
    pub fn read(&self, index: u16) -> u8 {
        self.mem.read(index)
    }

    /// Writes to a register
    pub fn write(&mut self, index: u16, value: u8) {
        self.mem.write(index, value)
    }
}

/// This memory controller is the first MBC chip and might be wired in two different ways.
/// By default, this controller supports 512 KiB of ROM and 32 KiB of RAM.
/// Alternatively, the controller can support up to 2 MiB of ROM and only 8 KiB of RAM.
///
/// Note that when determining what ROM bank to use beyond the 0th bank, both 0x00 and 0x01
/// correspond to the first additional memory bank.
///
/// See the spec [here](https://gbdev.io/pandocs/MBC1.html).
// TODO: Would it be better to have the collection of membory banks be one large memory region that
// is then indexed into depending on which memory bank is selected?
//
// My guess is "yes". The size of the standard variant is 888 bytes while the Rewire variant is
// 3072 bytes. A Vec<T> is 24 bytes (on 64-bit machines). Using a single contigious allocation for
// sets of banks would reduce both variants to the 72 bytes.
pub enum MBC1Bank {
    Standard(MBC1Standard),
    Rewire(MBC1Rewired),
}

struct MBC1Standard {
    /// Memory bank 0, which holds the first 16 KiB of ROM which starts at 0x0000 and extends
    /// to 0x3FFF.
    rom_bank_0: Vec<u8>, // 16 KiB bank
    /// A series of memory banks, each holding 16 KiB of ROM.
    /// Regardless of the selected bank, these banks are addressed from 0x4000 to 0x7FFF.
    /// The selected bank is determined by ...
    ext_rom_banks: [Vec<u8>; 0x20], // 16 KiB banks x 32
    /// A series of memory banks, each holding 32 KiB of RAM.
    /// Regardless of the selected bank, these banks are addressed from 0xA000 to 0xBFFF.
    /// The selected bank is determined by ...
    ram: [Vec<u8>; 0x04], // 8 KiB banks x 4
}

struct MBC1Rewired {
    /// Memory bank 0, which holds the first 16 KiB of ROM which starts at 0x0000 and extends
    /// to 0x3FFF.
    rom_bank_0: Vec<u8>, // 16 KiB bank
    /// A series of memory banks, each holding 16 KiB of ROM.
    /// Regardless of the selected bank, these banks are addressed from 0x4000 to 0x7FFF.
    /// The selected bank is determined by ...
    // TODO: Do we need to avoid unecessary allocations based on ROM size?
    ext_rom_banks: [Vec<u8>; 0x7E], // 16 KiB banks x 162
    /// The sole memory back for RAM, which holds 8 KiB.
    /// This bank is addressed from 0xA000 to 0xBFFF.
    ram: Vec<u8>, // 8 KiB banks x 1
}

impl MBC1Bank {
    /// Retrieves a byte
    pub fn read(&self, index: u16) -> u8 {
        match self {
            MBC1Bank::Standard(bank) => bank.read(index),
            MBC1Bank::Rewire(bank) => bank.read(index),
        }
    }

    /// Writes to a register
    pub fn write(&mut self, index: u16, value: u8) {
        match self {
            MBC1Bank::Standard(bank) => bank.write(index, value),
            MBC1Bank::Rewire(bank) => bank.write(index, value),
        }
    }
}

impl MBC1Standard {
    /// Retrieves a byte
    pub fn read(&self, index: u16) -> u8 {
        match index {
            0x0000..=0x1FFF => todo!(),
            0x2000..=0x3FFF => todo!(),
            0x4000..=0x5FFF => todo!(),
            0x6000..=0x7FFF => todo!(),
        }
        todo!()
    }

    /// Writes to a register
    pub fn write(&mut self, index: u16, value: u8) {
        todo!()
    }
}

impl MBC1Rewired {
    /// Retrieves a byte
    pub fn read(&self, index: u16) -> u8 {
        todo!()
    }

    /// Writes to a register
    pub fn write(&mut self, index: u16, value: u8) {
        todo!()
    }
}
