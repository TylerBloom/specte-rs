#![allow(dead_code, unused)]

use std::borrow::Cow;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::{Index, IndexMut};

use crate::cpu::check_bit_const;
use crate::lookup::{parse_instruction, Instruction, InterruptOp};
use crate::{ButtonInput, JoypadInput, SsabInput};

pub mod io;
mod mbc;
pub(crate) mod vram;

pub use mbc::MemoryBankController;
use mbc::*;

use io::IoRegisters;

use self::vram::{OamIndex, VRam, VramIndex};

pub static START_UP_HEADER: &[u8; 0x900] = include_bytes!("../cgb.bin");

pub type StartUpHeaders = ([u8; 0x100], [u8; 0x700]);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MemoryMap {
    // The MBC
    mbc: MemoryBankController,
    // The video RAM and Object attribute map
    pub(crate) vram: VRam,
    // The working RAM
    wram: ([u8; 0x1000], [u8; 0x1000]),
    // There is a region of memory that is marked as inaccessible (0xFEA0 through 0xFEFF). Instead
    // of panicking when this area is accessed, a reference to this dead byte is used instead.
    dead_byte: u8,
    pub io: IoRegisters,
    // High RAM
    pub(crate) hr: [u8; 0x7F],
    /// The interrupt enable register. Bits 0-4 flag where or not certain interrupt handlers can be
    /// called.
    ///  - Bit 0 corresponds to the VBlank interrupt
    ///  - Bit 1 corresponds to the LCD interrupt
    ///  - Bit 2 corresponds to the timer interrupt
    ///  - Bit 3 corresponds to the serial interrupt
    ///  - Bit 4 corresponds to the joypad interrupt
    /// When intexed, this register is at 0xFFFF.
    pub ie: u8,
}

impl MemoryMap {
    pub fn new<'a, C: Into<Cow<'a, [u8]>>>(cart: C) -> Self {
        Self {
            mbc: MemoryBankController::new(cart),
            vram: VRam::new(),
            wram: ([0; 0x1000], [0; 0x1000]),
            dead_byte: 0,
            io: IoRegisters::default(),
            hr: [0; 0x7F],
            ie: 0,
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

    /// Reads the next operation to be performed. If the given IME flag is true, and an interrupts
    /// has been requested, the returned instruction will be a `call` to the corresponding
    /// interrupt handler. Otherwise, the given PC is used to decode the next instruction.
    pub fn read_op(&self, index: u16, ime: bool) -> Instruction {
        match self.check_interrupt() {
            Some(op) if ime => op,
            _ => parse_instruction(self, index),
        }
    }

    fn check_interrupt(&self) -> Option<Instruction> {
        match self.ie & self.io[0xFF0F] {
            0 => None,
            n => {
                if check_bit_const::<0>(n) {
                    Some(Instruction::Interrupt(InterruptOp::VBlank))
                } else if check_bit_const::<1>(n) {
                    Some(Instruction::Interrupt(InterruptOp::LCD))
                } else if check_bit_const::<2>(n) {
                    Some(Instruction::Interrupt(InterruptOp::Timer))
                } else if check_bit_const::<3>(n) {
                    Some(Instruction::Interrupt(InterruptOp::Serial))
                } else if check_bit_const::<4>(n) {
                    Some(Instruction::Interrupt(InterruptOp::Joypad))
                } else {
                    // Technically unreachable
                    None
                }
            }
        }
    }

    /// This method ticks the memory. The only thing this affects is the divider and timer
    /// registers.
    pub fn tick(&mut self) {
        self.io.tick();
    }

    // TODO: It is somewhat unclear to me if the IE register acts like a barrier to interrupts or
    // not. It would seems (from the start up sequence) the answer is "no".

    pub fn request_vblank_int(&mut self) {
        // self.io.interrupt_flags |= self.ie & 0b1;
        self.io.interrupt_flags |= 0b1;
    }

    pub fn request_lcd_int(&mut self) {
        // self.io.interrupt_flags |= self.ie & 0b10;
        self.io.interrupt_flags |= 0b10;
    }

    pub fn request_timer_int(&mut self) {
        // self.io.interrupt_flags |= self.ie & 0b100;
        self.io.interrupt_flags |= 0b100;
    }

    pub fn request_serial_int(&mut self) {
        // self.io.interrupt_flags |= self.ie & 0b1000;
        self.io.interrupt_flags |= 0b1000;
    }

    pub fn request_button_int(&mut self, input: ButtonInput) {
        self.io.joypad.register_input(input);
        // self.io.interrupt_flags |= self.ie & 0b1_0000;
        self.io.interrupt_flags |= 0b1_0000;
    }

    pub(crate) fn clear_interrupt_req(&mut self, op: InterruptOp) {
        let mask = match op {
            InterruptOp::VBlank => 0b1,
            InterruptOp::LCD => 0b10,
            InterruptOp::Timer => 0b100,
            InterruptOp::Serial => 0b1000,
            InterruptOp::Joypad => 0b1_0000,
        };
        self.io.interrupt_flags &= !mask;
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

    /// Creates a dummy memory map that should only be used for testing. Notably, this will not
    /// have a ROM header, so it is not bootable.
    pub fn construct() -> Self {
        let rom = vec![0; 32000];
        let ram = vec![0; 4000];
        let mbc = MemoryBankController::Direct { rom, ram };
        Self {
            mbc,
            vram: VRam::new(),
            wram: ([0; 0x1000], [0; 0x1000]),
            dead_byte: 0,
            io: IoRegisters::default(),
            hr: [0; 0x7F],
            ie: 0,
        }
    }
}

impl Index<u16> for MemoryMap {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        static DEAD_BYTE: u8 = 0;
        match index {
            0x0000..=0x7FFF => &self.mbc[index],
            n @ 0x8000..=0x9FFF => &self.vram[VramIndex(n)],
            0xA000..=0xBFFF => todo!(),
            n @ 0xC000..=0xCFFF => &self.wram.0[n as usize - 0xC000],
            n @ 0xD000..=0xDFFF => &self.wram.1[n as usize - 0xD000],
            // Echo RAM
            n @ 0xE000..=0xEFFF => &self.wram.0[n as usize - 0xE000],
            n @ 0xF000..=0xFDFF => &self.wram.1[n as usize - 0xF000],
            n @ 0xFE00..=0xFE9F => &self.vram[OamIndex(n)],
            // NOTE: This region *should not* actually be accessed, but, instead of panicking, a
            // dead byte will be returned instead.
            0xFEA0..=0xFEFF => &DEAD_BYTE,
            n @ 0xFF00..=0xFF7F => &self.io[n],
            n @ 0xFF80..=0xFFFE => &self.hr[(n - 0xFF80) as usize],
            0xFFFF => &self.ie,
        }
    }
}

impl IndexMut<u16> for MemoryMap {
    fn index_mut(&mut self, index: u16) -> &mut Self::Output {
        println!("Mut index into MemMap: 0x{index:0>4X}");
        match index {
            0x0000..=0x7FFF => &mut self.mbc[index],
            n @ 0x8000..=0x9FFF => &mut self.vram[VramIndex(n)],
            0xA000..=0xBFFF => todo!(),
            n @ 0xC000..=0xCFFF => &mut self.wram.0[n as usize - 0xC000],
            n @ 0xD000..=0xDFFF => &mut self.wram.1[n as usize - 0xD000],
            // Echo RAM
            n @ 0xE000..=0xEFFF => &mut self.wram.0[n as usize - 0xE000],
            n @ 0xF000..=0xFDFF => &mut self.wram.1[n as usize - 0xF000],
            n @ 0xFE00..=0xFE9F => &mut self.vram[OamIndex(n)],
            // NOTE: This region *should not* actually be accessed, but, instead of panicking, a
            // dead byte will be returned instead.
            0xFEA0..=0xFEFF => {
                self.dead_byte = 0;
                &mut self.dead_byte
            }
            n @ 0xFF00..=0xFF7F => &mut self.io[n],
            n @ 0xFF80..=0xFFFE => &mut self.hr[(n - 0xFF80) as usize],
            0xFFFF => &mut self.ie,
        }
    }
}
