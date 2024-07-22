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
use tracing::trace;
use vram::PpuMode;

use self::vram::{CpuOamIndex, VRam, CpuVramIndex};

pub static START_UP_HEADER: &[u8; 0x900] = include_bytes!("../cgb.bin");

pub type StartUpHeaders = ([u8; 0x100], [u8; 0x700]);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct MemoryMap {
    // The MBC
    mbc: MemoryBankController,
    // The video RAM and Object attribute map
    vram: VRam,
    // The working RAM
    wram: ([u8; 0x1000], [u8; 0x1000]),
    io: IoRegisters,
    // High RAM
    hr: [u8; 0x7F],
    /// The interrupt enable register. Bits 0-4 flag where or not certain interrupt handlers can be
    /// called.
    ///  - Bit 0 corresponds to the VBlank interrupt
    ///  - Bit 1 corresponds to the LCD interrupt
    ///  - Bit 2 corresponds to the timer interrupt
    ///  - Bit 3 corresponds to the serial interrupt
    ///  - Bit 4 corresponds to the joypad interrupt
    /// When intexed, this register is at 0xFFFF.
    ie: u8,
    // There is a region of memory that is marked as inaccessible (0xFEA0 through 0xFEFF). Instead
    // of panicking when this area is accessed, a reference to this dead byte is used instead.
    dead_byte: u8,
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

    pub(crate) fn inc_lcd_y(&mut self) {
        self.io.inc_lcd_y()
    }

    pub(crate) fn inc_ppu_status(&mut self, state: PpuMode ) {
        self.vram.inc_status(state)
    }

    pub fn request_vblank_int(&mut self) {
        self.io.request_vblank_int()
    }

    pub fn request_lcd_int(&mut self) {
        self.io.request_lcd_int()
    }

    pub fn request_timer_int(&mut self) {
        self.io.request_timer_int()
    }

    pub fn request_serial_int(&mut self) {
        self.io.request_serial_int()
    }

    pub fn request_button_int(&mut self, input: ButtonInput) {
        self.io.request_button_int(input)
    }

    pub(crate) fn clear_interrupt_req(&mut self, op: InterruptOp) {
        self.io.clear_interrupt_req(op)
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

#[cfg(test)]
impl MemoryMap {
    pub fn io(&mut self) -> &mut IoRegisters {
        &mut self.io
    }
}

impl Index<u16> for MemoryMap {
    type Output = u8;

    fn index(&self, index: u16) -> &Self::Output {
        trace!("Mut index into MemMap: 0x{index:0>4X}");
        static DEAD_BYTE: u8 = 0;
        match index {
            0x0000..=0x7FFF => &self.mbc[index],
            n @ 0x8000..=0x9FFF => &self.vram[CpuVramIndex(n)],
            n @ 0xA000..=0xBFFF => &self.mbc[n],
            n @ 0xC000..=0xCFFF => &self.wram.0[n as usize - 0xC000],
            n @ 0xD000..=0xDFFF => &self.wram.1[n as usize - 0xD000],
            // Echo RAM
            n @ 0xE000..=0xEFFF => &self.wram.0[n as usize - 0xE000],
            n @ 0xF000..=0xFDFF => &self.wram.1[n as usize - 0xF000],
            n @ 0xFE00..=0xFE9F => &self.vram[CpuOamIndex(n)],
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
        // println!("Mut index into MemMap: 0x{index:0>4X}");
        trace!("Mut index into MemMap: 0x{index:0>4X}");
        match index {
            n @ 0x0000..=0x7FFF => &mut self.mbc[n],
            n @ 0x8000..=0x9FFF => &mut self.vram[CpuVramIndex(n)],
            n @ 0xA000..=0xBFFF => &mut self.mbc[n],
            n @ 0xC000..=0xCFFF => &mut self.wram.0[n as usize - 0xC000],
            n @ 0xD000..=0xDFFF => &mut self.wram.1[n as usize - 0xD000],
            // Echo RAM
            n @ 0xE000..=0xEFFF => &mut self.wram.0[n as usize - 0xE000],
            n @ 0xF000..=0xFDFF => &mut self.wram.1[n as usize - 0xF000],
            n @ 0xFE00..=0xFE9F => &mut self.vram[CpuOamIndex(n)],
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

/* --------- Indexing types use by the PPU --------- */

/// A type used to index into the Object Attribute Map. This type is only used by the PPU.
#[derive(Clone, Copy)]
pub(crate) struct OamIndex(pub u8);

impl Index<OamIndex> for MemoryMap {
    type Output = u8;

    fn index(&self, OamIndex(index): OamIndex) -> &Self::Output {
        todo!()
    }
}

/// A type used to index an object inside of the Object Attribute Map. This includes the y pos, x pos,
/// tile index, and attributes of the object. This type is only used by the PPU.
pub(crate) struct OamObjectIndex(pub u8);

impl Index<OamObjectIndex> for MemoryMap {
    type Output = [u8; 4];

    fn index(&self, OamObjectIndex(index): OamObjectIndex) -> &Self::Output {
        todo!()
    }
}

pub(crate) struct ObjTileDataIndex(pub u8);

impl Index<ObjTileDataIndex> for MemoryMap {
    /// We return a slice here because the object could be either 8x8 or 8x16, and this is
    /// determined by the state of a LCDC bit. It is the job of the PPU to check the length of the
    /// slice and handle it accordingly.
    type Output = [u8];

    fn index(&self, ObjTileDataIndex(index): ObjTileDataIndex) -> &Self::Output {
        todo!()
    }
}

/// A type used to index a background tile inside of the VRAM Tile Map. This type is only used by
/// the PPU.
// TODO: This should take the x and y position on the map and then this transform that into a
// index. This is because there are several pieces of data that we need to use (SCY, SCX, and BG
// tile map area from LCDC)
pub(crate) struct BgTileMapIndex(pub u8);

impl Index<BgTileMapIndex> for MemoryMap {
    type Output = u8;

    fn index(&self, BgTileMapIndex(index): BgTileMapIndex) -> &Self::Output {
        todo!()
    }
}

/// A type used to index a background tile's attributes inside VRAM Tile Map (in bank 1). This type
/// is only used by the PPU.
pub(crate) struct BgTileMapAttrIndex(pub u8);

impl Index<BgTileMapAttrIndex> for MemoryMap {
    type Output = u8;

    fn index(&self, BgTileMapAttrIndex(index): BgTileMapAttrIndex) -> &Self::Output {
        todo!()
    }
}

/// A type used to index a background tile's data inside VRAM Tile Data. This index is meant to be
/// used by first indexing using the `BgTileMapIndex` and then constructing this index from the
/// value returned there. This type is only used by the PPU.
///
/// NOTE: When indexing this way, all of the logic around what indexing strategy (defined by the
/// LCDC register) to use is handled here.
// TODO: Should the PPU should be forced to go through the `BgTileMapIndex` in order get this
// index? This is technically correct, but perhaps is too verbose/unwieldy.
pub(crate) struct BgTileDataIndex(pub u8);

impl Index<BgTileDataIndex> for MemoryMap {
    type Output = [u8; 16];

    fn index(&self, BgTileDataIndex(index): BgTileDataIndex) -> &Self::Output {
        todo!()
    }
}

// TODO: It seems unclear if the window should be indexed or if an iterator makes more sense
// there... Either way, some abstraction is needed to handle the window.
