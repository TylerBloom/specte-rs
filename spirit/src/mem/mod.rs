#![allow(dead_code, unused)]

use std::borrow::Cow;
use std::fmt::Debug;
use std::hash::Hash;
use std::ops::Index;
use std::ops::IndexMut;

use crate::ButtonInput;
use crate::JoypadInput;
use crate::SsabInput;
use crate::cpu::check_bit_const;
use crate::lookup::Instruction;
use crate::lookup::InterruptOp;
use crate::lookup::parse_instruction;

pub mod io;
mod mbc;
pub(crate) mod vram;

pub use mbc::MemoryBankController;
use mbc::*;

use io::IoRegisters;
use serde::Deserialize;
use serde::Serialize;
use serde_with::serde_as;
use tracing::trace;
use vram::PpuMode;

use self::vram::CpuOamIndex;
use self::vram::CpuVramIndex;
use self::vram::VRam;

pub static START_UP_HEADER: &[u8; 0x900] = include_bytes!("../cgb.bin");

pub type StartUpHeaders = ([u8; 0x100], [u8; 0x700]);

/// This trait is used to abstract over the memory map. This is used during testing.
pub trait MemoryLike {
    fn read_byte(&self, addr: u16) -> u8;

    fn write_byte(&mut self, addr: u16, val: u8);

    fn read_op(&self, addr: u16, ime: bool) -> Instruction;

    fn clear_interrupt_req(&mut self, op: InterruptOp) {}

    fn vram_transfer(&mut self);
}

/// The `impl FnOnce` in `update_byte` would make `MemoryLike` non-object safe, which is needs for
/// instrution parsing.
pub trait MemoryLikeExt: MemoryLike {
    fn update_byte(&mut self, addr: u16, op: impl FnOnce(&mut u8)) -> u8 {
        let mut val = self.read_byte(addr);
        op(&mut val);
        self.write_byte(addr, val);
        val
    }
}

#[serde_as]
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct MemoryMap {
    // The MBC
    mbc: MemoryBankController,
    // The video RAM and Object attribute map
    pub vram: VRam,
    // The working RAM
    #[serde(serialize_with = "crate::utils::serialize_slices_as_one")]
    #[serde(deserialize_with = "crate::utils::deserialize_slices_as_one")]
    wram: [[u8; 0x1000]; 2],
    io: IoRegisters,
    // High RAM
    #[serde_as(as = "serde_with::Bytes")]
    hr: [u8; 0x7F],
    /// ADDR FF46
    oam_dma: OamDma,
    /// ADDR FF51-FF55
    vram_dma: VramDma,
    /// The interrupt enable register. Bits 0-4 flag where or not certain interrupt handlers can be
    /// called.
    ///  - Bit 0 corresponds to the VBlank interrupt
    ///  - Bit 1 corresponds to the LCD interrupt
    ///  - Bit 2 corresponds to the timer interrupt
    ///  - Bit 3 corresponds to the serial interrupt
    ///  - Bit 4 corresponds to the joypad interrupt
    /// When indexed, this register is at 0xFFFF.
    pub ie: u8,
}

impl MemoryLike for MemoryMap {
    /// This method is part of a family of methods that are similar to the methods from the `Index`
    /// trait.
    fn read_byte(&self, addr: u16) -> u8 {
        if self.oam_dma.in_conflict(addr) {
            println!("DMA Bus conflict @ {addr:0x}");
            return 0xFF;
        }
        self.dma_read_byte(addr)
    }

    /// This method is part of a family of methods that are similar to the methods from the `Index`
    /// trait. Unlike the index method, this provides control to the map around what gets written.
    /// For example, some registers only have some bits that can be written to. This allows all
    /// other bits to be masked out.
    fn write_byte(&mut self, addr: u16, val: u8) {
        if self.oam_dma.in_conflict(addr) {
            println!("DMA Bus conflict @ {addr:0x}");
            return;
        }
        trace!("Mut index into MemMap: 0x{addr:0>4X}");
        match addr {
            n @ 0x0000..=0x7FFF => self.mbc.write_byte(n, val),
            n @ 0x8000..=0x9FFF => {
                if n >= 0x9800 && check_bit_const::<3>(val) {
                    // println!("Writting 0b{val:0>8b} to 0x{n:0>4X}, which will read from VRAM bank 1");
                }
                self.vram[CpuVramIndex(self.io.vram_select == 1, n)] = val
            }
            n @ 0xA000..=0xBFFF => self.mbc.write_byte(n, val),
            n @ 0xC000..=0xCFFF => self.wram[0][n as usize - 0xC000] = val,
            n @ 0xD000..=0xDFFF => self.wram[1][n as usize - 0xD000] = val,
            // Echo RAM
            n @ 0xE000..=0xEFFF => self.wram[0][n as usize - 0xE000] = val,
            n @ 0xF000..=0xFDFF => self.wram[1][n as usize - 0xF000] = val,
            n @ 0xFE00..=0xFE9F => self.vram[CpuOamIndex(n)] = val,
            // NOTE: This region *should not* actually be accessed
            0xFEA0..=0xFEFF => {}
            0xFF51 => self.vram_dma.src_hi = val,
            0xFF52 => self.vram_dma.src_lo = val,
            0xFF53 => self.vram_dma.dest_hi = val,
            0xFF54 => self.vram_dma.dest_lo = val,
            0xFF55 => self.vram_dma.trigger(val),
            0xFF46 => self.oam_dma.trigger(val),
            n @ 0xFF00..=0xFF7F => self.io.write_byte(n, val),
            n @ 0xFF80..=0xFFFE => self.hr[(n - 0xFF80) as usize] = val,
            0xFFFF => self.ie = val,
        }
    }

    /// Reads the next operation to be performed. If the given IME flag is true, and an interrupts
    /// has been requested, the returned instruction will be a `call` to the corresponding
    /// interrupt handler. Otherwise, the given PC is used to decode the next instruction.
    fn read_op(&self, addr: u16, ime: bool) -> Instruction {
        match (
            self.vram_dma.get_op(self.io.lcd_y, self.vram.status),
            self.check_interrupt(),
        ) {
            (Some(op), _) => op,
            (None, Some(op)) if ime => op,
            _ => parse_instruction(self, addr),
        }
    }

    fn clear_interrupt_req(&mut self, op: InterruptOp) {
        self.io.clear_interrupt_req(op)
    }

    fn vram_transfer(&mut self) {
        let Some((src, dest)) = self.vram_dma.next_addrs() else {
            return;
        };
        for i in 0..16 {
            let byte = self.read_byte(src + i);
            self.write_byte(dest + i, byte);
        }
    }
}

impl MemoryLikeExt for MemoryMap {
    #[track_caller]
    fn update_byte(&mut self, index: u16, update: impl FnOnce(&mut u8)) -> u8 {
        let ptr = match index {
            n @ 0x0000..=0x7FFF => return self.mbc.update_byte(n, update),
            n @ 0x8000..=0x9FFF => &mut self.vram[CpuVramIndex(self.io.vram_select == 1, n)],
            n @ 0xA000..=0xBFFF => return self.mbc.update_byte(n, update),
            n @ 0xC000..=0xCFFF => &mut self.wram[0][n as usize - 0xC000],
            n @ 0xD000..=0xDFFF => &mut self.wram[1][n as usize - 0xD000],
            // Echo RAM
            n @ 0xE000..=0xEFFF => &mut self.wram[0][n as usize - 0xE000],
            n @ 0xF000..=0xFDFF => &mut self.wram[1][n as usize - 0xF000],
            n @ 0xFE00..=0xFE9F => &mut self.vram[CpuOamIndex(n)],
            // NOTE: This region *should not* actually be accessed
            0xFEA0..=0xFEFF => {
                return 0;
            }
            0xFF51 => &mut self.vram_dma.src_hi,
            0xFF52 => &mut self.vram_dma.src_lo,
            0xFF53 => &mut self.vram_dma.dest_hi,
            0xFF54 => &mut self.vram_dma.dest_lo,
            0xFF55 => {
                let mut byte = self.vram_dma.trigger;
                update(&mut byte);
                self.vram_dma.trigger(byte);
                return byte;
            }
            0xFF46 => {
                let mut byte = self.oam_dma.register;
                update(&mut byte);
                self.oam_dma.trigger(byte);
                return byte;
            }
            n @ 0xFF00..=0xFF7F => return self.io.update_byte(n, update),
            n @ 0xFF80..=0xFFFE => &mut self.hr[(n - 0xFF80) as usize],
            0xFFFF => &mut self.ie,
        };
        update(ptr);
        *ptr
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
struct OamDma {
    register: u8,
    read_addr: u16,
    write_addr: u16,
    ticks: u16,
    bus: ConflictBus,
}

/// When the OAM DMA is active, the transfer occurs on one of two busses. If a read or write occurs
/// from the CPU (including a push/pop to/from the stack or instruction read), the operation needs
/// to be ignored (reads get 0xFF).
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
enum ConflictBus {
    Wram,
    Cartridge,
}

impl OamDma {
    fn new() -> Self {
        Self {
            register: 0,
            read_addr: 0,
            write_addr: 0,
            ticks: 640,
            bus: ConflictBus::Cartridge,
        }
    }

    /// Calculates if a read/write is happening in region that is being transferred.
    fn in_conflict(&self, index: u16) -> bool {
        if self.ticks >= 640 {
            return false;
        }
        match (self.bus, index) {
            (ConflictBus::Wram, 0xC000..0xE000) => true,
            (ConflictBus::Wram, _) => false,
            (ConflictBus::Cartridge, 0xC000..0xE000) => false,
            (ConflictBus::Cartridge, _) => true,
        }
    }

    fn trigger(&mut self, value: u8) {
        self.register = value;
        // TODO: Verify that this is correct. The docs say that `value` must be below 0xDF, but the
        // multiplication should handle that.
        self.read_addr = (value as u16) << 8;
        self.bus = match self.read_addr {
            0xC000..0xE000 => ConflictBus::Wram,
            _ => ConflictBus::Cartridge,
        };
        self.write_addr = 0;
        println!("Beginning OAM DMA starting at 0x{:0>4X}", self.read_addr);
        self.ticks = 0;
    }

    fn tick(&mut self) -> Option<(u16, u16)> {
        if self.ticks == 640 {
            return None;
        }
        self.ticks += 1;
        if self.ticks % 4 == 0 {
            /*
            println!(
                "At {} ticks, transferring 0x{:0>4X} -> 0x{:0>4X}",
                self.ticks, self.read_addr, self.write_addr
            );
            */
            let digest = (self.read_addr, self.write_addr);
            self.read_addr += 1;
            self.write_addr += 1;
            Some(digest)
        } else {
            None
        }
    }
}

#[derive(Default, Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
struct VramDma {
    src_hi: u8,
    src_lo: u8,
    curr_src: u16,
    dest_hi: u8,
    dest_lo: u8,
    curr_dest: u16,
    trigger: u8,
    /// The amount of data left to transfer.
    len_left: u8,
    ly: u8,
}

impl VramDma {
    fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    fn read_trigger(&self) -> u8 {
        self.len_left.wrapping_sub(1)
    }

    fn trigger(&mut self, value: u8) {
        println!("Writing to VRAM DMA transfer");
        if !check_bit_const::<7>(value) && self.len_left > 0 {
            println!("Cancelling VRAM DMA transfer");
            self.len_left |= 1 << 7;
        } else {
            println!("Cancelling VRAM DMA transfer");
            self.trigger = value;
            self.len_left = value & 0x7F;
            self.curr_src = 0xFFF0 & u16::from_be_bytes([self.src_hi, self.src_hi]);
            self.curr_dest = 0x8000 + (0x1FF0 & u16::from_be_bytes([self.dest_hi, self.dest_lo]));
        }
    }

    /// Returns the next pair of addresses to transfer data from and into, respectively. These
    /// addresses represent an entire tile's worth data that needs to be transferred, not just a
    /// byte.
    fn next_addrs(&mut self) -> Option<(u16, u16)> {
        if self.len_left > 0 {
            self.len_left -= 1;
            let src = self.curr_src;
            let dest = self.curr_dest;
            self.curr_src += 16;
            self.curr_dest += 16;
            Some((src, dest))
        } else {
            None
        }
    }

    fn get_op(&self, ly: u8, state: PpuMode) -> Option<Instruction> {
        if check_bit_const::<7>(self.read_trigger()) {
            return None;
        }
        if check_bit_const::<7>(self.trigger) {
            if self.ly == ly && matches!(state, PpuMode::HBlank) {
                Some(Instruction::Transfer)
            } else {
                None
            }
        } else {
            Some(Instruction::Transfer)
        }
    }
}

impl MemoryMap {
    pub fn new(cart: Vec<u8>) -> Self {
        Self {
            mbc: MemoryBankController::new(cart),
            vram: VRam::new(),
            wram: [[0; 0x1000]; 2],
            io: IoRegisters::default(),
            hr: [0; 0x7F],
            ie: 0,
            oam_dma: OamDma::new(),
            vram_dma: VramDma::new(),
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
        println!("Starting start up unmapping...");
        for i in 0..=0xFF {
            self.mbc.direct_overwrite(i as u16, &mut headers.0[i]);
        }
        for i in 0..=0x6FF {
            self.mbc
                .direct_overwrite((i + 0x200) as u16, &mut headers.1[i]);
        }
        println!("Finished start up unmapping!!");
    }

    /// This method only exists to sidestep the "DMA contains" check and should only be called by
    /// the `read_byte` method and during the DMA transfer.
    fn dma_read_byte(&self, index: u16) -> u8 {
        static DEAD_BYTE: u8 = 0;
        match index {
            0x0000..=0x7FFF => self.mbc.read_byte(index),
            n @ 0x8000..=0x9FFF => self.vram[CpuVramIndex(self.io.vram_select == 1, n)],
            n @ 0xA000..=0xBFFF => self.mbc.read_byte(n),
            n @ 0xC000..=0xCFFF => self.wram[0][n as usize - 0xC000],
            n @ 0xD000..=0xDFFF => self.wram[1][n as usize - 0xD000],
            // Echo RAM
            n @ 0xE000..=0xEFFF => self.wram[0][n as usize - 0xE000],
            n @ 0xF000..=0xFDFF => self.wram[1][n as usize - 0xF000],
            n @ 0xFE00..=0xFE9F => self.vram[CpuOamIndex(n)],
            // NOTE: This region *should not* actually be accessed, but, instead of panicking, a
            // dead byte will be returned instead.
            0xFEA0..=0xFEFF | 0xFF51..0xFF54 => DEAD_BYTE,
            0xFF55 => self.vram_dma.read_trigger(),
            0xFF46 => self.oam_dma.register,
            n @ 0xFF00..=0xFF7F => self.io.read_byte(n),
            n @ 0xFF80..=0xFFFE => self.hr[(n - 0xFF80) as usize],
            0xFFFF => self.ie,
        }
    }

    pub(crate) fn write_bytes(&mut self, idx: u16, val: u16) -> u16 {
        todo!()
    }

    fn check_interrupt(&self) -> Option<Instruction> {
        match self.ie & self.io.read_byte(0xFF0F) {
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
        if let Some((r, w)) = self.oam_dma.tick() {
            let byte = self.dma_read_byte(r);
            // println!("Transferring byte to OAM: 0x{byte:0>2X}");
            self.vram.oam[w as usize] = byte;
        }
        self.io.tick();
    }

    pub(crate) fn reset_ppu_status(&mut self) {
        self.vram.reset_status()
    }

    pub(crate) fn inc_ppu_status(&mut self, state: PpuMode) {
        self.vram.inc_status(state)
    }

    pub(crate) fn reset_lcd_y(&mut self) {
        self.io.reset_lcd_y()
    }

    pub(crate) fn inc_lcd_y(&mut self) {
        self.io.inc_lcd_y()
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
        let mbc = MemoryBankController::Direct {
            rom,
            ram,
            dead_byte: 0,
        };
        Self {
            mbc,
            vram: VRam::new(),
            wram: [[0; 0x1000]; 2],
            io: IoRegisters::default(),
            hr: [0; 0x7F],
            ie: 0,
            oam_dma: OamDma::new(),
            vram_dma: VramDma::new(),
        }
    }

    pub fn io(&self) -> &IoRegisters {
        &self.io
    }

    pub fn io_mut(&mut self) -> &mut IoRegisters {
        &mut self.io
    }
}

/* --------- Indexing types use by the PPU --------- */

/// A type used to index an object inside of the Object Attribute Map. The inner value of the index
/// notes the object's position in the map and *not* the object's address in memory. This includes
/// the y pos, x pos, tile index, and attributes of the object. This type is only used by the PPU.
pub struct OamObjectIndex(pub u8);

impl Index<OamObjectIndex> for MemoryMap {
    type Output = [u8; 4];

    fn index(&self, index: OamObjectIndex) -> &Self::Output {
        &self.vram[index]
    }
}

pub struct ObjTileDataIndex(pub u8, pub bool);

impl Index<ObjTileDataIndex> for MemoryMap {
    /// We return a slice here because the object could be either 8x8 or 8x16, and this is
    /// determined by the state of a LCDC bit. It is the job of the PPU to check the length of the
    /// slice and handle it accordingly.
    type Output = [u8];

    fn index(&self, index: ObjTileDataIndex) -> &Self::Output {
        &self.vram[(index, check_bit_const::<2>(self.io.lcd_control))]
    }
}

/// A type used to index a background tile inside of the VRAM Tile Map.
///
/// This type is only used by the PPU. As such, the coordinates should be intruppted as the
/// position of the pixels that the PPU is trying to render. This means that 1) they should be
/// treated as offsets from the BG position registers and 2) they will need to be divided in order
/// to index into the map.
pub struct BgTileMapIndex {
    pub x: u8,
    pub y: u8,
}

struct BgTileMapInnerIndex {
    pub second_map: bool,
    pub x: u8,
    pub y: u8,
}

impl Index<BgTileMapIndex> for MemoryMap {
    type Output = u8;

    fn index(&self, BgTileMapIndex { x, y }: BgTileMapIndex) -> &Self::Output {
        let index = BgTileMapInnerIndex {
            second_map: check_bit_const::<3>(self.io.lcd_control),
            // We want to ignore the bottom 3 bits
            x: x.wrapping_add(self.io.bg_position.1 & 0xF8),
            y: y.wrapping_add(self.io.bg_position.0),
        };
        &self.vram[index]
    }
}

/// A type used to index a background tile's attributes inside VRAM Tile Map (in bank 1).
///
/// This type is only used by the PPU.
pub struct BgTileMapAttrIndex {
    pub x: u8,
    pub y: u8,
}

pub struct BgTileMapAttrInnerIndex {
    pub second_map: bool,
    pub x: u8,
    pub y: u8,
}

impl Index<BgTileMapAttrIndex> for MemoryMap {
    type Output = u8;

    fn index(&self, BgTileMapAttrIndex { x, y }: BgTileMapAttrIndex) -> &Self::Output {
        let index = BgTileMapAttrInnerIndex {
            // TODO: Is this correct???
            second_map: false, // check_bit_const::<3>(self.io.lcd_control),
            x: x.wrapping_add(self.io.bg_position.1 & 0xF8),
            y: y.wrapping_add(self.io.bg_position.0),
        };
        &self.vram[index]
    }
}

pub struct WindowTileMapIndex {
    pub x: u8,
    pub y: u8,
}

impl Index<WindowTileMapIndex> for MemoryMap {
    type Output = u8;

    fn index(&self, WindowTileMapIndex { x, y }: WindowTileMapIndex) -> &Self::Output {
        // We use the BG index here because the only difference between Window and BG data is how
        // we calculate the X and Y (and transparently... TBD on if that's an issue)
        let index = BgTileMapInnerIndex {
            second_map: check_bit_const::<6>(self.io.lcd_control),
            // TODO: Not sure if this needs to be wrapping. Also, how are the X and Y bounds (< 143
            // and 166, respectively) are honored. Probably should be controlled on writes...
            x: x.wrapping_sub(self.io.window_position[1].wrapping_sub(7)),
            y,
        };
        &self.vram[index]
    }
}

pub struct WindowTileMapAttrIndex {
    pub x: u8,
    pub y: u8,
}

impl Index<WindowTileMapAttrIndex> for MemoryMap {
    type Output = u8;

    fn index(&self, WindowTileMapAttrIndex { x, y }: WindowTileMapAttrIndex) -> &Self::Output {
        let index = BgTileMapAttrInnerIndex {
            second_map: check_bit_const::<6>(self.io.lcd_control),
            // TODO: Not sure if this needs to be wrapping. Also, how are the X and Y bounds (< 143
            // and 166, respectively) are honored. Probably should be controlled on writes...
            x: x.wrapping_sub(self.io.window_position[1].wrapping_sub(7)),
            y, //.wrapping_add(self.io.window_position[0]),
        };
        &self.vram[index]
    }
}

/// A type used to index a background tile's data inside VRAM Tile Data. This index is meant to be
/// used by first indexing using the `BgTileMapIndex` and then constructing this index from the
/// value returned there. This type is only used by the PPU.
// TODO: Should the PPU should be forced to go through the `BgTileMapIndex` in order get this
// index? This is technically correct, but perhaps is too verbose/unwieldy.
pub struct BgTileDataIndex {
    pub index: u8,
    pub attr: u8,
}

struct BgTileDataInnerIndex {
    unsigned_indexing: bool,
    index: u8,
    bank: bool,
}

impl Index<BgTileDataIndex> for MemoryMap {
    type Output = [u8; 16];

    fn index(&self, BgTileDataIndex { index, attr }: BgTileDataIndex) -> &Self::Output {
        let index = BgTileDataInnerIndex {
            index,
            unsigned_indexing: check_bit_const::<4>(self.io.lcd_control),
            bank: check_bit_const::<3>(attr),
            // bank: false,
        };
        &self.vram[index]
    }
}

pub struct WindowTileDataIndex {
    pub index: u8,
    pub attr: u8,
}

impl Index<WindowTileDataIndex> for MemoryMap {
    type Output = [u8; 16];

    fn index(&self, WindowTileDataIndex { index, attr }: WindowTileDataIndex) -> &Self::Output {
        let index = BgTileDataInnerIndex {
            index,
            unsigned_indexing: check_bit_const::<4>(self.io.lcd_control),
            bank: check_bit_const::<3>(attr),
        };
        &self.vram[index]
    }
}

#[cfg(test)]
impl MemoryLike for Vec<u8> {
    fn read_byte(&self, addr: u16) -> u8 {
        // println!("Read at 0x{addr:0>4X} (aka {addr})");
        self[addr as usize]
    }

    fn write_byte(&mut self, addr: u16, val: u8) {
        // println!("Writing {val} to 0x{addr:0>4X} (aka {addr})");
        self[addr as usize] = val;
    }

    fn read_op(&self, addr: u16, _ime: bool) -> Instruction {
        parse_instruction(self, addr)
    }

    fn vram_transfer(&mut self) {}
}

#[cfg(test)]
impl MemoryLikeExt for Vec<u8> {}
