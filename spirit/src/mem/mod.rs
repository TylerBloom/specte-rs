#![allow(dead_code, unused)]

use std::fmt::Debug;
use std::fmt::Display;
use std::hash::Hash;
use std::ops::Index;

use serde::Deserialize;
use serde::Serialize;
use serde_with::serde_as;
use tracing::info;
use tracing::trace;

use self::vram::CpuOamIndex;
use self::vram::CpuVramIndex;
use self::vram::VRam;
use self::vram::dma::VramDma;

use crate::ButtonInput;
use crate::JoypadInput;
use crate::SsabInput;
use crate::cpu::check_bit_const;
use crate::instruction::Instruction;
use crate::instruction::InterruptOp;
use crate::lookup::parse_instruction;
use crate::mem::oam::dma::OamDma;
use crate::ppu::Ppu;

pub mod io;
pub mod mbc;
pub mod oam;
pub mod vram;

use io::IoRegisters;
pub use mbc::MemoryBankController;
use mbc::*;
use vram::PpuMode;

pub static START_UP_HEADER: &[u8; 0x900] = include_bytes!("../cgb.bin");

pub type StartUpHeaders = ([u8; 0x100], [u8; 0x700]);

/// This trait is used to abstract over the memory map. This is used during testing.
pub trait MemoryLike {
    fn read_byte(&self, addr: u16) -> u8;

    fn write_byte(&mut self, addr: u16, val: u8);

    fn read_op(&self, addr: u16, ime: bool) -> Instruction;

    fn clear_interrupt_req(&mut self, op: InterruptOp) {}

    fn check_interrupt(&self) -> Option<InterruptOp>;

    fn vram_transfer(&mut self);

    fn switch_speeds(&mut self);

    /// Makes necessary changes in memory that occur during a instruction that aren't just reads
    /// and writes and then ticks the PPU to make its necessary changes.
    // FIXME: It generally makes more sense to just have the PPU just take a reference to the
    // memory (that's the PPU's tick is implemented) and have the tick be done by the Gameboy
    // directly. Unfortunately, the `MemoryLike` abstraction makes that much more difficult. When
    // `MemoryLike` is removed, `MemoryMap::tick` does not need to take this reference.
    fn tick(&mut self, ppu: &mut Ppu);
}

#[serde_as]
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct MemoryMap {
    // The MBC
    pub mbc: MemoryBankController,
    // The video RAM and Object attribute map
    pub vram: VRam,
    // The working RAM
    wram: WRam,
    pub(crate) io: IoRegisters,
    // High RAM
    #[serde_as(as = "serde_with::Bytes")]
    pub(crate) hr: [u8; 0x7F],
    /// ADDR FF46
    pub oam_dma: OamDma,
    /// ADDR FF51-FF55
    pub(crate) vram_dma: VramDma,
    /// The interrupt enable register. Bits 0-4 flag where or not certain interrupt handlers can be
    /// called.
    ///  - Bit 0 corresponds to the VBlank interrupt
    ///  - Bit 1 corresponds to the LCD interrupt
    ///  - Bit 2 corresponds to the timer interrupt
    ///  - Bit 3 corresponds to the serial interrupt
    ///  - Bit 4 corresponds to the joypad interrupt
    /// When indexed, this register is at 0xFFFF.
    pub ie: u8,
    pub(crate) speed_mode: SpeedMode,
    is_gbc: bool,
}

/// Marks the speed at which certain parts of the GB should be running at. Note that "speed" here
/// is tracked relatively rather than in absolute terms. That is, for example, the absolute amount
/// amount of time that it takes the CPU to "tick" once (a machine cycle) is cut in half in double
/// speed mode. So, relative to the PPU, the CPU performs twice as many cycles in the same span of
/// time compared to what it would perform at standard speed.
///
/// The values of the variants denote the number of ticks that components not affected
/// by the speed change perform in the amount of time the CPU "tick"s once.
#[repr(u8)]
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) enum SpeedMode {
    Standard = 4,
    Double = 2,
}

impl MemoryLike for MemoryMap {
    /// This method is part of a family of methods that are similar to the methods from the `Index`
    /// trait.
    fn read_byte(&self, addr: u16) -> u8 {
        if self.oam_dma.in_conflict(addr) {
            info!("DMA Bus read conflict @ 0x{addr:0>4X}");
            println!("DMA Bus read conflict @ 0x{addr:0>4X}");
            return 0xFF;
        }
        self.dma_read_byte(addr)
    }

    /// This method is part of a family of methods that are similar to the methods from the `Index`
    /// trait. Unlike the index method, this provides control to the map around what gets written.
    /// For example, some registers only have some bits that can be written to. This allows all
    /// other bits to be masked out.
    #[track_caller]
    fn write_byte(&mut self, addr: u16, val: u8) {
        if self.oam_dma.in_conflict(addr) {
            info!("DMA Bus write conflict @ 0x{addr:0>4X}");
            return;
        }
        trace!("Mut index into MemMap: 0x{addr:0>4X}");
        match addr {
            0x0000..=0x7FFF => self.mbc.write_byte(addr, val),
            0x8000..=0x9FFF => self.vram[CpuVramIndex(self.io.vram_select == 1, addr)] = val,

            0xA000..=0xBFFF => self.mbc.write_byte(addr, val),
            0xC000..=0xCFFF | 0xD000..=0xDFFF | 0xFF70 | 0xE000..=0xEFFF | 0xF000..=0xFDFF => {
                self.wram.write_byte(addr, val)
            }
            0xFE00..=0xFE9F => self.vram[CpuOamIndex(addr)] = val,
            // NOTE: This region *should not* actually be accessed
            0xFEA0..=0xFEFF => {}
            0xFF51..=0xFF55 => {
                if self.is_gbc {
                    self.vram_dma.write_byte(addr, val)
                }
            }
            0xFF46 => self.oam_dma.trigger(val),
            0xFF00..=0xFF7F => self.io.write_byte(addr, val),
            0xFF80..=0xFFFE => self.hr[(addr - 0xFF80) as usize] = val,
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
            (None, Some(op)) if ime => Instruction::Interrupt(op),
            _ => parse_instruction(self.read_byte(addr)),
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

    /// This method ticks the memory. The only thing this affects is the divider and timer
    /// registers.
    fn tick(&mut self, ppu: &mut Ppu) {
        self.io.tick(self.speed_mode);
        ppu.tick(self);
        for _ in 0..4 {
            // FIXME: This is incorect. OAM DMA transfers happen immediately, not while being
            // ticked. This needs to be removed.
            if let Some((r, w)) = self.oam_dma.tick() {
                let byte = self.dma_read_byte(r);
                // info!("Transferring byte to OAM @ 0x{r:0>4X}: 0x{byte:0>2X}");
                self.vram.oam[w as usize] = byte;
            }
        }
    }

    fn check_interrupt(&self) -> Option<InterruptOp> {
        match self.ie & self.io.read_byte(0xFF0F) {
            0 => None,
            n => {
                if check_bit_const::<0>(n) {
                    Some(InterruptOp::VBlank)
                } else if check_bit_const::<1>(n) {
                    Some(InterruptOp::LCD)
                } else if check_bit_const::<2>(n) {
                    Some(InterruptOp::Timer)
                } else if check_bit_const::<3>(n) {
                    Some(InterruptOp::Serial)
                } else if check_bit_const::<4>(n) {
                    Some(InterruptOp::Joypad)
                } else {
                    // Technically unreachable
                    None
                }
            }
        }
    }

    fn switch_speeds(&mut self) {
        self.speed_mode = SpeedMode::Double;
    }
}

impl MemoryMap {
    pub fn new(cart: Vec<u8>) -> Self {
        Self {
            mbc: MemoryBankController::new(cart),
            vram: VRam::new(),
            wram: WRam::default(),
            io: IoRegisters::default(),
            hr: [0; 0x7F],
            ie: 0,
            oam_dma: OamDma::new(),
            vram_dma: VramDma::new(),
            speed_mode: SpeedMode::Standard,
            is_gbc: true,
        }
    }

    pub(crate) fn start_up_remap(&mut self) -> StartUpHeaders {
        let mut digest = ([0; 0x100], [0; 0x700]);
        for (i, &byte) in START_UP_HEADER.iter().enumerate().take(0x100) {
            digest.0[i] = byte;
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
        info!("Starting start up unmapping...");
        self.is_gbc = self.io.is_gbc;
        self.wram.is_gbc = self.io.is_gbc;
        for i in 0..=0xFF {
            self.mbc.direct_overwrite(i as u16, &mut headers.0[i]);
        }
        for i in 0..=0x6FF {
            self.mbc
                .direct_overwrite((i + 0x200) as u16, &mut headers.1[i]);
        }
        self.wram = WRam::default();
        info!("Finished start up unmapping!!");
    }

    /// This method only exists to sidestep the "DMA contains" check and should only be called by
    /// the `read_byte` method and during the DMA transfer.
    fn dma_read_byte(&self, index: u16) -> u8 {
        match index {
            0x0000..=0x7FFF => self.mbc.read_byte(index),
            n @ 0x8000..=0x9FFF => self.vram[CpuVramIndex(self.io.vram_select == 1, n)],
            n @ 0xA000..=0xBFFF => self.mbc.read_byte(n),
            0xC000..=0xCFFF | 0xD000..=0xDFFF | 0xE000..=0xEFFF | 0xF000..=0xFDFF | 0xFF70 => {
                self.wram.read_byte(index)
            }
            // Echo RAM
            n @ 0xFE00..=0xFE9F => self.vram[CpuOamIndex(n)],
            // NOTE: This region *should not* actually be accessed, but, instead of panicking, a
            // dead byte will be returned instead.
            0xFEA0..=0xFEFF => 0xFF,
            0xFF51..=0xFF55 => {
                if self.is_gbc {
                    self.vram_dma.read_byte(index)
                } else {
                    0xFF
                }
            }
            0xFF46 => self.oam_dma.register,
            n @ 0xFF00..=0xFF7F => self.io.read_byte(n),
            n @ 0xFF80..=0xFFFE => self.hr[(n - 0xFF80) as usize],
            0xFFFF => self.ie,
        }
    }

    pub(crate) fn write_bytes(&mut self, idx: u16, val: u16) -> u16 {
        todo!()
    }

    pub(crate) fn reset_ppu_status(&mut self) {
        self.io.set_ppu_status(PpuMode::default());
        self.vram.reset_status()
    }

    pub(crate) fn inc_ppu_status(&mut self, state: PpuMode) {
        self.io.set_ppu_status(state);
        self.vram.inc_status(state)
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
        self.io.register_button_input(input)
    }
}

// #[cfg(test)]
impl MemoryMap {
    pub fn rom_mut(&mut self) -> &mut [u8] {
        let MemoryBankController::Direct { rom, .. } = &mut self.mbc else {
            panic!("Do not call MemoryMap::rom unless you called MemoryMap::construct");
        };
        rom.0.as_mut_slice()
    }

    /// Creates a dummy memory map that should only be used for testing. Notably, this will not
    /// have a ROM header, so it is not bootable.
    pub fn construct() -> Self {
        let mbc = MemoryBankController::Direct {
            rom: MemoryBank::new(),
            ram: RamBank::new(),
        };
        Self {
            mbc,
            vram: VRam::new(),
            wram: WRam::default(),
            io: IoRegisters::default(),
            hr: [0; 0x7F],
            ie: 0,
            oam_dma: OamDma::new(),
            vram_dma: VramDma::new(),
            speed_mode: SpeedMode::Standard,
            is_gbc: true,
        }
    }

    pub fn io(&self) -> &IoRegisters {
        &self.io
    }

    pub fn io_mut(&mut self) -> &mut IoRegisters {
        &mut self.io
    }
}

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct WRam {
    /// Selects which of the seven RAM banks that get rotated between is in use. Only the bottom 3
    /// bits are used, both 0 and 1 correspond to bank 1.
    bank_selection: u8,
    static_bank: MemoryBank<0x1000>,
    rotation_bank: [MemoryBank<0x1000>; 7],
    is_gbc: bool,
}

impl WRam {
    fn read_byte(&self, addr: u16) -> u8 {
        match addr {
            0xFF70 => {
                if self.is_gbc {
                    std::cmp::max(self.bank_selection, 1)
                } else {
                    0xFF
                }
            }
            0xC000..0xD000 | 0xE000..0xF000 => self.static_bank[(addr & 0x0FFF) as usize],
            0xD000..0xE000 | 0xF000..0xFE00 => {
                let addr = addr & 0x0FFF;
                // ROM Banks 1-7 are mapped to an array whose indices are 0-6. This performs that
                // mapping while keeping the selection value of 0 as 0.
                // NOTE: `bank_selection` is less than 8 as only the bottom three bits are written
                // to.
                let bank = self.bank_selection.saturating_sub(1);
                self.rotation_bank[bank as usize][addr as usize]
            }
            _ => unreachable!(
                "The WRAM should only be index with 0xFF70, a WRAM address, or an Echo RAM address, not 0x{addr:0>4X}"
            ),
        }
    }

    fn write_byte(&mut self, addr: u16, value: u8) {
        match addr {
            0xFF70 => {
                if self.is_gbc {
                    self.bank_selection = value & 0x07
                }
            }
            0xC000..0xD000 | 0xE000..0xF000 => self.static_bank[(addr & 0x0FFF) as usize] = value,
            0xD000..0xE000 | 0xF000..0xFE00 => {
                let addr = addr & 0x0FFF;
                // See indexing logic in write
                let bank = self.bank_selection.saturating_sub(1);
                self.rotation_bank[bank as usize][addr as usize] = value
            }
            _ => unreachable!(
                "The WRAM should only be index with 0xFF70, a WRAM address, or an Echo RAM address, not 0x{addr:0>4X}"
            ),
        }
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
        // info!("Read at 0x{addr:0>4X} (aka {addr})");
        self[addr as usize]
    }

    fn write_byte(&mut self, addr: u16, val: u8) {
        // info!("Writing {val} to 0x{addr:0>4X} (aka {addr})");
        self[addr as usize] = val;
    }

    fn read_op(&self, addr: u16, _ime: bool) -> Instruction {
        parse_instruction(self.read_byte(addr))
    }

    fn vram_transfer(&mut self) {}

    fn tick(&mut self, _ppu: &mut Ppu) {}

    fn switch_speeds(&mut self) {}

    fn check_interrupt(&self) -> Option<InterruptOp> {
        None
    }
}

// #[cfg(test)]
// impl MemoryLikeExt for Vec<u8> {}
