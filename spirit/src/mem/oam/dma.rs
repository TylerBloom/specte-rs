use std::fmt::Display;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct OamDma {
    pub register: u8,
    read_addr: u16,
    write_addr: u16,
    ticks: u16,
    bus: ConflictBus,
}

/// When the OAM DMA is active, the transfer occurs on one of two busses. If a read or write occurs
/// from the CPU (including a push/pop to/from the stack or instruction read), the operation needs
/// to be ignored (reads get 0xFF).
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Serialize, Deserialize, derive_more::Display)]
pub enum ConflictBus {
    Wram,
    Cartridge,
}

impl OamDma {
    pub fn new() -> Self {
        Self {
            register: 0,
            read_addr: 0,
            write_addr: 0,
            ticks: 640,
            bus: ConflictBus::Cartridge,
        }
    }

    /// Calculates if a read/write is happening in region that is being transferred.
    pub fn in_conflict(&self, index: u16) -> bool {
        if self.ticks >= 640 {
            return false;
        }

        // TODO: For the DMG, the only memory that can be accessed is HRAM. It is unclear if that
        // "only" restriction applies to the GBC. That is, if transferring from WRAM, can the CPU
        // *only* access cartridge memory?
        // For now, the less restrictive route will be taken as we can assume that any ROM authors
        // will have taken the correct behavior into account.
        //
        // When transferring from WRAM, the CPU and access cartridge memory (ROM or RAM).
        // When transferring from the cartridge, the CPU can access WRAM.
        //
        // See here: https://gbdev.io/pandocs/OAM_DMA_Transfer.html#oam-dma-bus-conflicts
        match (self.bus, index) {
            (ConflictBus::Wram, 0xC000..0xE000) => true,
            (ConflictBus::Wram, _) => false,
            (ConflictBus::Cartridge, 0x0000..0x8000 | 0xA000..0xC000) => true,
            (ConflictBus::Cartridge, _) => false,
        }
    }

    pub fn trigger(&mut self, value: u8) {
        self.register = value;
        // TODO: Verify that this is correct. The docs say that `value` must be below 0xDF, but
        // this seems like a note for ROM authors not Emu authors.
        self.read_addr = u16::from_be_bytes([value, 0]);
        self.bus = match self.read_addr {
            0xC000..0xE000 => ConflictBus::Wram,
            _ => ConflictBus::Cartridge,
        };
        self.write_addr = 0;
        tracing::info!("Beginning OAM DMA starting at 0x{:0>4X}", self.read_addr);
        self.ticks = 0;
    }

    pub fn tick(&mut self) -> Option<(u16, u16)> {
        if self.ticks == 640 {
            return None;
        }
        self.ticks += 1;
        self.ticks.is_multiple_of(4).then(|| {
            let digest = (self.read_addr, self.write_addr);
            self.read_addr += 1;
            self.write_addr += 1;
            digest
        })
    }
}

impl Default for OamDma {
    fn default() -> Self {
        Self::new()
    }
}

impl Display for OamDma {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            register,
            read_addr,
            write_addr,
            ticks,
            bus,
        } = self;
        writeln!(f, "OamDma {{")?;
        writeln!(f, "  register:   0x{register:0>2X}")?;
        writeln!(f, "  read_addr:  0x{read_addr:0>4X}")?;
        writeln!(f, "  write_addr: 0x{write_addr:0>4X}")?;
        writeln!(f, "  ticks:      {ticks}")?;
        writeln!(f, "  bus:        {bus}")?;
        writeln!(f, "}}")
    }
}
