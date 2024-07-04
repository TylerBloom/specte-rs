// TODO: There are periods during which the CPU can not access data in VRAM (and a few other
// places) because the PPU might be accessing those areas. Any writes to that area are ignored and
// any reads return garbage data. To simulate this, the VRAM should be moved into its own type that
// contains the RAM, a flag (or maybe a few) to indicate the status, and a dead byte to use for
// indexing. When the PPU is ticked, it will need access to this object in order to set these
// values.

use std::ops::{Index, IndexMut};

static DEAD_READ_ONLY_BYTE: u8 = 0xFF;

/// This wrapper type is used to communicate that the VRAM should be indexed into when indexing into VRam.
/// Since there is state that determines what gets indexed into, this type is used rather than
/// making the field `pub(crate)`/`pub(super)`.
pub(super) struct VramIndex(pub u16);

/// This wrapper type is used to communicate that the OAM should be indexed into when indexing into VRam.
/// Since there is state that determines what gets indexed into, this type is used rather than
/// making the field `pub(crate)`/`pub(super)`.
pub(super) struct OamIndex(pub u16);

#[repr(u8)]
#[derive(
    Debug, Default, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, derive_more::IsVariant,
)]
pub(crate) enum PpuMode {
    /// Also refered to as "Mode 2" in the pandocs.
    #[default]
    OamScan = 0,
    /// Also refered to as "Mode 3" in the pandocs.
    Drawing = 1,
    /// Also refered to as "Mode 0" in the pandocs.
    HBlank = 2,
    /// Also refered to as "Mode 1" in the pandocs.
    VBlank = 3,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub(crate) struct VRam {
    // TODO: This is a GBC emulator, show it needs to support switching between banks 0 and 1.
    /// The main video RAM. Accessible through the address range 0x8000 through 0x9FFF.
    pub(crate) vram: ([u8; 0x2000], [u8; 0x2000]),
    /// The Object Attribute Map. Accessible through the address range 0xFE00 through 0xFE9F
    pub(crate) oam: [u8; 0x100],
    /// The status that the PPU is currently in. This mode is set when the PPU is ticked and
    /// determines how the VRAM and OAM are indexed into.
    status: PpuMode,
    /// Because we need to return a reference to some byte when indexing but the state of the PPU
    /// might restrict reads/write, this byte is used in those cases.
    dead_byte: u8,
}

impl VRam {
    pub(super) fn new() -> Self {
        Self {
            vram: ([0; 0x2000], [0; 0x2000]),
            oam: [0; 0x100],
            status: PpuMode::default(),
            dead_byte: 0xFF,
        }
    }

    pub(crate) fn inc_status(&mut self, other: PpuMode) {
        self.status = std::cmp::max(self.status, other);
    }

    pub(crate) fn reset_status(&mut self) {
        self.status = PpuMode::default();
    }
}

impl Index<VramIndex> for VRam {
    type Output = u8;

    fn index(&self, VramIndex(index): VramIndex) -> &Self::Output {
        println!("Indexing into VRAM @ 0x{index:0>4X}");
        if self.status.is_drawing() {
            &DEAD_READ_ONLY_BYTE
        } else {
            &self.vram.0[index as usize - 0x8000]
        }
    }
}

impl IndexMut<VramIndex> for VRam {
    fn index_mut(&mut self, VramIndex(index): VramIndex) -> &mut Self::Output {
        println!("Mutably indexing into OAM @ 0x{index:0>4X}");
        if self.status.is_drawing() {
            self.dead_byte = 0xFF;
            &mut self.dead_byte
        } else {
            &mut self.vram.0[index as usize - 0x8000]
        }
    }
}

impl Index<OamIndex> for VRam {
    type Output = u8;

    fn index(&self, OamIndex(index): OamIndex) -> &Self::Output {
        println!("Indexing into OAM @ 0x{index:0>4X}");
        if matches!(self.status, PpuMode::OamScan | PpuMode::Drawing) {
            &DEAD_READ_ONLY_BYTE
        } else {
            &self.oam[index as usize - 0xFE00]
        }
    }
}

impl IndexMut<OamIndex> for VRam {
    fn index_mut(&mut self, OamIndex(index): OamIndex) -> &mut Self::Output {
        println!("Mutably indexing into OAM @ 0x{index:0>4X}");
        if matches!(self.status, PpuMode::OamScan | PpuMode::Drawing) {
            self.dead_byte = 0xFF;
            &mut self.dead_byte
        } else {
            &mut self.oam[index as usize - 0xFE00]
        }
    }
}
