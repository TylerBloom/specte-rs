use std::ops::{Index, IndexMut};

use serde::{Deserialize, Serialize};
use serde_with::serde_as;
use tracing::{info, trace};

use super::{
    io::{BgPaletteIndex, ObjPaletteIndex},
    BgTileDataIndex, BgTileDataInnerIndex, BgTileMapAttrIndex, BgTileMapAttrInnerIndex,
    BgTileMapIndex, BgTileMapInnerIndex, OamObjectIndex, ObjTileDataIndex,
};

static DEAD_READ_ONLY_BYTE: u8 = 0xFF;

/// This wrapper type is used to communicate that the VRAM should be indexed into when indexing into VRam.
/// Since there is state that determines what gets indexed into, this type is used rather than
/// making the field `pub(crate)`/`pub(super)`.
pub(super) struct CpuVramIndex(pub bool, pub u16);

/// This wrapper type is used to communicate that the OAM should be indexed into when indexing into VRam.
/// Since there is state that determines what gets indexed into, this type is used rather than
/// making the field `pub(crate)`/`pub(super)`.
pub(super) struct CpuOamIndex(pub u16);

#[repr(u8)]
#[derive(
    Debug,
    Default,
    Clone,
    Copy,
    Hash,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    derive_more::IsVariant,
    Serialize,
    Deserialize,
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

#[serde_as]
#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub struct VRam {
    /// The main video RAM. Accessible through the address range 0x8000 through 0x9FFF.
    #[serde(serialize_with = "crate::utils::serialize_slices_as_one")]
    #[serde(deserialize_with = "crate::utils::deserialize_slices_as_one")]
    pub vram: [[u8; 0x2000]; 2],
    /// The Object Attribute Map. Accessible through the address range 0xFE00 through 0xFE9F
    #[serde_as(as = "serde_with::Bytes")]
    pub oam: [u8; 0xA0],
    /// The status that the PPU is currently in. This mode is set when the PPU is ticked and
    /// determines how the VRAM and OAM are indexed into.
    pub status: PpuMode,
    /// Because we need to return a reference to some byte when indexing but the state of the PPU
    /// might restrict reads/write, this byte is used in those cases.
    dead_byte: u8,
}

impl VRam {
    pub(super) fn new() -> Self {
        Self {
            vram: [[0; 0x2000]; 2],
            oam: [0; 0xA0],
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

impl Index<CpuVramIndex> for VRam {
    type Output = u8;

    fn index(&self, CpuVramIndex(bank, index): CpuVramIndex) -> &Self::Output {
        if self.status.is_drawing() {
            println!("Attempting to read from VRAM while locked!!!");
            &DEAD_READ_ONLY_BYTE
        } else {
            if bank {
                &self.vram[1][index as usize - 0x8000]
            } else {
                &self.vram[0][index as usize - 0x8000]
            }
        }
    }
}

impl IndexMut<CpuVramIndex> for VRam {
    fn index_mut(&mut self, CpuVramIndex(bank, index): CpuVramIndex) -> &mut Self::Output {
        if self.status.is_drawing() {
            println!("Attempting to read from VRAM while locked!!!");
            self.dead_byte = 0xFF;
            &mut self.dead_byte
        } else {
            if bank {
                &mut self.vram[1][index as usize - 0x8000]
            } else {
                &mut self.vram[0][index as usize - 0x8000]
            }
        }
    }
}

impl Index<CpuOamIndex> for VRam {
    type Output = u8;

    fn index(&self, CpuOamIndex(index): CpuOamIndex) -> &Self::Output {
        trace!("Indexing into OAM @ 0x{index:0>4X}");
        if matches!(self.status, PpuMode::OamScan | PpuMode::Drawing) {
            &DEAD_READ_ONLY_BYTE
        } else {
            &self.oam[index as usize - 0xFE00]
        }
    }
}

impl IndexMut<CpuOamIndex> for VRam {
    fn index_mut(&mut self, CpuOamIndex(index): CpuOamIndex) -> &mut Self::Output {
        trace!("Mutably indexing into OAM @ 0x{index:0>4X}");
        if matches!(self.status, PpuMode::OamScan | PpuMode::Drawing) {
            self.dead_byte = 0xFF;
            &mut self.dead_byte
        } else {
            &mut self.oam[index as usize - 0xFE00]
        }
    }
}

/* --------- Indexing types use by the PPU --------- */

impl Index<OamObjectIndex> for VRam {
    type Output = [u8; 4];

    fn index(&self, OamObjectIndex(index): OamObjectIndex) -> &Self::Output {
        // There are only 40 objects in the OAM and this type indexes those object, not their
        // memory address.
        debug_assert!(index < 40);
        let index = 4 * index as usize;
        (&self.oam[index..(index + 4)]).try_into().unwrap()
    }
}

impl Index<(ObjTileDataIndex, bool)> for VRam {
    /// We return a slice here because the object could be either 8x8 or 8x16, and this is
    /// determined by the state of a LCDC bit. It is the job of the PPU to check the length of the
    /// slice and handle it accordingly.
    type Output = [u8];

    fn index(
        &self,
        (ObjTileDataIndex(index, bank), size): (ObjTileDataIndex, bool),
    ) -> &Self::Output {
        let bank = if bank { &self.vram[1] } else { &self.vram[0] };
        let start = 16 * (if size { index & !0b1 } else { index } as usize);
        let end = start + 16 + if size { 16 } else { 0 };
        &bank[start..end]
    }
}

impl Index<BgTileMapInnerIndex> for VRam {
    type Output = u8;

    fn index(
        &self,
        BgTileMapInnerIndex { second_map, x, y }: BgTileMapInnerIndex,
    ) -> &Self::Output {
        let x = x as usize / 8;
        let y = y as usize / 8;
        let index = 0x1800 + (second_map as usize * 0x400) + (y * 32) + x;
        &self.vram[0][index]
    }
}

impl Index<BgTileMapAttrInnerIndex> for VRam {
    type Output = u8;

    fn index(
        &self,
        BgTileMapAttrInnerIndex { x, y, second_map }: BgTileMapAttrInnerIndex,
    ) -> &Self::Output {
        let x = x as usize / 8;
        let y = y as usize / 8;
        let index = 0x1800 + (second_map as usize * 0x400) + (y * 32) + x;
        &self.vram[1][index]
    }
}

impl Index<BgTileDataInnerIndex> for VRam {
    type Output = [u8; 16];

    fn index(
        &self,
        BgTileDataInnerIndex {
            unsigned_indexing,
            index,
            bank,
        }: BgTileDataInnerIndex,
    ) -> &Self::Output {
        let index = if unsigned_indexing {
            16 * index as usize
        } else {
            if index > 127 {
                let offset = (index as i8) as isize;
                0x1000usize.wrapping_add_signed(16 * offset)
            } else {
                0x1000 + (16 * index as usize)
            }
        };
        let bank = &self.vram[bank as usize];
        (&bank[index..index + 16]).try_into().unwrap()
    }
}
