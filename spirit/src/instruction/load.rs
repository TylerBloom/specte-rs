
use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum LoadOp {
    /// Used for opcodes in 0x40..0x80
    #[display("LD {src} -> {dest}")]
    Basic {
        dest: RegOrPointer,
        src: RegOrPointer,
    },
    /// Used for opcodes 0xX1
    #[display("LD 0x{_1:0>4X} -> {_0}")]
    Direct16(WideReg, u16),
    /// Used for opcodes 0x_6 and 0x_E
    #[display("LD 0x{_1:0>2X} -> {_0}")]
    Direct(RegOrPointer, u8),
    /// Used for opcodes 0x_A
    #[display("LD {_0} -> A")]
    LoadIntoA(LoadAPointer),
    /// Used for opcodes 0x_2
    #[display("LD A -> {_0}")]
    StoreFromA(LoadAPointer),
    /// Opcode: 0x08
    /// Store SP & $FF at address n16 and SP >> 8 at address n16 + 1.
    #[display("LD SP -> 0x{_0:0>4X}")]
    StoreSP(u16),
    /// Opcode: 0xF9
    #[display("LD SP -> HL")]
    HLIntoSP,
    /// Opcode: 0xF8
    /// Add the signed value e8 to SP and store the result in HL.
    #[display("LD SP + {_0} -> HL")]
    SPIntoHL(i8),
    /// Used for opcodes 0x_1
    #[display("POP {_0}")]
    Pop(WideRegWithoutSP),
    /// Used for opcodes 0x_5
    #[display("PUSH {_0}")]
    Push(WideRegWithoutSP),
    /// Used for opcode 0xE0
    #[display("LDH A -> 0xFF{_0:0>2X}")]
    LoadHigh(u8),
    /// Used for opcode 0xF0
    #[display("LDH 0xFF{_0:0>2X} -> A")]
    StoreHigh(u8),
    /// Used for opcode 0xE2
    #[display("LDHCA")]
    Ldhca,
    /// Used for opcode 0xF2
    #[display("LDHAC")]
    Ldhac,
    // TODO: These are backwards. For `LoadA`, the ptr is the dest and A is the value. The reverse
    // is true for `StoreA`.
    /// Used for opcode 0xEA
    #[display("LD 0x{ptr:0>4X} -> A")]
    LoadA { ptr: u16 },
    /// Used for opcode 0xFA
    #[display("LD A -> 0x{ptr:0>4X}")]
    StoreA { ptr: u16 },
}

impl LoadOp {
    pub(crate) fn execute(self, state: &mut GameboyState<'_>) {
        match self {
            LoadOp::Basic { dest, src } => todo!(),
            LoadOp::Direct16(wide_reg, _) => todo!(),
            LoadOp::Direct(reg_or_pointer, _) => todo!(),
            LoadOp::LoadIntoA(load_apointer) => todo!(),
            LoadOp::StoreFromA(load_apointer) => todo!(),
            LoadOp::StoreSP(_) => todo!(),
            LoadOp::HLIntoSP => todo!(),
            LoadOp::SPIntoHL(_) => todo!(),
            LoadOp::Pop(wide_reg_without_sp) => todo!(),
            LoadOp::Push(wide_reg_without_sp) => todo!(),
            LoadOp::LoadHigh(_) => todo!(),
            LoadOp::StoreHigh(_) => todo!(),
            LoadOp::Ldhca => todo!(),
            LoadOp::Ldhac => todo!(),
            LoadOp::LoadA { ptr } => todo!(),
            LoadOp::StoreA { ptr } => todo!(),
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            LoadOp::Basic {
                dest: RegOrPointer::Pointer,
                src: RegOrPointer::Pointer,
            } => 4,
            LoadOp::Basic {
                dest: RegOrPointer::Pointer,
                ..
            }
            | LoadOp::Basic {
                src: RegOrPointer::Pointer,
                ..
            } => 8,
            LoadOp::Basic { .. } => 4,
            LoadOp::Direct16(_, _) => 12,
            LoadOp::Direct(RegOrPointer::Pointer, _) => 12,
            LoadOp::Direct(_, _) => 8,
            LoadOp::LoadIntoA(_) => 8,
            LoadOp::StoreFromA(_) => 8,
            LoadOp::StoreSP(_) => 20,
            LoadOp::HLIntoSP => 8,
            LoadOp::SPIntoHL(_) => 12,
            LoadOp::Pop(_) => 12,
            LoadOp::Push(_) => 16,
            LoadOp::LoadHigh(_) => 12,
            LoadOp::StoreHigh(_) => 12,
            LoadOp::Ldhca => 8,
            LoadOp::Ldhac => 8,
            LoadOp::LoadA { .. } => 16,
            LoadOp::StoreA { .. } => 16,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            LoadOp::Basic { .. } => 1,
            LoadOp::Direct16(_, _) => 3,
            LoadOp::Direct(_, _) => 2,
            LoadOp::LoadIntoA(_) => 1,
            LoadOp::StoreFromA(_) => 1,
            LoadOp::StoreSP(_) => 3,
            LoadOp::HLIntoSP => 1,
            LoadOp::SPIntoHL(_) => 2,
            LoadOp::Pop(_) => 1,
            LoadOp::Push(_) => 1,
            LoadOp::LoadHigh(_) => 2,
            LoadOp::StoreHigh(_) => 2,
            LoadOp::Ldhca => 1,
            LoadOp::Ldhac => 1,
            LoadOp::LoadA { .. } => 3,
            LoadOp::StoreA { .. } => 3,
        }
    }
}
