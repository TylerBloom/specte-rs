use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum BitShiftOp {
    #[display("RL {_0}")]
    Rl(RegOrPointer),
    #[display("RLC {_0}")]
    Rlc(RegOrPointer),
    #[display("RR {_0}")]
    Rr(RegOrPointer),
    #[display("RRC {_0}")]
    Rrc(RegOrPointer),
    #[display("SLA {_0}")]
    Sla(RegOrPointer),
    #[display("SRA {_0}")]
    Sra(RegOrPointer),
    #[display("SWAP {_0}")]
    Swap(RegOrPointer),
    #[display("SRL {_0}")]
    Srl(RegOrPointer),
}

impl BitShiftOp {
    pub(crate) fn execute<M: MemoryLikeExt>(self, state: GameboyState<'_, M>) {
        match self {
            BitShiftOp::Rlc(reg_or_pointer) => todo!(),
            BitShiftOp::Rl(reg_or_pointer) => todo!(),
            BitShiftOp::Rr(reg_or_pointer) => todo!(),
            BitShiftOp::Rrc(reg_or_pointer) => todo!(),
            BitShiftOp::Sla(reg_or_pointer) => todo!(),
            BitShiftOp::Sra(reg_or_pointer) => todo!(),
            BitShiftOp::Swap(reg_or_pointer) => todo!(),
            BitShiftOp::Srl(reg_or_pointer) => todo!(),
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            BitShiftOp::Rlc(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rlc(_) => 8,
            BitShiftOp::Rrc(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rrc(_) => 8,
            BitShiftOp::Rl(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rl(_) => 8,
            BitShiftOp::Rr(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rr(_) => 8,
            BitShiftOp::Sla(RegOrPointer::Pointer) => 16,
            BitShiftOp::Sla(_) => 8,
            BitShiftOp::Sra(RegOrPointer::Pointer) => 16,
            BitShiftOp::Sra(_) => 8,
            BitShiftOp::Swap(RegOrPointer::Pointer) => 16,
            BitShiftOp::Swap(_) => 8,
            BitShiftOp::Srl(RegOrPointer::Pointer) => 16,
            BitShiftOp::Srl(_) => 8,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        2
    }
}
