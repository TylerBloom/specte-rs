use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum BitShiftOp {
    #[display("RLC {_0}")]
    Rlc(RegOrPointer),
    #[display("RLCA")]
    Rlca,
    #[display("RRC {_0}")]
    Rrc(RegOrPointer),
    #[display("RRCA")]
    Rrca,
    #[display("RL {_0}")]
    Rl(RegOrPointer),
    #[display("RLA")]
    Rla,
    #[display("RR {_0}")]
    Rr(RegOrPointer),
    #[display("RRA")]
    Rra,
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
    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            BitShiftOp::Rlca => 4,
            BitShiftOp::Rlc(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rlc(_) => 8,
            BitShiftOp::Rrca => 4,
            BitShiftOp::Rrc(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rrc(_) => 8,
            BitShiftOp::Rl(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rl(_) => 8,
            BitShiftOp::Rla => 4,
            BitShiftOp::Rr(RegOrPointer::Pointer) => 16,
            BitShiftOp::Rr(_) => 8,
            BitShiftOp::Rra => 4,
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
        match self {
            BitShiftOp::Rlca => 1,
            BitShiftOp::Rlc(_) => 2,
            BitShiftOp::Rrca => 1,
            BitShiftOp::Rrc(_) => 2,
            BitShiftOp::Rla => 1,
            BitShiftOp::Rl(_) => 2,
            BitShiftOp::Rra => 1,
            BitShiftOp::Rr(_) => 2,
            BitShiftOp::Sla(_) => 2,
            BitShiftOp::Sra(_) => 2,
            BitShiftOp::Swap(_) => 2,
            BitShiftOp::Srl(_) => 2,
        }
    }
}
