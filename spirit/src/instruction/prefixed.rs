use crate::GameboyState;
use crate::instruction::BitOp;
use crate::instruction::BitShiftOp;
use crate::mem::MemoryLike;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum PrefixedInstruction {
    BitShift(BitShiftOp),
    Bit(BitOp),
}

impl PrefixedInstruction {
    pub(crate) fn execute<M: MemoryLike>(self, state: &mut GameboyState<'_, M>) {
        tracing::info!("Executing prefixed {self}");
        match self {
            PrefixedInstruction::BitShift(op) => op.execute(state),
            PrefixedInstruction::Bit(op) => op.execute(state),
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            PrefixedInstruction::BitShift(op) => op.length(),
            PrefixedInstruction::Bit(op) => op.length(),
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            PrefixedInstruction::BitShift(op) => op.size(),
            PrefixedInstruction::Bit(op) => op.size(),
        }
    }
}
