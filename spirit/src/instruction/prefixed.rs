use crate::{
    GameboyState,
    instruction::{BitOp, BitShiftOp},
    mem::MemoryLikeExt,
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum PrefixedInstruction {
    BitShift(BitShiftOp),
    Bit(BitOp),
}

impl PrefixedInstruction {
    pub(crate) fn execute<M: MemoryLikeExt>(self, mut state: GameboyState<'_, M>) {
        match self {
            PrefixedInstruction::BitShift(op) => op.execute(state),
            PrefixedInstruction::Bit(op) => op.execute(state),
        }
    }
}
