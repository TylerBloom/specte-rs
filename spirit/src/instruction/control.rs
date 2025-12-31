use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum ControlOp {
    #[display("HALT")]
    Halt,
    #[display("NOOP")]
    Noop,
    #[display("Stop")]
    Stop,
}

impl ControlOp {
    pub(crate) fn execute<M: MemoryLikeExt>(self, mut state: GameboyState<'_, M>) {
        match self {
            ControlOp::Noop => state.tick(MCycle::final_cycle()),
            ControlOp::Halt => todo!(),
            ControlOp::Stop => todo!(),
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            ControlOp::Noop => 4,
            ControlOp::Stop => 4,
            ControlOp::Halt => 4,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            ControlOp::Noop => 1,
            ControlOp::Stop => 2,
            ControlOp::Halt => 1,
        }
    }
}
