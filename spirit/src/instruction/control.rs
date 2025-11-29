use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum ControlOp {
    #[display("HALT")]
    Halt,
    #[display("NOOP")]
    Noop,
    #[display("Stop 0x{_0:0>2X}")]
    Stop(u8),
}

impl ControlOp {
    pub(crate) fn execute(self, state: &mut GameboyState<'_>) {
        match self {
            ControlOp::Halt => todo!(),
            ControlOp::Noop => todo!(),
            ControlOp::Stop(_) => todo!(),
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            ControlOp::Noop => 4,
            ControlOp::Stop(_) => 4,
            ControlOp::Halt => 4,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            ControlOp::Noop => 1,
            ControlOp::Stop(_) => 2,
            ControlOp::Halt => 1,
        }
    }
}
