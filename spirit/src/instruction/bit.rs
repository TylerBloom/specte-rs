
use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{op} {bit} {reg}")]
pub struct BitOp {
    pub bit: u8,
    pub reg: RegOrPointer,
    pub op: BitOpInner,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum BitOpInner {
    #[display("BIT")]
    Bit,
    #[display("RES")]
    Res,
    #[display("SET")]
    Set,
}

impl BitOp {
    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        8 + 8 * (self.reg.is_pointer() as u8)
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        2
    }
}
