use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{op} {reg}")]
pub struct BitOp {
    // This is going to be calculated from the op code, which is in the CPU's IR register
    // pub bit: u8,
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
    pub(crate) fn execute<M: MemoryLikeExt>(self, mut state: GameboyState<'_, M>) {
        let BitOp { bit, reg, op } = self;
        match op {
            BitOpInner::Bit => {
                todo!()
            }
            BitOpInner::Res => {
                todo!()
            }
            BitOpInner::Set => {
                todo!()
            }
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        8 + 8 * (self.reg.is_pointer() as u8)
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        2
    }
}
