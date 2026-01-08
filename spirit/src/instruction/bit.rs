use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{op} {bit} {reg}")]
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
    pub(crate) fn execute<M: MemoryLikeExt>(self, state: &mut GameboyState<'_, M>) {
        let BitOp { bit, reg, op } = self;
        match reg {
            RegOrPointer::Reg(reg) => {
                let signal = match op {
                    BitOpInner::Bit => AluSignal::bit(bit, reg),
                    BitOpInner::Res => AluSignal::res(bit, reg),
                    BitOpInner::Set => AluSignal::set(bit, reg),
                };
                state.tick(MCycle::final_with_alu(signal));
            }
            RegOrPointer::Pointer => {
                // We are loading data from memory and performing the operation. This is different
                // that the actual hardware, but this is earier to do than hand-rolling the write
                // on the next cycle.
                let mut cycle = MCycle::load_pointer();
                let signal = match op {
                    BitOpInner::Bit => AluSignal::bit(bit, DataLocation::Bus),
                    BitOpInner::Res => AluSignal::res(bit, DataLocation::Bus),
                    BitOpInner::Set => AluSignal::set(bit, DataLocation::Bus),
                };
                cycle.alu = Some(signal);
                state.tick(cycle);
                state.tick(MCycle::write_pointer());
                state.tick(MCycle::final_cycle());
            }
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self.reg {
            RegOrPointer::Reg(_) => 8,
            RegOrPointer::Pointer => 16,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        2
    }
}
