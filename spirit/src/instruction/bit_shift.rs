use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{op} {reg}")]
pub struct BitShiftOp {
    pub reg: RegOrPointer,
    pub op: BitShiftOpInner,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum BitShiftOpInner {
    #[display("RL")]
    Rl,
    #[display("RLC")]
    Rlc,
    #[display("RR")]
    Rr,
    #[display("RRC")]
    Rrc,
    #[display("SLA")]
    Sla,
    #[display("SRA")]
    Sra,
    #[display("SWAP")]
    Swap,
    #[display("SRL")]
    Srl,
}

impl BitShiftOp {
    pub(crate) fn execute<M: MemoryLikeExt>(self, state: &mut GameboyState<'_, M>) {
        let Self { reg, op } = self;
        let op = match op {
            BitShiftOpInner::Rl => AluOp::Rl,
            BitShiftOpInner::Rlc => AluOp::Rlc,
            BitShiftOpInner::Rr => AluOp::Rr,
            BitShiftOpInner::Rrc => AluOp::Rrc,
            BitShiftOpInner::Sla => AluOp::Sla,
            BitShiftOpInner::Sra => AluOp::Sra,
            BitShiftOpInner::Swap => AluOp::Swap,
            BitShiftOpInner::Srl => AluOp::Srl,
        };
        match reg {
            RegOrPointer::Reg(reg) => {
                let signal = AluSignal {
                    input_one: reg.into(),
                    input_two: reg.into(),
                    op,
                    output: reg.into(),
                };
                state.tick(MCycle::final_with_alu(signal))
            }
            RegOrPointer::Pointer => {
                // We are loading data from memory and performing the operation. This is different
                // that the actual hardware, but this is earier to do than hand-rolling the write
                // on the next cycle.
                let mut cycle = MCycle::load_pointer();
                let signal = AluSignal {
                    input_one: DataLocation::Bus,
                    input_two: DataLocation::Bus,
                    op,
                    output: DataLocation::Bus,
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
