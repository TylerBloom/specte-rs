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
    pub(crate) fn execute(self, state: &mut GameboyState<'_>) {
        let BitOp { bit, reg, op } = self;
        debug_assert!(bit < 8);
        match op {
            BitOpInner::Bit => {
                // self.f.z = !check_bit(bit, byte);
                // self.f.n = false;
                // self.f.h = true;
                todo!()
            }
            BitOpInner::Res => {
                // Read the OP code
                state.tick();
                state.tick();

                // Inc PC
                state.cpu.inc_pc();
                state.tick();
                state.tick();

                match reg {
                    RegOrPointer::Reg(reg) => {
                        // Read the OP code
                        state.tick();
                        state.tick();

                        // Inc PC
                        state.cpu.inc_pc();
                        state.tick();
                        state.tick();

                        // Do actual work
                        state.cpu[reg] &= !(0x1 << bit);
                        state.tick();
                    }
                    RegOrPointer::Pointer => {
                        // Read the OP code
                        state.tick();

                        // Inc PC
                        let ptr state.cpu.ptr();
                        let z = state.mem[ptr];
                        state.tick();
                        state.tick();
                        state.tick();

                        // Do actual work
                        state.cpu[reg] &= !(0x1 << bit);
                        state.tick();

                        // Read the OP code
                        state.tick();
                        state.tick();

                        // Inc PC
                        state.cpu.inc_pc();
                        state.tick();
                        state.tick();
                    }
                }

                // self.update_byte(reg, mem, |byte| *byte &= !(0x1 << bit));
            }
            BitOpInner::Set => {
                // self.update_byte(reg, mem, |byte| *byte |= 0x1 << bit);
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
