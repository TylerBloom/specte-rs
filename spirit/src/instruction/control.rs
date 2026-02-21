use crate::cpu::{CpuState, check_bit_const};

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
    pub(crate) fn execute<M: MemoryLike>(self, state: &mut GameboyState<'_, M>) {
        match self {
            ControlOp::Noop => state.tick(MCycle::final_cycle()),
            ControlOp::Halt => {
                state.tick(MCycle::final_cycle());
                state.cpu.state = CpuState::Halted;
            }
            ControlOp::Stop => {
                let speed_control = state.mem.read_byte(0xFF4D);
                // If the bottom bit is set, the speed switch is primed, and, if the top bit is
                // unset, we are in the normal speed. In this case, we will switch speeds.
                state.tick(MCycle::final_cycle());
                state.tick(MCycle::final_cycle());
                if !check_bit_const::<7>(speed_control) && check_bit_const::<0>(speed_control) {
                    state.switch_speeds()
                } else {
                    state.cpu.state = CpuState::Stopped;
                }
            }
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
