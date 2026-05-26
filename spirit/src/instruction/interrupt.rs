use crate::GameboyState;
use crate::cpu::CpuState;
use crate::cpu::FullRegister;
use crate::instruction::AddrAction;
use crate::instruction::DataLocation;
use crate::instruction::IduSignal;
use crate::instruction::MCycle;
use crate::instruction::PointerReg;
use crate::mem::MemoryLikeExt;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
#[repr(u16)]
pub enum InterruptOp {
    #[display("VBlank")]
    VBlank = 0x0040,
    #[display("LCD")]
    LCD = 0x0048,
    #[display("Timer")]
    Timer = 0x0050,
    #[display("Serial")]
    Serial = 0x0058,
    #[display("Joypad")]
    Joypad = 0x0060,
}

impl InterruptOp {
    pub(crate) fn execute<M: MemoryLikeExt>(self, state: &mut GameboyState<'_, M>) {
        state.cpu.state = CpuState::Running;
        state.tick(MCycle::noop());
        let cycle = MCycle {
            addr_bus: PointerReg::SP,
            action: AddrAction::Noop,
            idu: Some((IduSignal::Dec, FullRegister::SP)),
            alu: None,
        };
        state.tick(cycle);
        let [s, _] = state.cpu.pc.0.to_be_bytes();
        let cycle = MCycle {
            addr_bus: PointerReg::SP,
            action: AddrAction::Write(DataLocation::Literal(s)),
            idu: Some((IduSignal::Dec, FullRegister::SP)),
            alu: None,
        };
        state.tick(cycle);
        let [_, p] = state.cpu.pc.0.to_be_bytes();
        let cycle = MCycle {
            addr_bus: PointerReg::SP,
            action: AddrAction::Write(DataLocation::Literal(p)),
            idu: None,
            alu: None,
        };
        state.tick(cycle);
        // There is an edge case where pushing the PC to the stack can overwrite the IF register.
        // If this happens, the PC is not changed.
        if state.mem.read_byte(0xFF0F) != 0 {
            state.cpu.ime.reset();
            state.mem.clear_interrupt_req(self);
            // FIXME: I think is incorrect. Once the jump is made, we need to load the byte that we
            // are now pointing at into the IR. Othewise, the instruction that was in the IR will
            // be the first to be ran after the jump. This can cause a whole mess of problems:
            // - If the would-have-been-next instruction were a relative jump, we could be jumping into an
            // abritary byte.
            // - Something similar can happen if the executed instruction were a different length
            // than the first instruction after the jump to the intrrupt's position.
            // ^^ These should be test cases.
            let addr = self as u16;
            state.cpu.pc = addr.into();
            state.tick(MCycle::final_cycle());
        }
    }
}
