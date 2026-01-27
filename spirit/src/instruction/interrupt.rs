use crate::GameboyState;
use crate::cpu::CpuState;
use crate::cpu::FullRegister;
use crate::instruction::AddrAction;
use crate::instruction::DataLocation;
use crate::instruction::IduSignal;
use crate::instruction::MCycle;
use crate::instruction::PointerReg;
use crate::mem::MemoryLike;

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
    pub(crate) fn execute<M: MemoryLike>(self, state: &mut GameboyState<'_, M>) {
        state.cpu.state = CpuState::Running;
        state.tick(MCycle::noop());
        let cycle = MCycle {
            addr_bus: PointerReg::SP,
            action: AddrAction::Noop,
            idu: Some((IduSignal::Dec, FullRegister::SP)),
            alu: None,
        };
        state.tick(cycle);
        let [hi, lo] = (state.cpu.pc - 1u16).0.to_be_bytes();
        let cycle = MCycle {
            addr_bus: PointerReg::SP,
            action: AddrAction::Write(DataLocation::Literal(hi)),
            idu: Some((IduSignal::Dec, FullRegister::SP)),
            alu: None,
        };
        state.tick(cycle);

        // There are a few edge cases where pushing the PC to the stack. The IF or IE can be
        // overwritten. Should this happen, the dispatch checks which address, if any, to set the
        // PC to. If none, the PC is set to 0.
        state.cpu.pc = 0.into();
        state.cpu.ime.reset();
        if let Some(op) = state.mem.check_interrupt() {
            state.mem.clear_interrupt_req(op);
            let addr = op as u16;
            state.cpu.pc = addr.into();
        }

        let cycle = MCycle {
            addr_bus: PointerReg::SP,
            action: AddrAction::Write(DataLocation::Literal(lo)),
            idu: None,
            alu: None,
        };
        state.tick(cycle);

        state.tick(MCycle::final_cycle());
    }
}
