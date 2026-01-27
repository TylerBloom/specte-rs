use crate::GameboyState;
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
        state.tick(MCycle::noop());
        state.tick(MCycle::noop());
        let [hi, lo] = state.cpu.pc.0.to_be_bytes();
        let cycle = MCycle {
            addr_bus: PointerReg::SP,
            action: AddrAction::Write(DataLocation::Literal(hi)),
            idu: Some((IduSignal::Inc, FullRegister::SP)),
            alu: None,
        };
        state.tick(cycle);
        let cycle = MCycle {
            addr_bus: PointerReg::SP,
            action: AddrAction::Write(DataLocation::Literal(lo)),
            idu: Some((IduSignal::Inc, FullRegister::SP)),
            alu: None,
        };
        state.tick(cycle);
        // There is an edge case where pushing the PC to the stack can overwrite the IF register.
        // If this happens, the PC is not changed.
        if state.mem.read_byte(0xFF0F) != 0 {
            state.cpu.ime = false;
            state.mem.clear_interrupt_req(self);
            let addr = self as u16;
            state.tick(MCycle::noop());
            state.cpu.pc = addr.into();
        }
    }
}
