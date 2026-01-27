use crate::utils::Wrapping;

use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum JumpOp {
    /// Op Code: 0x18
    #[display("JR")]
    Relative,
    /// Op Codes: 0x20, 0x30, 0x28, 0x38
    #[display("JR {_0}")]
    ConditionalRelative(Condition),
    /// Op Code: 0xC3
    #[display("JP")]
    Absolute,
    /// Op Codes: 0xC2, 0xD2, 0xCA, 0xDA
    #[display("JP {_0}")]
    ConditionalAbsolute(Condition),
    /// Op Code: 0xE9
    #[display("JP HL")]
    JumpToHL,
    /// Op Code: 0xCD
    #[display("CALL ")]
    Call,
    /// Op Codes: 0xC4, 0xD4, 0xCC, 0xDC
    #[display("CALL {_0}")]
    ConditionalCall(Condition),
    /// Op Code: 0xC9
    #[display("RET")]
    Return,
    /// Op Codes: 0xC0, 0xD0, 0xC8, 0xD8
    #[display("RET {_0}")]
    ConditionalReturn(Condition),
    /// Op Code: 0xD9
    /// Return from the subroutine and enable intrupts
    #[display("RETI")]
    ReturnAndEnable,
    /// Op Code: 0xC7
    #[display("RST 0x00")]
    RST00,
    /// Op Code: 0xCF
    #[display("RST 0x08")]
    RST08,
    /// Op Code: 0xD7
    #[display("RST 0x10")]
    RST10,
    /// Op Code: 0xDF
    #[display("RST 0x18")]
    RST18,
    /// Op Code: 0xE7
    #[display("RST 0x20")]
    RST20,
    /// Op Code: 0xEF
    #[display("RST 0x28")]
    RST28,
    /// Op Code: 0xF7
    #[display("RST 0x30")]
    RST30,
    /// Op Code: 0xFF
    #[display("RST 0x38")]
    RST38,
}

impl JumpOp {
    pub(crate) fn execute<M: MemoryLike>(self, state: &mut GameboyState<'_, M>) {
        fn vector_jump<const ADDR: u16, M: MemoryLike>(state: &mut GameboyState<'_, M>) {
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
            state.cpu.pc = ADDR.into();

            state.tick(MCycle::final_cycle());
        }
        match self {
            JumpOp::Relative => {
                state.tick(MCycle::load_pc());

                state.tick(MCycle::noop());
                let pc = state.cpu.pc.0;
                let z = state.cpu.z.0;
                let [w, z] = pc
                    .wrapping_add_signed((z as i8) as i16)
                    .to_be_bytes()
                    .map(Wrapping);
                state.cpu.w = w;
                state.cpu.z = z;

                let mut cycle = MCycle::final_cycle();
                cycle.addr_bus = PointerReg::Ghost;
                state.tick(cycle);
            }
            JumpOp::ConditionalRelative(cond) => {
                state.tick(MCycle::load_pc());
                if cond.passed(state.cpu) {
                    state.tick(MCycle::noop());
                    let pc = state.cpu.pc.0;
                    let z = state.cpu.z.0;
                    let [w, z] = pc
                        .wrapping_add_signed((z as i8) as i16)
                        .to_be_bytes()
                        .map(Wrapping);
                    state.cpu.w = w;
                    state.cpu.z = z;
                    let mut cycle = MCycle::final_cycle();
                    cycle.addr_bus = PointerReg::Ghost;
                    state.tick(cycle);
                } else {
                    state.tick(MCycle::final_cycle());
                }
            }
            JumpOp::Absolute => {
                state.tick(MCycle::load_pc());
                let z = state.cpu.z;
                state.tick(MCycle::load_pc());
                let w = state.cpu.z;
                state.cpu.z = z;
                state.cpu.w = w;
                state.tick(MCycle::noop());
                let ghost = state.cpu.ghost_addr();
                state.cpu.pc = ghost.into();
                state.tick(MCycle::final_cycle());
            }
            JumpOp::ConditionalAbsolute(cond) => {
                state.tick(MCycle::load_pc());
                let z = state.cpu.z;
                state.tick(MCycle::load_pc());
                let w = state.cpu.z;
                state.cpu.z = z;
                state.cpu.w = w;
                if cond.passed(state.cpu) {
                    state.tick(MCycle::noop());
                    let ghost = state.cpu.ghost_addr();
                    state.cpu.pc = ghost.into();
                }
                state.tick(MCycle::final_cycle());
            }
            JumpOp::JumpToHL => {
                let cycle = MCycle {
                    addr_bus: PointerReg::HL,
                    action: AddrAction::Read(ReadLocation::InstrRegister),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
            }
            JumpOp::Call => {
                state.tick(MCycle::load_pc());
                let z = state.cpu.z;
                state.tick(MCycle::load_pc());
                let w = state.cpu.z;
                state.cpu.z = z;
                state.cpu.w = w;

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
                let ghost = state.cpu.ghost_addr();
                state.cpu.pc = ghost.into();

                state.tick(MCycle::final_cycle());
            }
            JumpOp::ConditionalCall(cond) => {
                state.tick(MCycle::load_pc());
                let z = state.cpu.z;
                state.tick(MCycle::load_pc());
                let w = state.cpu.z;
                state.cpu.z = z;
                state.cpu.w = w;

                if cond.passed(state.cpu) {
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
                    let ghost = state.cpu.ghost_addr();
                    state.cpu.pc = ghost.into();
                }

                state.tick(MCycle::final_cycle());
            }
            JumpOp::Return => {
                let cycle = MCycle {
                    addr_bus: PointerReg::SP,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: Some((IduSignal::Inc, FullRegister::SP)),
                    alu: None,
                };
                state.tick(cycle);
                let cycle = MCycle {
                    addr_bus: PointerReg::SP,
                    action: AddrAction::Read(ReadLocation::RegisterW),
                    idu: Some((IduSignal::Inc, FullRegister::SP)),
                    alu: None,
                };
                state.tick(cycle);

                state.tick(MCycle::noop());
                let ghost = state.cpu.ghost_addr();
                state.cpu.pc = ghost.into();

                state.tick(MCycle::final_cycle());
            }
            JumpOp::ConditionalReturn(cond) => {
                state.tick(MCycle::noop());
                if cond.passed(state.cpu) {
                    let cycle = MCycle {
                        addr_bus: PointerReg::SP,
                        action: AddrAction::Read(ReadLocation::RegisterZ),
                        idu: Some((IduSignal::Inc, FullRegister::SP)),
                        alu: None,
                    };
                    state.tick(cycle);
                    let cycle = MCycle {
                        addr_bus: PointerReg::SP,
                        action: AddrAction::Read(ReadLocation::RegisterW),
                        idu: Some((IduSignal::Inc, FullRegister::SP)),
                        alu: None,
                    };
                    state.tick(cycle);

                    state.tick(MCycle::noop());
                    let ghost = state.cpu.ghost_addr();
                    state.cpu.pc = ghost.into();
                }
                state.tick(MCycle::final_cycle());
            }
            JumpOp::ReturnAndEnable => {
                let cycle = MCycle {
                    addr_bus: PointerReg::SP,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: Some((IduSignal::Inc, FullRegister::SP)),
                    alu: None,
                };
                state.tick(cycle);
                let cycle = MCycle {
                    addr_bus: PointerReg::SP,
                    action: AddrAction::Read(ReadLocation::RegisterW),
                    idu: Some((IduSignal::Inc, FullRegister::SP)),
                    alu: None,
                };
                state.tick(cycle);

                state.tick(MCycle::noop());
                let ghost = state.cpu.ghost_addr();
                state.cpu.pc = ghost.into();
                state.cpu.to_set_ime = true;

                state.tick(MCycle::final_cycle());
            }
            JumpOp::RST00 => vector_jump::<0x00, _>(state),
            JumpOp::RST08 => vector_jump::<0x08, _>(state),
            JumpOp::RST10 => vector_jump::<0x10, _>(state),
            JumpOp::RST18 => vector_jump::<0x18, _>(state),
            JumpOp::RST20 => vector_jump::<0x20, _>(state),
            JumpOp::RST28 => vector_jump::<0x28, _>(state),
            JumpOp::RST30 => vector_jump::<0x30, _>(state),
            JumpOp::RST38 => vector_jump::<0x38, _>(state),
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self, cpu: &Cpu) -> u8 {
        match self {
            JumpOp::ConditionalRelative(cond) => 8 + (4 * cond.passed(cpu) as u8),
            JumpOp::Relative => 12,
            JumpOp::ConditionalAbsolute(cond) => 12 + (4 * cond.passed(cpu) as u8),
            JumpOp::Absolute => 16,
            JumpOp::JumpToHL => 4,
            JumpOp::Call => 24,
            JumpOp::ConditionalCall(cond) => 12 + (12 * cond.passed(cpu) as u8),
            JumpOp::Return => 16,
            JumpOp::ConditionalReturn(cond) => 8 + (12 * cond.passed(cpu) as u8),
            JumpOp::ReturnAndEnable => 16,
            JumpOp::RST00 => 16,
            JumpOp::RST08 => 16,
            JumpOp::RST10 => 16,
            JumpOp::RST18 => 16,
            JumpOp::RST20 => 16,
            JumpOp::RST28 => 16,
            JumpOp::RST30 => 16,
            JumpOp::RST38 => 16,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            JumpOp::ConditionalRelative(_) => 2,
            JumpOp::Relative => 2,
            JumpOp::ConditionalAbsolute(_) => 3,
            JumpOp::Absolute => 3,
            JumpOp::JumpToHL => 1,
            JumpOp::Call => 3,
            JumpOp::ConditionalCall(_) => 3,
            JumpOp::Return => 1,
            JumpOp::ConditionalReturn(_) => 1,
            JumpOp::ReturnAndEnable => 1,
            JumpOp::RST00 => 1,
            JumpOp::RST08 => 1,
            JumpOp::RST10 => 1,
            JumpOp::RST18 => 1,
            JumpOp::RST20 => 1,
            JumpOp::RST28 => 1,
            JumpOp::RST30 => 1,
            JumpOp::RST38 => 1,
        }
    }
}
