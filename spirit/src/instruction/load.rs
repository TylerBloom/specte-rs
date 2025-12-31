use crate::{mem::MemoryLikeExt, utils::Wrapping};

use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum LoadOp {
    /// Used for opcodes in 0x40..0x80
    #[display("LD {src} -> {dest}")]
    Basic {
        dest: RegOrPointer,
        src: RegOrPointer,
    },
    /// Used for opcodes 0xX1
    #[display("LD 0x{_1:0>4X} -> {_0}")]
    Direct16(WideReg, u16),
    /// Used for opcodes 0x_6 and 0x_E
    #[display("LD 0x{_1:0>2X} -> {_0}")]
    Direct(RegOrPointer, u8),
    /// Used for opcodes 0x_A
    #[display("LD {_0} -> A")]
    LoadIntoA(LoadAPointer),
    /// Used for opcodes 0x_2
    #[display("LD A -> {_0}")]
    StoreFromA(LoadAPointer),
    /// Opcode: 0x08
    /// Store SP & $FF at address n16 and SP >> 8 at address n16 + 1.
    #[display("LD SP -> 0x{_0:0>4X}")]
    StoreSP(u16),
    /// Opcode: 0xF9
    #[display("LD SP -> HL")]
    HLIntoSP,
    /// Opcode: 0xF8
    /// Add the signed value e8 to SP and store the result in HL.
    #[display("LD SP + {_0} -> HL")]
    SPIntoHL(i8),
    /// Used for opcodes 0x_1
    #[display("POP {_0}")]
    Pop(WideRegWithoutSP),
    /// Used for opcodes 0x_5
    #[display("PUSH {_0}")]
    Push(WideRegWithoutSP),
    /// Used for opcode 0xE0
    #[display("LDH A -> 0xFF{_0:0>2X}")]
    LoadHigh(u8),
    /// Used for opcode 0xF0
    #[display("LDH 0xFF{_0:0>2X} -> A")]
    StoreHigh(u8),
    /// Used for opcode 0xE2
    #[display("LDHCA")]
    Ldhca,
    /// Used for opcode 0xF2
    #[display("LDHAC")]
    Ldhac,
    // TODO: These are backwards. For `LoadA`, the ptr is the dest and A is the value. The reverse
    // is true for `StoreA`.
    /// Used for opcode 0xEA
    #[display("LD 0x{ptr:0>4X} -> A")]
    LoadA { ptr: u16 },
    /// Used for opcode 0xFA
    #[display("LD A -> 0x{ptr:0>4X}")]
    StoreA { ptr: u16 },
}

impl LoadOp {
    pub(crate) fn execute<M: MemoryLikeExt>(self, mut state: GameboyState<'_, M>) {
        match self {
            LoadOp::Basic { dest, src } => match (dest, src) {
                (RegOrPointer::Reg(dest), RegOrPointer::Reg(src)) => {
                    let signal = AluSignal::move_into(src.into(), dest.into());
                    let cycle = MCycle::final_with_alu(signal);
                    state.tick(cycle);
                }
                (RegOrPointer::Reg(dest), RegOrPointer::Pointer) => {
                    state.tick(MCycle::load_pointer());
                    let signal =
                        AluSignal::move_into(DataLocation::Bus, DataLocation::Register(dest));
                    state.tick(MCycle::final_with_alu(signal));
                }
                (RegOrPointer::Pointer, RegOrPointer::Reg(src)) => {
                    let cycle = MCycle {
                        addr_bus: PointerReg::HL,
                        action: AddrAction::Write(src.into()),
                        idu: None,
                        alu: None,
                    };
                    state.tick(cycle);
                    state.tick(MCycle::final_cycle());
                }
                (RegOrPointer::Pointer, RegOrPointer::Pointer) => {
                    todo!("Impl HALT")
                }
            },
            LoadOp::Direct16(wide_reg, _) => {
                let cycle = MCycle {
                    addr_bus: PointerReg::PC,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
                let cycle = MCycle {
                    addr_bus: PointerReg::PC,
                    action: AddrAction::Read(ReadLocation::RegisterW),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
                state.cpu.ghost_move(wide_reg);
            }
            LoadOp::Direct(reg_or_pointer, _) => match reg_or_pointer {
                RegOrPointer::Reg(reg) => {
                    let cycle = MCycle::load_pc();
                    state.tick(cycle);
                    let signal = AluSignal::move_into(DataLocation::Bus, reg.into());
                    state.tick(MCycle::final_with_alu(signal));
                }
                RegOrPointer::Pointer => {
                    let cycle = MCycle::load_pc();
                    state.tick(cycle);
                    let cycle = MCycle {
                        addr_bus: PointerReg::HL,
                        action: AddrAction::Write(DataLocation::Bus),
                        idu: None,
                        alu: None,
                    };
                    state.tick(cycle);
                    state.tick(MCycle::final_cycle());
                }
            },
            LoadOp::LoadIntoA(ptr) => {
                let cycle = match ptr {
                    LoadAPointer::BC => MCycle {
                        addr_bus: PointerReg::BC,
                        action: AddrAction::Read(DataLocation::Bus.into()),
                        idu: None,
                        alu: None,
                    },
                    LoadAPointer::DE => MCycle {
                        addr_bus: PointerReg::DE,
                        action: AddrAction::Read(DataLocation::Bus.into()),
                        idu: None,
                        alu: None,
                    },
                    LoadAPointer::Hli => MCycle {
                        addr_bus: PointerReg::HL,
                        action: AddrAction::Read(DataLocation::Bus.into()),
                        idu: Some((IduSignal::Inc, FullRegister::HL)),
                        alu: None,
                    },
                    LoadAPointer::Hld => MCycle {
                        addr_bus: PointerReg::HL,
                        action: AddrAction::Read(DataLocation::Bus.into()),
                        idu: Some((IduSignal::Inc, FullRegister::HL)),
                        alu: None,
                    },
                };
                state.tick(cycle);
                state.tick(MCycle::final_with_alu(AluSignal::move_into_a(
                    DataLocation::Bus,
                )));
            }
            LoadOp::StoreFromA(ptr) => {
                let cycle = match ptr {
                    LoadAPointer::BC => MCycle {
                        addr_bus: PointerReg::BC,
                        action: AddrAction::Write(HalfRegister::A.into()),
                        idu: None,
                        alu: None,
                    },
                    LoadAPointer::DE => MCycle {
                        addr_bus: PointerReg::DE,
                        action: AddrAction::Write(HalfRegister::A.into()),
                        idu: None,
                        alu: None,
                    },
                    LoadAPointer::Hli => MCycle {
                        addr_bus: PointerReg::HL,
                        action: AddrAction::Write(HalfRegister::A.into()),
                        idu: Some((IduSignal::Inc, FullRegister::HL)),
                        alu: None,
                    },
                    LoadAPointer::Hld => MCycle {
                        addr_bus: PointerReg::HL,
                        action: AddrAction::Write(HalfRegister::A.into()),
                        idu: Some((IduSignal::Inc, FullRegister::HL)),
                        alu: None,
                    },
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            LoadOp::StoreSP(_) => {
                // Construct pointer out of "ghost registers"
                let cycle = MCycle {
                    addr_bus: PointerReg::PC,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
                let cycle = MCycle {
                    addr_bus: PointerReg::PC,
                    action: AddrAction::Read(ReadLocation::RegisterW),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
                // Write SP to the stack using the "ghost pointer"
                let [s, p] = state.cpu.sp.0.to_be_bytes();
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Write(s.into()),
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                state.cpu.inc_ghost_w();
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Write(p.into()),
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            LoadOp::HLIntoSP => {
                let cycle = MCycle {
                    addr_bus: PointerReg::HL,
                    action: AddrAction::Noop,
                    idu: Some((IduSignal::Noop, FullRegister::SP)),
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            LoadOp::SPIntoHL(_) => {
                state.tick(MCycle::load_pc());
                // It is just easier to handroll this operations than shove it into an MCycle...
                let z = state.cpu.z() as i16;
                let sp = state.cpu.sp;
                // TODO: Adjust flags...
                state.cpu.sp = Wrapping(sp.0.wrapping_add_signed(z));
                state.blind_tick();
                state.tick(MCycle::final_cycle());
            }
            LoadOp::Pop(wide_reg_without_sp) => {
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
                state.tick(MCycle::final_cycle());
                state.cpu.ghost_move_wide_reg(wide_reg_without_sp);
            }
            LoadOp::Push(wide_reg_without_sp) => {
                let cycle = MCycle {
                    addr_bus: PointerReg::SP,
                    action: AddrAction::Noop,
                    idu: Some((IduSignal::Dec, FullRegister::SP)),
                    alu: None,
                };
                let (most, least) = wide_reg_without_sp.split();
                state.tick(cycle);
                let cycle = MCycle {
                    addr_bus: PointerReg::SP,
                    action: AddrAction::Write(most.into()),
                    idu: Some((IduSignal::Dec, FullRegister::SP)),
                    alu: None,
                };
                state.tick(cycle);
                let cycle = MCycle {
                    addr_bus: PointerReg::SP,
                    action: AddrAction::Write(least.into()),
                    idu: Some((IduSignal::Dec, FullRegister::SP)),
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            LoadOp::LoadHigh(_) => {
                let cycle = MCycle {
                    addr_bus: PointerReg::PC,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
                state.cpu.w = Wrapping(0xFF);
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Write(HalfRegister::A.into()),
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            LoadOp::StoreHigh(_) => {
                let cycle = MCycle {
                    addr_bus: PointerReg::PC,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
                state.cpu.w = Wrapping(0xFF);
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                let signal = AluSignal::move_into_a(DataLocation::Bus);
                state.tick(MCycle::final_with_alu(signal));
            }
            LoadOp::Ldhca => {
                state.cpu.w = Wrapping(0xFF);
                state.cpu.z = state.cpu.c;
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Write(HalfRegister::A.into()),
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            LoadOp::Ldhac => {
                state.cpu.w = Wrapping(0xFF);
                state.cpu.z = state.cpu.c;
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                let signal = AluSignal::move_into_a(DataLocation::Bus);
                state.tick(MCycle::final_with_alu(signal));
            }
            LoadOp::LoadA { ptr: _ } => {
                let cycle = MCycle {
                    addr_bus: PointerReg::PC,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
                let cycle = MCycle {
                    addr_bus: PointerReg::PC,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Write(HalfRegister::A.into()),
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            LoadOp::StoreA { ptr: _ } => {
                let cycle = MCycle {
                    addr_bus: PointerReg::PC,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
                let cycle = MCycle {
                    addr_bus: PointerReg::PC,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: Some((IduSignal::Inc, FullRegister::PC)),
                    alu: None,
                };
                state.tick(cycle);
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Read(ReadLocation::RegisterZ),
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                let signal = AluSignal::move_into_a(DataLocation::Bus);
                state.tick(MCycle::final_with_alu(signal));
            }
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            LoadOp::Basic {
                dest: RegOrPointer::Pointer,
                src: RegOrPointer::Pointer,
            } => 4,
            LoadOp::Basic {
                dest: RegOrPointer::Pointer,
                ..
            }
            | LoadOp::Basic {
                src: RegOrPointer::Pointer,
                ..
            } => 8,
            LoadOp::Basic { .. } => 4,
            LoadOp::Direct16(_, _) => 12,
            LoadOp::Direct(RegOrPointer::Pointer, _) => 12,
            LoadOp::Direct(_, _) => 8,
            LoadOp::LoadIntoA(_) => 8,
            LoadOp::StoreFromA(_) => 8,
            LoadOp::StoreSP(_) => 20,
            LoadOp::HLIntoSP => 8,
            LoadOp::SPIntoHL(_) => 12,
            LoadOp::Pop(_) => 12,
            LoadOp::Push(_) => 16,
            LoadOp::LoadHigh(_) => 12,
            LoadOp::StoreHigh(_) => 12,
            LoadOp::Ldhca => 8,
            LoadOp::Ldhac => 8,
            LoadOp::LoadA { .. } => 16,
            LoadOp::StoreA { .. } => 16,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            LoadOp::Basic { .. } => 1,
            LoadOp::Direct16(_, _) => 3,
            LoadOp::Direct(_, _) => 2,
            LoadOp::LoadIntoA(_) => 1,
            LoadOp::StoreFromA(_) => 1,
            LoadOp::StoreSP(_) => 3,
            LoadOp::HLIntoSP => 1,
            LoadOp::SPIntoHL(_) => 2,
            LoadOp::Pop(_) => 1,
            LoadOp::Push(_) => 1,
            LoadOp::LoadHigh(_) => 2,
            LoadOp::StoreHigh(_) => 2,
            LoadOp::Ldhca => 1,
            LoadOp::Ldhac => 1,
            LoadOp::LoadA { .. } => 3,
            LoadOp::StoreA { .. } => 3,
        }
    }
}
