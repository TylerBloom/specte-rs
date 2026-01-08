use crate::mem::MemoryLikeExt;
use crate::utils::Wrapping;

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
    #[display("LD -> {_0}")]
    Direct16(WideReg),
    /// Used for opcodes 0x_6 and 0x_E
    #[display("LD -> {_0}")]
    Direct(RegOrPointer),
    /// Used for opcodes 0x_A
    #[display("LD {_0} -> A")]
    LoadIntoA(LoadAPointer),
    /// Used for opcodes 0x_2
    #[display("LD A -> {_0}")]
    StoreFromA(LoadAPointer),
    /// Opcode: 0x08
    /// Store SP & $FF at address n16 and SP >> 8 at address n16 + 1.
    #[display("LD SP -> _")]
    StoreSP,
    /// Opcode: 0xF9
    #[display("LD SP -> HL")]
    HLIntoSP,
    /// Opcode: 0xF8
    /// Add the signed value e8 to SP and store the result in HL.
    #[display("LD SP+ -> HL")]
    SPIntoHL,
    /// Used for opcodes 0x_1
    #[display("POP {_0}")]
    Pop(WideRegWithoutSP),
    /// Used for opcodes 0x_5
    #[display("PUSH {_0}")]
    Push(WideRegWithoutSP),
    /// Used for opcode 0xE0
    #[display("LDH A")]
    LoadHigh,
    /// Used for opcode 0xF0
    #[display("LDH A")]
    StoreHigh,
    /// Used for opcode 0xE2
    #[display("LDHCA")]
    Ldhca,
    /// Used for opcode 0xF2
    #[display("LDHAC")]
    Ldhac,
    // TODO: These are backwards. For `LoadA`, the ptr is the dest and A is the value. The reverse
    // is true for `StoreA`.
    /// Used for opcode 0xEA
    #[display("LD A")]
    LoadA,
    /// Used for opcode 0xFA
    #[display("LD A")]
    StoreA,
}

impl LoadOp {
    pub(crate) fn execute<M: MemoryLikeExt>(self, state: &mut GameboyState<'_, M>) {
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
                    todo!()
                    // self.state = CpuState::Halted;
                    // self.pc -= Wrapping(ControlOp::Halt.size() as u16);
                }
            },
            LoadOp::Direct16(wide_reg) => {
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
            LoadOp::Direct(reg_or_pointer) => match reg_or_pointer {
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
                        idu: Some((IduSignal::Dec, FullRegister::HL)),
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
                        idu: Some((IduSignal::Dec, FullRegister::HL)),
                        alu: None,
                    },
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            LoadOp::StoreSP => {
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
                // Write SP to mem using the "ghost pointer"
                let [s, p] = state.cpu.sp.0.to_be_bytes();
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Write(p.into()),
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                // Increment the "ghost pointer"
                state.cpu.z += 1;
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Write(s.into()),
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
            LoadOp::SPIntoHL => {
                state.tick(MCycle::load_pc());
                // It is just easier to handroll this operations than shove it into an MCycle...
                state.tick(MCycle::noop());
                let z = state.cpu.z.0;
                let sp = state.cpu.sp.0;
                let [h, l] = sp
                    .wrapping_add_signed((z as i8) as i16)
                    .to_be_bytes()
                    .map(Wrapping);
                let hl = u16::from_be_bytes([h.0, l.0]);
                state.cpu.h = h;
                state.cpu.f.z = false;
                state.cpu.f.n = false;
                state.cpu.f.h = (sp << 12) > (hl << 12);
                state.cpu.f.c = (sp << 8) > (hl << 8);
                state.tick(MCycle::final_cycle());
                state.cpu.l = l;
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
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            LoadOp::LoadHigh => {
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
            LoadOp::StoreHigh => {
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
            LoadOp::LoadA => {
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
                let cycle = MCycle {
                    addr_bus: PointerReg::Ghost,
                    action: AddrAction::Write(HalfRegister::A.into()),
                    idu: None,
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            LoadOp::StoreA => {
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
            LoadOp::Direct16(_) => 12,
            LoadOp::Direct(RegOrPointer::Pointer) => 12,
            LoadOp::Direct(_) => 8,
            LoadOp::LoadIntoA(_) => 8,
            LoadOp::StoreFromA(_) => 8,
            LoadOp::StoreSP => 20,
            LoadOp::HLIntoSP => 8,
            LoadOp::SPIntoHL => 12,
            LoadOp::Pop(_) => 12,
            LoadOp::Push(_) => 16,
            LoadOp::LoadHigh => 12,
            LoadOp::StoreHigh => 12,
            LoadOp::Ldhca => 8,
            LoadOp::Ldhac => 8,
            LoadOp::LoadA => 16,
            LoadOp::StoreA => 16,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            LoadOp::Basic { .. } => 1,
            LoadOp::Direct16(_) => 3,
            LoadOp::Direct(_) => 2,
            LoadOp::LoadIntoA(_) => 1,
            LoadOp::StoreFromA(_) => 1,
            LoadOp::StoreSP => 3,
            LoadOp::HLIntoSP => 1,
            LoadOp::SPIntoHL => 2,
            LoadOp::Pop(_) => 1,
            LoadOp::Push(_) => 1,
            LoadOp::LoadHigh => 2,
            LoadOp::StoreHigh => 2,
            LoadOp::Ldhca => 1,
            LoadOp::Ldhac => 1,
            LoadOp::LoadA => 3,
            LoadOp::StoreA => 3,
        }
    }
}
