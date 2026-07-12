use super::*;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, derive_more::Display)]
#[display("{_variant}")]
pub enum ArithmeticOp {
    #[display("ADD {_0}")]
    Add(SomeByte),
    #[display("ADD HL, {_0}")]
    Add16(WideReg),
    #[display("ADD SP, ")]
    AddSP,
    #[display("ADC {_0}")]
    Adc(SomeByte),
    #[display("SUB {_0}")]
    Sub(SomeByte),
    #[display("SBC {_0}")]
    Sbc(SomeByte),
    #[display("AND {_0}")]
    And(SomeByte),
    #[display("XOR {_0}")]
    Xor(SomeByte),
    #[display("OR {_0}")]
    Or(SomeByte),
    #[display("CP {_0}")]
    Cp(SomeByte),
    #[display("INC {_0}")]
    Inc(RegOrPointer),
    #[display("INC {_0}")]
    Inc16(WideReg),
    #[display("DEC {_0}")]
    Dec(RegOrPointer),
    #[display("DEC {_0}")]
    Dec16(WideReg),
}

impl ArithmeticOp {
    pub(crate) fn execute<M: MemoryLike>(self, state: &mut GameboyState<'_, M>) {
        fn load_byte<M:MemoryLike>(
            state: &mut GameboyState<'_, M>,
            byte: SomeByte,
        ) -> DataLocation {
            match byte {
                SomeByte::Referenced(RegOrPointer::Reg(reg)) => DataLocation::Register(reg),
                SomeByte::Referenced(RegOrPointer::Pointer) => {
                    state.tick(MCycle::load_pointer());
                    DataLocation::Bus
                }
                SomeByte::Direct => {
                    state.tick(MCycle::load_pc());
                    DataLocation::Bus
                }
            }
        }

        match self {
            ArithmeticOp::Add(byte) => {
                let reg = load_byte(state, byte);
                state.tick(MCycle::final_with_alu(AluSignal::add(reg)));
            }
            ArithmeticOp::Adc(byte) => {
                let reg = load_byte(state, byte);
                state.tick(MCycle::final_with_alu(AluSignal::adc(reg)));
            }
            ArithmeticOp::Sub(byte) => {
                let reg = load_byte(state, byte);
                state.tick(MCycle::final_with_alu(AluSignal::sub(reg)));
            }
            ArithmeticOp::Sbc(byte) => {
                let reg = load_byte(state, byte);
                state.tick(MCycle::final_with_alu(AluSignal::sbc(reg)));
            }
            ArithmeticOp::And(byte) => {
                let reg = load_byte(state, byte);
                state.tick(MCycle::final_with_alu(AluSignal::and(reg)));
            }
            ArithmeticOp::Xor(byte) => {
                let reg = load_byte(state, byte);
                state.tick(MCycle::final_with_alu(AluSignal::xor(reg)));
            }
            ArithmeticOp::Or(byte) => {
                let reg = load_byte(state, byte);
                state.tick(MCycle::final_with_alu(AluSignal::or(reg)));
            }
            ArithmeticOp::Cp(byte) => {
                let reg = load_byte(state, byte);
                state.tick(MCycle::final_with_alu(AluSignal::cp(reg)));
            }
            ArithmeticOp::Inc(reg_or_pointer) => {
                let carry = state.cpu.f.c;
                match reg_or_pointer {
                    RegOrPointer::Reg(reg) => {
                        let signal = AluSignal::inc(reg.into());
                        state.tick(MCycle::final_with_alu(signal));
                    }
                    RegOrPointer::Pointer => {
                        state.tick(MCycle::load_pointer());
                        let signal = AluSignal::inc(DataLocation::Bus);
                        let cycle = MCycle {
                            // The register doesn't really matter
                            addr_bus: PointerReg::HL,
                            action: AddrAction::Noop,
                            idu: None,
                            alu: Some(signal),
                        };
                        state.tick(cycle);
                        // FIXME: Its easier to just hand roll a write to memory. This shouldn't be
                        // the case...
                        let addr = state.cpu.ptr();
                        let val = state.cpu.z.0;
                        state.mem.write_byte(addr, val);
                        state.tick(MCycle::final_cycle());
                    }
                }
                state.cpu.f.c = carry;
            }
            ArithmeticOp::Dec(reg_or_pointer) => {
                let carry = state.cpu.f.c;
                match reg_or_pointer {
                    RegOrPointer::Reg(reg) => {
                        let signal = AluSignal::dec(reg.into());
                        state.tick(MCycle::final_with_alu(signal));
                    }
                    RegOrPointer::Pointer => {
                        state.tick(MCycle::load_pointer());
                        let signal = AluSignal::dec(DataLocation::Bus);
                        let cycle = MCycle {
                            // The register doesn't really matter
                            addr_bus: PointerReg::HL,
                            action: AddrAction::Noop,
                            idu: None,
                            alu: Some(signal),
                        };
                        state.tick(cycle);
                        // FIXME: Its easier to just hand roll a write to memory. This shouldn't be
                        // the case...
                        let addr = state.cpu.ptr();
                        let val = state.cpu.z.0;
                        state.mem.write_byte(addr, val);
                        state.tick(MCycle::final_cycle());
                    }
                }
                state.cpu.f.c = carry;
            }
            ArithmeticOp::AddSP => {
                state.tick(MCycle::load_pc());
                // Hand rolling...
                let old_sp = state.cpu.sp.0;
                state.tick(MCycle::noop());
                state.tick(MCycle::noop());
                let (sp, _c) = old_sp.overflowing_add_signed(state.cpu.z.0 as i8 as i16);
                state.cpu.f.z = false;
                state.cpu.f.n = false;
                state.cpu.f.h = (old_sp << 12) > (sp << 12);
                state.cpu.f.c = (old_sp << 8) > (sp << 8);
                state.cpu.sp = sp.into();
                state.tick(MCycle::final_cycle());
            }
            ArithmeticOp::Add16(wide_reg) => {
                let z = state.cpu.f.z;
                let [hi, lo] = match wide_reg {
                    WideReg::BC => state.cpu.bc().to_be_bytes(),
                    WideReg::DE => state.cpu.de().to_be_bytes(),
                    WideReg::HL => state.cpu.ptr().to_be_bytes(),
                    WideReg::SP => state.cpu.sp.0.to_be_bytes(),
                };
                let signal = AluSignal {
                    input_one: DataLocation::Register(HalfRegister::L),
                    input_two: DataLocation::Literal(lo),
                    op: AluOp::Add,
                    output: DataLocation::Register(HalfRegister::L),
                };
                let cycle = MCycle {
                    // Not actually used
                    addr_bus: PointerReg::SP,
                    action: AddrAction::Noop,
                    idu: None,
                    alu: Some(signal),
                };
                state.tick(cycle);
                let signal = AluSignal {
                    input_one: DataLocation::Register(HalfRegister::H),
                    input_two: DataLocation::Literal(hi),
                    op: AluOp::Adc,
                    output: DataLocation::Register(HalfRegister::H),
                };
                state.tick(MCycle::final_with_alu(signal));
                state.cpu.f.z = z;
            }
            ArithmeticOp::Inc16(wide_reg) => {
                let (reg, full) = match wide_reg {
                    WideReg::BC => (PointerReg::BC, FullRegister::BC),
                    WideReg::DE => (PointerReg::DE, FullRegister::DE),
                    WideReg::HL => (PointerReg::HL, FullRegister::HL),
                    WideReg::SP => (PointerReg::SP, FullRegister::SP),
                };
                let cycle = MCycle {
                    addr_bus: reg,
                    action: AddrAction::Noop,
                    idu: Some((IduSignal::Inc, full)),
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
            ArithmeticOp::Dec16(wide_reg) => {
                let (reg, full) = match wide_reg {
                    WideReg::BC => (PointerReg::BC, FullRegister::BC),
                    WideReg::DE => (PointerReg::DE, FullRegister::DE),
                    WideReg::HL => (PointerReg::HL, FullRegister::HL),
                    WideReg::SP => (PointerReg::SP, FullRegister::SP),
                };
                let cycle = MCycle {
                    addr_bus: reg,
                    action: AddrAction::Noop,
                    idu: Some((IduSignal::Dec, full)),
                    alu: None,
                };
                state.tick(cycle);
                state.tick(MCycle::final_cycle());
            }
        }
    }

    /// Returns the number of ticks to will take to complete this instruction.
    pub fn length(&self) -> u8 {
        match self {
            ArithmeticOp::Add16(_) => 8,
            ArithmeticOp::Add(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Add(_) => 8,
            ArithmeticOp::Adc(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Adc(_) => 8,
            ArithmeticOp::Sub(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Sub(_) => 8,
            ArithmeticOp::Sbc(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Sbc(_) => 8,
            ArithmeticOp::And(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::And(_) => 8,
            ArithmeticOp::Xor(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Xor(_) => 8,
            ArithmeticOp::Or(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Or(_) => 8,
            ArithmeticOp::Cp(SomeByte::Referenced(RegOrPointer::Reg(_))) => 4,
            ArithmeticOp::Cp(_) => 8,
            ArithmeticOp::Inc(RegOrPointer::Pointer) => 12,
            ArithmeticOp::Inc(RegOrPointer::Reg(_)) => 4,
            ArithmeticOp::Dec(RegOrPointer::Pointer) => 12,
            ArithmeticOp::Dec(RegOrPointer::Reg(_)) => 4,
            ArithmeticOp::Inc16(_) => 8,
            ArithmeticOp::Dec16(_) => 8,
            ArithmeticOp::AddSP => 16,
        }
    }

    /// Returns the size of the bytes to took to construct this instruction
    pub const fn size(&self) -> u8 {
        match self {
            ArithmeticOp::Add16(_) => 1,
            ArithmeticOp::Add(SomeByte::Direct) => 2,
            ArithmeticOp::Add(_) => 1,
            ArithmeticOp::Adc(SomeByte::Direct) => 2,
            ArithmeticOp::Adc(_) => 1,
            ArithmeticOp::Sub(SomeByte::Direct) => 2,
            ArithmeticOp::Sub(_) => 1,
            ArithmeticOp::Sbc(SomeByte::Direct) => 2,
            ArithmeticOp::Sbc(_) => 1,
            ArithmeticOp::And(SomeByte::Direct) => 2,
            ArithmeticOp::And(_) => 1,
            ArithmeticOp::Xor(SomeByte::Direct) => 2,
            ArithmeticOp::Xor(_) => 1,
            ArithmeticOp::Or(SomeByte::Direct) => 2,
            ArithmeticOp::Or(_) => 1,
            ArithmeticOp::Cp(SomeByte::Direct) => 2,
            ArithmeticOp::Cp(_) => 1,
            ArithmeticOp::Inc(_) => 1,
            ArithmeticOp::Dec(_) => 1,
            ArithmeticOp::Inc16(_) => 1,
            ArithmeticOp::Dec16(_) => 1,
            ArithmeticOp::AddSP => 2,
        }
    }
}
