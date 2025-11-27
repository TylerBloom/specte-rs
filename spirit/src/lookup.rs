use array_concat::concat_arrays;

use crate::instruction::*;
// #[cfg(test)]
use crate::mem::MemoryLike;
#[cfg(not(test))]
use crate::mem::MemoryMap;

#[cfg(test)]
#[track_caller]
pub fn parse_instruction(mem: &dyn MemoryLike, pc: u16) -> Instruction {
    OP_LOOKUP[mem.read_byte(pc) as usize](mem, pc)
}

#[cfg(test)]
#[track_caller]
fn parse_prefixed_instruction(mem: &dyn MemoryLike, pc: u16) -> Instruction {
    PREFIXED_OP_LOOKUP[mem.read_byte(pc) as usize](mem, pc)
}

#[cfg(test)]
type OpArray<const N: usize> = [fn(&dyn MemoryLike, u16) -> Instruction; N];

#[cfg(not(test))]
#[track_caller]
pub fn parse_instruction(mem: &MemoryMap, pc: u16) -> Instruction {
    OP_LOOKUP[mem.read_byte(pc) as usize](mem, pc)
}

#[cfg(not(test))]
#[track_caller]
fn parse_prefixed_instruction(mem: &MemoryMap, pc: u16) -> Instruction {
    PREFIXED_OP_LOOKUP[mem.read_byte(pc) as usize](mem, pc)
}

#[cfg(not(test))]
type OpArray<const N: usize> = [fn(&MemoryMap, u16) -> Instruction; N];
#[cfg(test)]
#[track_caller]
fn invalid_op_code(mem: &dyn MemoryLike, pc: u16) -> Instruction {
    unreachable!(
        "Op code '0x{:0>2X}' @ 0x{pc:0>4X} does not correspond to any valid operation",
        mem.read_byte(pc)
    )
}

#[cfg(not(test))]
#[track_caller]
fn invalid_op_code(mem: &MemoryMap, pc: u16) -> Instruction {
    unreachable!(
        "Op code '0x{:0>2X}' @ 0x{pc:0>4X} does not correspond to any valid operation",
        mem.read_byte(pc)
    )
}

enum InnerRegOrPointer {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Pointer,
}

impl InnerRegOrPointer {
    const fn convert(self) -> RegOrPointer {
        match self {
            InnerRegOrPointer::A => RegOrPointer::Reg(HalfRegister::A),
            InnerRegOrPointer::B => RegOrPointer::Reg(HalfRegister::B),
            InnerRegOrPointer::C => RegOrPointer::Reg(HalfRegister::C),
            InnerRegOrPointer::D => RegOrPointer::Reg(HalfRegister::D),
            InnerRegOrPointer::E => RegOrPointer::Reg(HalfRegister::E),
            InnerRegOrPointer::H => RegOrPointer::Reg(HalfRegister::H),
            InnerRegOrPointer::L => RegOrPointer::Reg(HalfRegister::L),
            InnerRegOrPointer::Pointer => RegOrPointer::Pointer,
        }
    }
}

macro_rules! define_op {
    () => {
        invalid_op_code
    };
    (DAA) => {
        |_, _| Instruction::Daa
    };
    (SCF) => {
        |_, _| Instruction::Scf
    };
    (CPL) => {
        |_, _| Instruction::Cpl
    };
    (CCF) => {
        |_, _| Instruction::Ccf
    };
    (NOOP) => {
        |_, _| Instruction::ControlOp(ControlOp::Noop)
    };
    (JR) => {
        |data, pc| Instruction::Jump(JumpOp::Relative(data.read_byte(pc + 1) as i8))
    };
    (JR, $r: ident) => {
        |data, pc| {
            Instruction::Jump(JumpOp::ConditionalRelative(
                Condition::$r,
                data.read_byte(pc + 1) as i8,
            ))
        }
    };
    (JP) => {
        |data, pc| {
            Instruction::Jump(JumpOp::Absolute(u16::from_le_bytes([
                data.read_byte(pc + 1),
                data.read_byte(pc + 2),
            ])))
        }
    };
    (JP, HL) => {
        |_, _| Instruction::Jump(JumpOp::JumpToHL)
    };
    (JP, $r: ident) => {
        |data, pc| {
            Instruction::Jump(JumpOp::ConditionalAbsolute(
                Condition::$r,
                u16::from_le_bytes([data.read_byte(pc + 1), data.read_byte(pc + 2)]),
            ))
        }
    };
    (STOP) => {
        |data, pc| Instruction::ControlOp(ControlOp::Stop(data.read_byte(pc + 1)))
    };
    (ADD) => {
        |data, pc| {
            Instruction::Arithmetic(ArithmeticOp::Add(SomeByte::Direct(data.read_byte(pc + 1))))
        }
    };
    (ADD, SP) => {
        |data, pc| Instruction::Arithmetic(ArithmeticOp::AddSP(data.read_byte(pc + 1) as i8))
    };
    (ADD, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Add(InnerRegOrPointer::$r.convert().into()))
    };
    (ADC) => {
        |data, pc| {
            Instruction::Arithmetic(ArithmeticOp::Adc(SomeByte::Direct(data.read_byte(pc + 1))))
        }
    };
    (ADC, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Adc(InnerRegOrPointer::$r.convert().into()))
    };
    (SUB) => {
        |data, pc| {
            Instruction::Arithmetic(ArithmeticOp::Sub(SomeByte::Direct(data.read_byte(pc + 1))))
        }
    };
    (SUB, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Sub(InnerRegOrPointer::$r.convert().into()))
    };
    (SBC) => {
        |data, pc| {
            Instruction::Arithmetic(ArithmeticOp::Sbc(SomeByte::Direct(data.read_byte(pc + 1))))
        }
    };
    (SBC, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Sbc(InnerRegOrPointer::$r.convert().into()))
    };
    (AND) => {
        |data, pc| {
            Instruction::Arithmetic(ArithmeticOp::And(SomeByte::Direct(data.read_byte(pc + 1))))
        }
    };
    (AND, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::And(InnerRegOrPointer::$r.convert().into()))
    };
    (XOR) => {
        |data, pc| {
            Instruction::Arithmetic(ArithmeticOp::Xor(SomeByte::Direct(data.read_byte(pc + 1))))
        }
    };
    (XOR, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Xor(InnerRegOrPointer::$r.convert().into()))
    };
    (OR) => {
        |data, pc| {
            Instruction::Arithmetic(ArithmeticOp::Or(SomeByte::Direct(data.read_byte(pc + 1))))
        }
    };
    (OR, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Or(InnerRegOrPointer::$r.convert().into()))
    };
    (CP) => {
        |data, pc| {
            Instruction::Arithmetic(ArithmeticOp::Cp(SomeByte::Direct(data.read_byte(pc + 1))))
        }
    };
    (CP, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Cp(InnerRegOrPointer::$r.convert().into()))
    };
    (ADD16, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Add16(WideReg::$r))
    };
    (INC, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Inc(InnerRegOrPointer::$r.convert()))
    };
    (INC16, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Inc16(WideReg::$r))
    };
    (DEC, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Dec(InnerRegOrPointer::$r.convert()))
    };
    (DEC16, $r: ident) => {
        |_, _| Instruction::Arithmetic(ArithmeticOp::Dec16(WideReg::$r))
    };
    (LD, $r: ident, A) => {
        |_, _| Instruction::Load(LoadOp::StoreFromA(LoadAPointer::$r))
    };
    (LD SP) => {
        |data, pc| {
            Instruction::Load(LoadOp::StoreSP(u16::from_le_bytes([
                data.read_byte(pc + 1),
                data.read_byte(pc + 2),
            ])))
        }
    };
    (LoadA) => {
        |data, pc| {
            Instruction::Load(LoadOp::LoadA {
                ptr: u16::from_le_bytes([data.read_byte(pc + 1), data.read_byte(pc + 2)]),
            })
        }
    };
    (StoreA) => {
        |data, pc| {
            Instruction::Load(LoadOp::StoreA {
                ptr: u16::from_le_bytes([data.read_byte(pc + 1), data.read_byte(pc + 2)]),
            })
        }
    };
    (LD, $r: ident) => {
        |data, pc| {
            Instruction::Load(LoadOp::Direct(
                InnerRegOrPointer::$r.convert(),
                data.read_byte(pc + 1),
            ))
        }
    };
    (LD16, $r: ident) => {
        |data, pc| {
            Instruction::Load(LoadOp::Direct16(
                WideReg::$r,
                u16::from_le_bytes([data.read_byte(pc + 1), data.read_byte(pc + 2)]),
            ))
        }
    };
    (LD, A, $r: ident) => {
        |_, _| Instruction::Load(LoadOp::LoadIntoA(LoadAPointer::$r))
    };
    (LD, HL, SP) => {
        |data, pc| Instruction::Load(LoadOp::SPIntoHL(data.read_byte(pc + 1) as i8))
    };
    (LD, SP, HL) => {
        |_, _| Instruction::Load(LoadOp::HLIntoSP)
    };
    (LD, $r1: ident, $r2: ident,) => {
        |_, _| {
            Instruction::Load(LoadOp::Basic {
                dest: InnerRegOrPointer::$r1.convert(),
                src: InnerRegOrPointer::$r2.convert(),
            })
        }
    };
    (LD, Pointer, Pointer,) => {
        |_, _| Instruction::ControlOp(ControlOp::Halt)
    };
    (POP, $r: ident) => {
        |_, _| Instruction::Load(LoadOp::Pop(WideRegWithoutSP::$r))
    };
    (PUSH, $r: ident) => {
        |_, _| Instruction::Load(LoadOp::Push(WideRegWithoutSP::$r))
    };
    (RET) => {
        |_, _| Instruction::Jump(JumpOp::Return)
    };
    (RETI) => {
        |_, _| Instruction::Jump(JumpOp::ReturnAndEnable)
    };
    (RET, $r: ident) => {
        |_, _| Instruction::Jump(JumpOp::ConditionalReturn(Condition::$r))
    };
    (CALL) => {
        |data, pc| {
            Instruction::Jump(JumpOp::Call(u16::from_le_bytes([
                data.read_byte(pc + 1),
                data.read_byte(pc + 2),
            ])))
        }
    };
    (CALL, $r: ident) => {
        |data, pc| {
            Instruction::Jump(JumpOp::ConditionalCall(
                Condition::$r,
                u16::from_le_bytes([data.read_byte(pc + 1), data.read_byte(pc + 2)]),
            ))
        }
    };
    (PREFIX) => {
        |mem, pc| parse_prefixed_instruction(mem, pc + 1)
    };
    (DI) => {
        |_, _| Instruction::Di
    };
    (EI) => {
        |_, _| Instruction::Ei
    };
    (RST00) => {
        |_, _| Instruction::Jump(JumpOp::RST00)
    };
    (RST08) => {
        |_, _| Instruction::Jump(JumpOp::RST08)
    };
    (RST10) => {
        |_, _| Instruction::Jump(JumpOp::RST10)
    };
    (RST18) => {
        |_, _| Instruction::Jump(JumpOp::RST18)
    };
    (RST20) => {
        |_, _| Instruction::Jump(JumpOp::RST20)
    };
    (RST28) => {
        |_, _| Instruction::Jump(JumpOp::RST28)
    };
    (RST30) => {
        |_, _| Instruction::Jump(JumpOp::RST30)
    };
    (RST38) => {
        |_, _| Instruction::Jump(JumpOp::RST38)
    };
    (LoadHigh) => {
        |data, pc| Instruction::Load(LoadOp::LoadHigh(data.read_byte(pc + 1)))
    };
    (StoreHigh) => {
        |data, pc| Instruction::Load(LoadOp::StoreHigh(data.read_byte(pc + 1)))
    };
    (LDHCA) => {
        |_, _| Instruction::Load(LoadOp::Ldhca)
    };
    (LDHAC) => {
        |_, _| Instruction::Load(LoadOp::Ldhac)
    };
    /* --- Prefixed op definitions --- */
    (RLCA) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rlca)
    };
    (RLA) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rla)
    };
    (RL, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rl(InnerRegOrPointer::$r.convert()))
    };
    (RLC, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rlc(InnerRegOrPointer::$r.convert()))
    };
    (RRCA) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rrca)
    };
    (RRA) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rra)
    };
    (RRC, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rrc(InnerRegOrPointer::$r.convert()))
    };
    (RR, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Rr(InnerRegOrPointer::$r.convert()))
    };
    (SLA, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Sla(InnerRegOrPointer::$r.convert()))
    };
    (SRA, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Sra(InnerRegOrPointer::$r.convert()))
    };
    (SWAP, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Swap(InnerRegOrPointer::$r.convert()))
    };
    (SRL, $r: ident) => {
        |_, _| Instruction::BitShift(BitShiftOp::Srl(InnerRegOrPointer::$r.convert()))
    };
    (BIT, $b: literal, $r: ident) => {
        |data, pc| {
            Instruction::Bit(BitOp {
                bit: (data.read_byte(pc) - 0x40) / 8,
                reg: InnerRegOrPointer::$r.convert(),
                op: BitOpInner::Bit,
            })
        }
    };
    (RES, $b: literal, $r: ident) => {
        |data, pc| {
            Instruction::Bit(BitOp {
                bit: (data.read_byte(pc) - 0x80) / 8,
                reg: InnerRegOrPointer::$r.convert(),
                op: BitOpInner::Res,
            })
        }
    };
    (SET, $b: literal, $r: ident) => {
        |data, pc| {
            Instruction::Bit(BitOp {
                bit: (data.read_byte(pc) - 0xC0) / 8,
                reg: InnerRegOrPointer::$r.convert(),
                op: BitOpInner::Set,
            })
        }
    };
}

macro_rules! define_op_chunk {
    (LD) => {{
        const OPS: OpArray<0x40> = concat_arrays!(
            define_op_chunk!(LD, B),
            define_op_chunk!(LD, C),
            define_op_chunk!(LD, D),
            define_op_chunk!(LD, E),
            define_op_chunk!(LD, H),
            define_op_chunk!(LD, L),
            define_op_chunk!(LD, Pointer),
            define_op_chunk!(LD, A)
        );
        OPS
    }};
    ($x: ident, NUM) => {{
        const OPS: OpArray<0x40> = concat_arrays!(
            define_op_chunk!($x, 0,),
            define_op_chunk!($x, 1,),
            define_op_chunk!($x, 2,),
            define_op_chunk!($x, 3,),
            define_op_chunk!($x, 4,),
            define_op_chunk!($x, 5,),
            define_op_chunk!($x, 6,),
            define_op_chunk!($x, 7,)
        );
        OPS
    }};
    ($x: ident, $i: literal,) => {{
        const OPS: OpArray<0x08> = [
            define_op!($x, $i, B),
            define_op!($x, $i, C),
            define_op!($x, $i, D),
            define_op!($x, $i, E),
            define_op!($x, $i, H),
            define_op!($x, $i, L),
            define_op!($x, $i, Pointer),
            define_op!($x, $i, A),
        ];
        OPS
    }};
    ($x: ident) => {{
        const OPS: OpArray<8> = [
            define_op!($x, B),
            define_op!($x, C),
            define_op!($x, D),
            define_op!($x, E),
            define_op!($x, H),
            define_op!($x, L),
            define_op!($x, Pointer),
            define_op!($x, A),
        ];
        OPS
    }};
    ($x: ident, $r: ident) => {{
        const OPS: OpArray<8> = [
            define_op!($x, $r, B,),
            define_op!($x, $r, C,),
            define_op!($x, $r, D,),
            define_op!($x, $r, E,),
            define_op!($x, $r, H,),
            define_op!($x, $r, L,),
            define_op!($x, $r, Pointer,),
            define_op!($x, $r, A,),
        ];
        OPS
    }};
}

macro_rules! define_op_lookup_table {
    () => {
        concat_arrays!(
            define_op_lookup_table!(CHUNK_ONE),
            define_op_lookup_table!(CHUNK_TWO),
            define_op_lookup_table!(CHUNK_THREE),
            define_op_lookup_table!(CHUNK_FOUR)
        )
    };
    (PREFIXED) => {
        concat_arrays!(
            define_op_chunk!(RLC),
            define_op_chunk!(RRC),
            define_op_chunk!(RL),
            define_op_chunk!(RR),
            define_op_chunk!(SLA),
            define_op_chunk!(SRA),
            define_op_chunk!(SWAP),
            define_op_chunk!(SRL),
            define_op_chunk!(BIT, NUM),
            define_op_chunk!(RES, NUM),
            define_op_chunk!(SET, NUM)
        )
    };
    // NOTE: It is planned to use a transposition method for the top and bottom rows-of-four of
    // the op table. That way, they all can be defined in a similar way (in rows),
    // transposed in columns, and concatinated.
    (CHUNK_ONE) => {{
        const TO_TRANSPOSED: [OpArray<4>; 0x10] = [
            [
                define_op!(NOOP),
                define_op!(STOP),
                define_op!(JR, NotZero),
                define_op!(JR, NotCarry),
            ],
            [
                define_op!(LD16, BC),
                define_op!(LD16, DE),
                define_op!(LD16, HL),
                define_op!(LD16, SP),
            ],
            [
                define_op!(LD, BC, A),
                define_op!(LD, DE, A),
                define_op!(LD, Hli, A),
                define_op!(LD, Hld, A),
            ],
            [
                define_op!(INC16, BC),
                define_op!(INC16, DE),
                define_op!(INC16, HL),
                define_op!(INC16, SP),
            ],
            [
                define_op!(INC, B),
                define_op!(INC, D),
                define_op!(INC, H),
                define_op!(INC, Pointer),
            ],
            [
                define_op!(DEC, B),
                define_op!(DEC, D),
                define_op!(DEC, H),
                define_op!(DEC, Pointer),
            ],
            [
                define_op!(LD, B),
                define_op!(LD, D),
                define_op!(LD, H),
                define_op!(LD, Pointer),
            ],
            [
                define_op!(RLCA),
                define_op!(RLA),
                define_op!(DAA),
                define_op!(SCF),
            ],
            [
            define_op!(LD SP),
                define_op!(JR),
                define_op!(JR, Zero),
                define_op!(JR, Carry),
            ],
            [
                define_op!(ADD16, BC),
                define_op!(ADD16, DE),
                define_op!(ADD16, HL),
                define_op!(ADD16, SP),
            ],
            [
                define_op!(LD, A, BC),
                define_op!(LD, A, DE),
                define_op!(LD, A, Hli),
                define_op!(LD, A, Hld),
            ],
            [
                define_op!(DEC16, BC),
                define_op!(DEC16, DE),
                define_op!(DEC16, HL),
                define_op!(DEC16, SP),
            ],
            [
                define_op!(INC, C),
                define_op!(INC, E),
                define_op!(INC, L),
                define_op!(INC, A),
            ],
            [
                define_op!(DEC, C),
                define_op!(DEC, E),
                define_op!(DEC, L),
                define_op!(DEC, A),
            ],
            [
                define_op!(LD, C),
                define_op!(LD, E),
                define_op!(LD, L),
                define_op!(LD, A),
            ],
            [
                define_op!(RRCA),
                define_op!(RRA),
                define_op!(CPL),
                define_op!(CCF),
            ],
        ];
        const TRANSPOSED: [OpArray<16>; 4] = transpose!(TO_TRANSPOSED);
        const CHUNK: OpArray<0x40> =
            concat_arrays!(TRANSPOSED[0], TRANSPOSED[1], TRANSPOSED[2], TRANSPOSED[3]);
        CHUNK
    }};
    (CHUNK_TWO) => {
        define_op_chunk!(LD)
    };
    (CHUNK_THREE) => {{
        const CHUNK: OpArray<0x40> = concat_arrays!(
            define_op_chunk!(ADD),
            define_op_chunk!(ADC),
            define_op_chunk!(SUB),
            define_op_chunk!(SBC),
            define_op_chunk!(AND),
            define_op_chunk!(XOR),
            define_op_chunk!(OR),
            define_op_chunk!(CP)
        );
        CHUNK
    }};
    (CHUNK_FOUR) => {{
        const TO_TRANSPOSED: [OpArray<4>; 0x10] = [
            [
                define_op!(RET, NotZero),
                define_op!(RET, NotCarry),
                define_op!(LoadHigh),
                define_op!(StoreHigh),
            ],
            [
                define_op!(POP, BC),
                define_op!(POP, DE),
                define_op!(POP, HL),
                define_op!(POP, AF),
            ],
            [
                define_op!(JP, NotZero),
                define_op!(JP, NotCarry),
                define_op!(LDHCA),
                define_op!(LDHAC),
            ],
            [
                define_op!(JP),
                define_op!(), // DONE
                define_op!(), // DONE
                define_op!(DI),
            ],
            [
                define_op!(CALL, NotZero),
                define_op!(CALL, NotCarry),
                define_op!(), // DONE
                define_op!(), // DONE
            ],
            [
                define_op!(PUSH, BC),
                define_op!(PUSH, DE),
                define_op!(PUSH, HL),
                define_op!(PUSH, AF),
            ],
            [
                define_op!(ADD),
                define_op!(SUB),
                define_op!(AND),
                define_op!(OR),
            ],
            [
                define_op!(RST00),
                define_op!(RST10),
                define_op!(RST20),
                define_op!(RST30),
            ],
            [
                define_op!(RET, Zero),
                define_op!(RET, Carry),
                define_op!(ADD, SP),
                define_op!(LD, HL, SP),
            ],
            [
                define_op!(RET),
                define_op!(RETI),
                define_op!(JP, HL),
                define_op!(LD, SP, HL),
            ],
            [
                define_op!(JP, Zero),
                define_op!(JP, Carry),
                define_op!(LoadA),
                define_op!(StoreA),
            ],
            [
                define_op!(PREFIX),
                define_op!(), // DONE
                define_op!(), // DONE
                define_op!(EI),
            ],
            [
                define_op!(CALL, Zero),
                define_op!(CALL, Carry),
                define_op!(), // DONE
                define_op!(), // DONE
            ],
            [
                define_op!(CALL),
                define_op!(), // DONE
                define_op!(), // DONE
                define_op!(), // DONE
            ],
            [
                define_op!(ADC),
                define_op!(SBC),
                define_op!(XOR),
                define_op!(CP),
            ],
            [
                define_op!(RST08),
                define_op!(RST18),
                define_op!(RST28),
                define_op!(RST38),
            ],
        ];
        const TRANSPOSED: [OpArray<16>; 4] = transpose!(TO_TRANSPOSED);
        const CHUNK: OpArray<0x40> =
            concat_arrays!(TRANSPOSED[0], TRANSPOSED[1], TRANSPOSED[2], TRANSPOSED[3]);
        CHUNK
    }};
}

macro_rules! transpose {
    ($arr: ident) => {{
        const TRANSPOSED: [OpArray<16>; 4] = [
            transpose!($arr, 0),
            transpose!($arr, 1),
            transpose!($arr, 2),
            transpose!($arr, 3),
        ];
        TRANSPOSED
    }};
    ($arr: ident, $i: literal) => {{
        const INNER: OpArray<16> = [
            $arr[0][$i],
            $arr[1][$i],
            $arr[2][$i],
            $arr[3][$i],
            $arr[4][$i],
            $arr[5][$i],
            $arr[6][$i],
            $arr[7][$i],
            $arr[8][$i],
            $arr[9][$i],
            $arr[10][$i],
            $arr[11][$i],
            $arr[12][$i],
            $arr[13][$i],
            $arr[14][$i],
            $arr[15][$i],
        ];
        INNER
    }};
}

// TODO: Fix this once array_concat doesn't emit it anymore
#[allow(unexpected_cfgs)]
static OP_LOOKUP: OpArray<0x100> = define_op_lookup_table!();
#[allow(unexpected_cfgs)]
static PREFIXED_OP_LOOKUP: OpArray<0x100> = define_op_lookup_table!(PREFIXED);

#[cfg(test)]
mod test {
    use crate::mem::MemoryMap;

    use super::parse_instruction;
    use super::parse_prefixed_instruction;

    #[test]
    fn dedupped_op_lookup_tables() {
        let mut data = MemoryMap::construct();
        // Test standard ops
        let mut ops: Vec<_> = (0..=u8::MAX)
            .filter_map(|i| {
                data.rom_mut()[0] = i;
                std::panic::catch_unwind(|| parse_instruction(&data, 0)).ok()
            })
            .collect();
        // Ensure (almost) all of the operations are actually returning unique values
        let len = (u8::MAX - 10) as usize;
        assert_eq!(len, ops.len());
        ops.dedup();
        assert_eq!(len, ops.len());

        // Test prefixed ops
        let mut more_ops: Vec<_> = (0..=u8::MAX)
            .filter_map(|i| {
                data.rom_mut()[0] = i;
                std::panic::catch_unwind(|| parse_prefixed_instruction(&data, 0)).ok()
            })
            .collect();
        // Ensure all of the operations are actually returning unique values
        assert_eq!(0x100, more_ops.len());
        more_ops.dedup();
        assert_eq!(0x100, more_ops.len());
        ops.extend(more_ops);
        ops.dedup();
        assert_eq!(len + 0x100, ops.len());
    }
}
