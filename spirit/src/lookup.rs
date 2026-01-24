use array_concat::concat_arrays;

use crate::instruction::*;

#[track_caller]
pub fn parse_instruction(op_code: u8) -> Instruction {
    OP_LOOKUP[op_code as usize]
}

#[track_caller]
pub fn parse_prefixed_instruction(op_code: u8) -> PrefixedInstruction {
    PREFIXED_OP_LOOKUP[op_code as usize]
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
    () => {{ Instruction::Unused }};
    (DAA) => {{ Instruction::Daa }};
    (SCF) => {{ Instruction::Scf }};
    (CPL) => {{ Instruction::Cpl }};
    (CCF) => {{ Instruction::Ccf }};
    (NOOP) => {{ Instruction::ControlOp(ControlOp::Noop) }};
    (JR) => {{ Instruction::Jump(JumpOp::Relative) }};
    (JR, $r: ident) => {{ Instruction::Jump(JumpOp::ConditionalRelative(Condition::$r)) }};
    (JP) => {{ Instruction::Jump(JumpOp::Absolute) }};
    (JP, HL) => {{ Instruction::Jump(JumpOp::JumpToHL) }};
    (JP, $r: ident) => {{ Instruction::Jump(JumpOp::ConditionalAbsolute(Condition::$r)) }};
    (STOP) => {{ Instruction::ControlOp(ControlOp::Stop) }};
    (ADD) => {{ Instruction::Arithmetic(ArithmeticOp::Add(SomeByte::Direct)) }};
    (ADD, SP) => {{ Instruction::Arithmetic(ArithmeticOp::AddSP) }};
    (ADD, $r: ident) => {{
        Instruction::Arithmetic(ArithmeticOp::Add(SomeByte::Referenced(
            InnerRegOrPointer::$r.convert(),
        )))
    }};
    (ADC) => {{ Instruction::Arithmetic(ArithmeticOp::Adc(SomeByte::Direct)) }};
    (ADC, $r: ident) => {{
        Instruction::Arithmetic(ArithmeticOp::Adc(SomeByte::Referenced(
            InnerRegOrPointer::$r.convert(),
        )))
    }};
    (SUB) => {{ Instruction::Arithmetic(ArithmeticOp::Sub(SomeByte::Direct)) }};
    (SUB, $r: ident) => {{
        Instruction::Arithmetic(ArithmeticOp::Sub(SomeByte::Referenced(
            InnerRegOrPointer::$r.convert(),
        )))
    }};
    (SBC) => {{ Instruction::Arithmetic(ArithmeticOp::Sbc(SomeByte::Direct)) }};
    (SBC, $r: ident) => {{
        Instruction::Arithmetic(ArithmeticOp::Sbc(SomeByte::Referenced(
            InnerRegOrPointer::$r.convert(),
        )))
    }};
    (AND) => {{ Instruction::Arithmetic(ArithmeticOp::And(SomeByte::Direct)) }};
    (AND, $r: ident) => {{
        Instruction::Arithmetic(ArithmeticOp::And(SomeByte::Referenced(
            InnerRegOrPointer::$r.convert(),
        )))
    }};
    (XOR) => {{ Instruction::Arithmetic(ArithmeticOp::Xor(SomeByte::Direct)) }};
    (XOR, $r: ident) => {{
        Instruction::Arithmetic(ArithmeticOp::Xor(SomeByte::Referenced(
            InnerRegOrPointer::$r.convert(),
        )))
    }};
    (OR) => {{ Instruction::Arithmetic(ArithmeticOp::Or(SomeByte::Direct)) }};
    (OR, $r: ident) => {{
        Instruction::Arithmetic(ArithmeticOp::Or(SomeByte::Referenced(
            InnerRegOrPointer::$r.convert(),
        )))
    }};
    (CP) => {{ Instruction::Arithmetic(ArithmeticOp::Cp(SomeByte::Direct)) }};
    (CP, $r: ident) => {{
        Instruction::Arithmetic(ArithmeticOp::Cp(SomeByte::Referenced(
            InnerRegOrPointer::$r.convert(),
        )))
    }};
    (ADD16, $r: ident) => {{ Instruction::Arithmetic(ArithmeticOp::Add16(WideReg::$r)) }};
    (INC, $r: ident) => {{ Instruction::Arithmetic(ArithmeticOp::Inc(InnerRegOrPointer::$r.convert())) }};
    (INC16, $r: ident) => {{ Instruction::Arithmetic(ArithmeticOp::Inc16(WideReg::$r)) }};
    (DEC, $r: ident) => {{ Instruction::Arithmetic(ArithmeticOp::Dec(InnerRegOrPointer::$r.convert())) }};
    (DEC16, $r: ident) => {{ Instruction::Arithmetic(ArithmeticOp::Dec16(WideReg::$r)) }};
    (LD, $r: ident, A) => {{ Instruction::Load(LoadOp::StoreFromA(LoadAPointer::$r)) }};
    (LD SP) => {{ Instruction::Load(LoadOp::StoreSP) }};
    (LoadA) => {{ Instruction::Load(LoadOp::LoadA) }};
    (StoreA) => {{ Instruction::Load(LoadOp::StoreA) }};
    (LD, $r: ident) => {{ Instruction::Load(LoadOp::Direct(InnerRegOrPointer::$r.convert())) }};
    (LD16, $r: ident) => {{ Instruction::Load(LoadOp::Direct16(WideReg::$r)) }};
    (LD, A, $r: ident) => {{ Instruction::Load(LoadOp::LoadIntoA(LoadAPointer::$r)) }};
    (LD, HL, SP) => {{ Instruction::Load(LoadOp::SPIntoHL) }};
    (LD, SP, HL) => {{ Instruction::Load(LoadOp::HLIntoSP) }};
    (LD, $r1: ident, $r2: ident,) => {{
        Instruction::Load(LoadOp::Basic {
            dest: InnerRegOrPointer::$r1.convert(),
            src: InnerRegOrPointer::$r2.convert(),
        })
    }};
    (LD, Pointer, Pointer,) => {{ Instruction::ControlOp(ControlOp::Halt) }};
    (POP, $r: ident) => {{ Instruction::Load(LoadOp::Pop(WideRegWithoutSP::$r)) }};
    (PUSH, $r: ident) => {{ Instruction::Load(LoadOp::Push(WideRegWithoutSP::$r)) }};
    (RET) => {{ Instruction::Jump(JumpOp::Return) }};
    (RETI) => {{ Instruction::Jump(JumpOp::ReturnAndEnable) }};
    (RET, $r: ident) => {{ Instruction::Jump(JumpOp::ConditionalReturn(Condition::$r)) }};
    (CALL) => {{ Instruction::Jump(JumpOp::Call) }};
    (CALL, $r: ident) => {{ Instruction::Jump(JumpOp::ConditionalCall(Condition::$r)) }};
    (PREFIX) => {{ Instruction::Prefixed }};
    (DI) => {{ Instruction::Di }};
    (EI) => {{ Instruction::Ei }};
    (RST00) => {{ Instruction::Jump(JumpOp::RST00) }};
    (RST08) => {{ Instruction::Jump(JumpOp::RST08) }};
    (RST10) => {{ Instruction::Jump(JumpOp::RST10) }};
    (RST18) => {{ Instruction::Jump(JumpOp::RST18) }};
    (RST20) => {{ Instruction::Jump(JumpOp::RST20) }};
    (RST28) => {{ Instruction::Jump(JumpOp::RST28) }};
    (RST30) => {{ Instruction::Jump(JumpOp::RST30) }};
    (RST38) => {{ Instruction::Jump(JumpOp::RST38) }};
    (LoadHigh) => {{ Instruction::Load(LoadOp::LoadHigh) }};
    (StoreHigh) => {{ Instruction::Load(LoadOp::StoreHigh) }};
    (LDHCA) => {{ Instruction::Load(LoadOp::Ldhca) }};
    (LDHAC) => {{ Instruction::Load(LoadOp::Ldhac) }};
    (RLCA) => {{ Instruction::Rlca }};
    (RLA) => {{ Instruction::Rla }};
    (RRCA) => {{ Instruction::Rrca }};
    (RRA) => {{ Instruction::Rra }};
}

macro_rules! define_prefixed_op {
    (RL, $r: ident) => {{
        PrefixedInstruction::BitShift(BitShiftOp {
            op: BitShiftOpInner::Rl,
            reg: InnerRegOrPointer::$r.convert(),
        })
    }};
    (RLC, $r: ident) => {{
        PrefixedInstruction::BitShift(BitShiftOp {
            op: BitShiftOpInner::Rlc,
            reg: InnerRegOrPointer::$r.convert(),
        })
    }};
    (RR, $r: ident) => {{
        PrefixedInstruction::BitShift(BitShiftOp {
            op: BitShiftOpInner::Rr,
            reg: InnerRegOrPointer::$r.convert(),
        })
    }};
    (RRC, $r: ident) => {{
        PrefixedInstruction::BitShift(BitShiftOp {
            op: BitShiftOpInner::Rrc,
            reg: InnerRegOrPointer::$r.convert(),
        })
    }};
    (SLA, $r: ident) => {{
        PrefixedInstruction::BitShift(BitShiftOp {
            op: BitShiftOpInner::Sla,
            reg: InnerRegOrPointer::$r.convert(),
        })
    }};
    (SRA, $r: ident) => {{
        PrefixedInstruction::BitShift(BitShiftOp {
            op: BitShiftOpInner::Sra,
            reg: InnerRegOrPointer::$r.convert(),
        })
    }};
    (SWAP, $r: ident) => {{
        PrefixedInstruction::BitShift(BitShiftOp {
            op: BitShiftOpInner::Swap,
            reg: InnerRegOrPointer::$r.convert(),
        })
    }};
    (SRL, $r: ident) => {{
        PrefixedInstruction::BitShift(BitShiftOp {
            op: BitShiftOpInner::Srl,
            reg: InnerRegOrPointer::$r.convert(),
        })
    }};
    (BIT, $b: literal, $r: ident) => {{
        PrefixedInstruction::Bit(BitOp {
            bit: $b,
            reg: InnerRegOrPointer::$r.convert(),
            op: BitOpInner::Bit,
        })
    }};
    (RES, $b: literal, $r: ident) => {{
        PrefixedInstruction::Bit(BitOp {
            bit: $b,
            reg: InnerRegOrPointer::$r.convert(),
            op: BitOpInner::Res,
        })
    }};
    (SET, $b: literal, $r: ident) => {{
        PrefixedInstruction::Bit(BitOp {
            bit: $b,
            reg: InnerRegOrPointer::$r.convert(),
            op: BitOpInner::Set,
        })
    }};
}

macro_rules! define_op_chunk {
    (LD) => {{
        const OPS: [Instruction; 0x40] = concat_arrays!(
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
    ($x: ident) => {{
        const OPS: [Instruction; 0x08] = [
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
        const OPS: [Instruction; 0x08] = [
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

macro_rules! define_prefixed_op_chunk {
    ($x: ident, NUM) => {{
        const OPS: [PrefixedInstruction; 0x40] = concat_arrays!(
            define_prefixed_op_chunk!($x, 0),
            define_prefixed_op_chunk!($x, 1),
            define_prefixed_op_chunk!($x, 2),
            define_prefixed_op_chunk!($x, 3),
            define_prefixed_op_chunk!($x, 4),
            define_prefixed_op_chunk!($x, 5),
            define_prefixed_op_chunk!($x, 6),
            define_prefixed_op_chunk!($x, 7)
        );
        OPS
    }};
    ($x: ident, $i: literal) => {{
        const OPS: [PrefixedInstruction; 0x08] = [
            define_prefixed_op!($x, $i, B),
            define_prefixed_op!($x, $i, C),
            define_prefixed_op!($x, $i, D),
            define_prefixed_op!($x, $i, E),
            define_prefixed_op!($x, $i, H),
            define_prefixed_op!($x, $i, L),
            define_prefixed_op!($x, $i, Pointer),
            define_prefixed_op!($x, $i, A),
        ];
        OPS
    }};
    ($x: ident) => {{
        const OPS: [PrefixedInstruction; 0x08] = [
            define_prefixed_op!($x, B),
            define_prefixed_op!($x, C),
            define_prefixed_op!($x, D),
            define_prefixed_op!($x, E),
            define_prefixed_op!($x, H),
            define_prefixed_op!($x, L),
            define_prefixed_op!($x, Pointer),
            define_prefixed_op!($x, A),
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
    // NOTE: It is planned to use a transposition method for the top and bottom rows-of-four of
    // the op table. That way, they all can be defined in a similar way (in rows),
    // transposed in columns, and concatinated.
    (CHUNK_ONE) => {{
        const TO_TRANSPOSED: [[Instruction; 0x04]; 0x10] = [
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
        const TRANSPOSED: [[Instruction; 0x10]; 0x04] = transpose!(TO_TRANSPOSED);
        const CHUNK: [Instruction; 0x40] =
            concat_arrays!(TRANSPOSED[0], TRANSPOSED[1], TRANSPOSED[2], TRANSPOSED[3]);
        CHUNK
    }};
    (CHUNK_TWO) => {
        define_op_chunk!(LD)
    };
    (CHUNK_THREE) => {{
        const CHUNK: [Instruction; 0x40] = concat_arrays!(
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
        const TO_TRANSPOSED: [[Instruction; 0x04]; 0x10] = [
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
        const TRANSPOSED: [[Instruction; 0x10]; 0x04] = transpose!(TO_TRANSPOSED);
        const CHUNK: [Instruction; 0x40] =
            concat_arrays!(TRANSPOSED[0], TRANSPOSED[1], TRANSPOSED[2], TRANSPOSED[3]);
        CHUNK
    }};
}

macro_rules! define_prefixed_op_lookup_table {
    () => {
        concat_arrays!(
            define_prefixed_op_chunk!(RLC),
            define_prefixed_op_chunk!(RRC),
            define_prefixed_op_chunk!(RL),
            define_prefixed_op_chunk!(RR),
            define_prefixed_op_chunk!(SLA),
            define_prefixed_op_chunk!(SRA),
            define_prefixed_op_chunk!(SWAP),
            define_prefixed_op_chunk!(SRL),
            define_prefixed_op_chunk!(BIT, NUM),
            define_prefixed_op_chunk!(RES, NUM),
            define_prefixed_op_chunk!(SET, NUM)
        )
    };
}

macro_rules! transpose {
    ($arr: ident) => {{
        const TRANSPOSED: [[Instruction; 0x10]; 0x04] = [
            transpose!($arr, 0),
            transpose!($arr, 1),
            transpose!($arr, 2),
            transpose!($arr, 3),
        ];
        TRANSPOSED
    }};
    ($arr: ident, $i: literal) => {{
        const INNER: [Instruction; 0x10] = [
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

pub static OP_LOOKUP: [Instruction; 0x100] = define_op_lookup_table!();
pub static PREFIXED_OP_LOOKUP: [PrefixedInstruction; 0x100] = define_prefixed_op_lookup_table!();

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use crate::instruction::Instruction;

    use super::parse_instruction;
    use super::parse_prefixed_instruction;

    static UNUSED_OP_CODES: &[u8] = &[
        0xD3,
        0xDB,
        0xDD,
        0xE3,
        0xE4,
        0xEB,
        0xEC,
        0xED,
        0xF4,
        0xFC,
        0xFD,
    ];

    #[test]
    fn unused_op_codes() {
        for op_code in UNUSED_OP_CODES {
            println!("Checking 0x{op_code:0>2X}");
            assert_eq!(parse_instruction(*op_code), Instruction::Unused);
        }
    }

    #[test]
    fn dedupped_op_lookup_tables() {
        // Test standard ops
        // Ensure (almost) all of the operations are actually returning unique values
        let ops: HashSet<_> = (0..=u8::MAX).map(|i| parse_instruction(i)).collect();
        assert_eq!(0x100 - UNUSED_OP_CODES.len() + 1, ops.len());

        // Test prefixed ops
        let mut more_ops: Vec<_> = (0..=u8::MAX)
            .map(|i| parse_prefixed_instruction(i))
            .collect();
        // Ensure all of the operations are actually returning unique values
        assert_eq!(0x100, more_ops.len());
        more_ops.dedup();
        assert_eq!(0x100, more_ops.len());
    }
}
