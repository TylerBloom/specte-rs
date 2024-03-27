use array_concat::concat_arrays;

type OpArray<const N: usize> = [fn(&[u8]) -> Instruction; N];

pub enum Instruction {
    Load(LoadOp),
    BitShift(BitShiftOp),
    Bit(BitOp),
}

pub enum LoadOp {
    Basic {
        dest: RegOrPointer,
        src: RegOrPointer,
    },
    Direct(RegOrPointer, u8),
    LoadIntoA(LoadAPointer),
    StoreFromA(LoadAPointer),
}

/// There are special operations for loading into the A register, so it is easier to have a special
/// enum for the unique types of pointers they use.
pub enum LoadAPointer {
    /// Use the BC register
    BC,
    /// Use the DE register
    DE,
    /// Use the HL register and increment after performing the operation
    Hli,
    /// Use the HL register and decrement after performing the operation
    Hld,
}

#[derive(Debug, Clone, Copy)]
pub enum BitShiftOp {
    Rlc(RegOrPointer),
    Rrc(RegOrPointer),
    Rl(RegOrPointer),
    Rr(RegOrPointer),
    Sla(RegOrPointer),
    Sra(RegOrPointer),
    Swap(RegOrPointer),
    Srl(RegOrPointer),
}

#[derive(Debug, Clone, Copy)]
pub enum RegOrPointer {
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Pointer,
}

#[derive(Debug, Clone, Copy)]
pub enum BitOp {
    Bit(u8, RegOrPointer),
    Res(u8, RegOrPointer),
    Set(u8, RegOrPointer),
}

macro_rules! define_op {
    () => {
        |data| {
            unreachable!(
                "Op code '{}' does not correspond to any valid operation",
                data[0]
            )
        }
    };
    (LD, $r: ident, A) => {
        |_| Instruction::Load(LoadOp::StoreFromA(LoadAPointer::$r))
    };
    (LD, $r: ident) => {
        |data| Instruction::Load(LoadOp::Direct(RegOrPointer::$r, data[1]))
    };
    (LD, A, $r: ident) => {
        |_| Instruction::Load(LoadOp::LoadIntoA(LoadAPointer::$r))
    };
    (LD, $r1: ident, $r2: ident,) => {
        |_| {
            Instruction::Load(LoadOp::Basic {
                dest: RegOrPointer::$r1,
                src: RegOrPointer::$r2,
            })
        }
    };
    (LD, Pointer, Pointer,) => {
        |_| Instruction::Control(ControlOp::Halt)
    };
    /* --- Prefixed op definitions --- */
    (RL, $r: ident) => {
        |_| Instruction::BitShift(BitShiftOp::Rl(RegOrPointer::$r))
    };
    (RLC, $r: ident) => {
        |_| Instruction::BitShift(BitShiftOp::Rlc(RegOrPointer::$r))
    };
    (RRC, $r: ident) => {
        |_| Instruction::BitShift(BitShiftOp::Rrc(RegOrPointer::$r))
    };
    (RR, $r: ident) => {
        |_| Instruction::BitShift(BitShiftOp::Rr(RegOrPointer::$r))
    };
    (SLA, $r: ident) => {
        |_| Instruction::BitShift(BitShiftOp::Sla(RegOrPointer::$r))
    };
    (SRA, $r: ident) => {
        |_| Instruction::BitShift(BitShiftOp::Sra(RegOrPointer::$r))
    };
    (SWAP, $r: ident) => {
        |_| Instruction::BitShift(BitShiftOp::Swap(RegOrPointer::$r))
    };
    (SRL, $r: ident) => {
        |_| Instruction::BitShift(BitShiftOp::Srl(RegOrPointer::$r))
    };
    (BIT, $b: literal, $r: ident) => {
        |data| Instruction::Bit(BitOp::Bit(data[1], RegOrPointer::$r))
    };
    (RES, $b: literal, $r: ident) => {
        |data| Instruction::Bit(BitOp::Res(data[1], RegOrPointer::$r))
    };
    (SET, $b: literal, $r: ident) => {
        |data| Instruction::Bit(BitOp::Set(data[1], RegOrPointer::$r))
    };
}

macro_rules! define_op_chunk {
    (LD) => {{
        let ops: OpArray<0x40> = concat_arrays!(
            define_op_chunk!(LD, B),
            define_op_chunk!(LD, C),
            define_op_chunk!(LD, D),
            define_op_chunk!(LD, E),
            define_op_chunk!(LD, H),
            define_op_chunk!(LD, L),
            define_op_chunk!(LD, Pointer),
            define_op_chunk!(LD, A)
        );
        ops
    }};
    ($x: ident, NUM) => {{
        let ops: OpArray<0x40> = concat_arrays!(
            define_op_chunk!($x, 0,),
            define_op_chunk!($x, 1,),
            define_op_chunk!($x, 2,),
            define_op_chunk!($x, 3,),
            define_op_chunk!($x, 4,),
            define_op_chunk!($x, 5,),
            define_op_chunk!($x, 6,),
            define_op_chunk!($x, 7,)
        );
        ops
    }};
    ($x: ident, $i: literal,) => {{
        let ops: OpArray<0x08> = [
            define_op!($x, $i, B),
            define_op!($x, $i, C),
            define_op!($x, $i, D),
            define_op!($x, $i, E),
            define_op!($x, $i, H),
            define_op!($x, $i, L),
            define_op!($x, $i, Pointer),
            define_op!($x, $i, A),
        ];
        ops
    }};
    ($x: ident) => {{
        let ops: OpArray<8> = [
            define_op!($x, B),
            define_op!($x, C),
            define_op!($x, D),
            define_op!($x, E),
            define_op!($x, H),
            define_op!($x, L),
            define_op!($x, Pointer),
            define_op!($x, A),
        ];
        ops
    }};
    ($x: ident, $r: ident) => {{
        let ops: OpArray<8> = [
            define_op!($x, $r, B,),
            define_op!($x, $r, C,),
            define_op!($x, $r, D,),
            define_op!($x, $r, E,),
            define_op!($x, $r, H,),
            define_op!($x, $r, L,),
            define_op!($x, $r, Pointer,),
            define_op!($x, $r, A,),
        ];
        ops
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
        const TO_TRANSPOSED: [OpArray<4>; 2] = [
            [
                define_op!(LD, BC, A),
                define_op!(LD, DE, A),
                define_op!(LD, Hli, A),
                define_op!(LD, Hld, A),
            ],
            [
                define_op!(LD, A, BC),
                define_op!(LD, A, DE),
                define_op!(LD, A, Hld),
                define_op!(LD, A, Hld),
            ],
        ];
        const TRANSPOSED: [OpArray<2>; 4] = transpose!(TO_TRANSPOSED);
        const CHUNK: OpArray<8> =
            concat_arrays!(TRANSPOSED[0], TRANSPOSED[1], TRANSPOSED[2], TRANSPOSED[3]);
        CHUNK
    }};
    (CHUNK_TWO) => {
        define_op_chunk!(LD)
    };
    (CHUNK_THREE) => {{
        const CHUNK: OpArray<0> = [];
        CHUNK
    }};
    (CHUNK_FOUR) => {{
        const CHUNK: OpArray<0> = [];
        CHUNK
    }};
}

macro_rules! transpose {
    ($arr: ident) => {{
        const TRANSPOSED: [OpArray<2>; 4] = [
            transpose!($arr, 0),
            transpose!($arr, 1),
            transpose!($arr, 2),
            transpose!($arr, 3),
        ];
        TRANSPOSED
    }};
    ($arr: ident, $i: literal) => {{
        const INNER: OpArray<2> = [
            $arr[0][$i],
            $arr[1][$i],
            // $arr[2][$i],
            // $arr[3][$i],
            // $arr[4][$i],
            // $arr[5][$i],
            // $arr[6][$i],
            // $arr[7][$i],
            // $arr[8][$i],
            // $arr[9][$i],
            // $arr[10][$i],
            // $arr[11][$i],
            // $arr[12][$i],
            // $arr[13][$i],
            // $arr[14][$i],
            // $arr[15][$i],
        ];
        INNER
    }};
}

static OP_LOOKUP: OpArray<0x48> = define_op_lookup_table!();
static PREFIXED_OP_LOOKUP: OpArray<0x100> = define_op_lookup_table!(PREFIXED);

pub fn parse_instruction(data: &[u8]) -> Instruction {
    OP_LOOKUP[data[0] as usize](data)
}

#[cfg(test)]
mod test {
    use super::OP_LOOKUP;
    use crate::lookup::PREFIXED_OP_LOOKUP;

    #[test]
    fn dedupped_op_lookup_tables() {
        let mut ops: Vec<_> = OP_LOOKUP.map(|f| f as usize).into();
        let init_len = ops.len();
        ops.dedup();
        // I'm unsure if the duplicated panic closures will have the same address or not.
        // If so, we can account for that by adding back the known number of invalid
        // instructions
        assert_eq!(ops.len(), init_len);
        assert_eq!(ops.len(), OP_LOOKUP.len());
        let mut ops: Vec<_> = PREFIXED_OP_LOOKUP.map(|f| f as usize).into();
        let init_len = ops.len();
        ops.dedup();
        assert_eq!(ops.len(), init_len);
        assert_eq!(ops.len(), PREFIXED_OP_LOOKUP.len());
    }
}
