use crate::cpu::{HalfRegister, RegisterFlags};
use derive_more::From;

/// The main instruction type. Used to parse a byte slice into, well, instructions.
#[derive(Debug, Clone, Copy, From)]
pub enum Instruction {
    /// Various CPU control operations
    Control(ControlOp),
    /// Instructions for loading data into and from registers
    Load(LoadOp),
    /// Instructions for performing arithmetic and logical operations
    ArithLog(ArithLogOp),
    /// Instructions for jumping to other instructions
    Jump(JumpOp),

    // TODO ops
    Ret(),
    Call(),

    // Both are 8-bit arithmetic ops
    Daa,
    Cpl,

    // All 4 are rotate/shift ops
    BitShift(BitShiftOp),
    Bit(BitOp),
}

impl Instruction {
    /// Takes a ROM's binary instruction sequence and constructs an instruction from it. Since
    /// instructions can consist of different numbers of bytes, the number of bytes used is also
    /// returned. This is used to inform the reader head how far to progress.
    pub fn parse(data: &[u8]) -> Self {
        println!("Reading op from {:?}", &data[..=0xF]);
        match data[0] {
            #[rustfmt::skip]
            0x00 | 0x10 | 0x37 | 0x76 | 0xF3 | 0xFB => ControlOp::parse(data).into(),
            #[rustfmt::skip]
              0x03 | 0x13 | 0x23 | 0x33
            | 0x04 | 0x14 | 0x24 | 0x34
            | 0x05 | 0x15 | 0x25 | 0x35
            | 0x09 | 0x19 | 0x29 | 0x39
            | 0x0B | 0x1B | 0x2B | 0x3B
            | 0x0C | 0x1C | 0x2C | 0x3C
            | 0x0D | 0x1D | 0x2D | 0x3D
            | 0xC6 | 0xD6 | 0xE6 | 0xF6
            | 0xCE | 0xDE | 0xEE | 0xFE
            | 0x80..=0xBF | 0xE8 => ArithLogOp::parse(data).into(),
            n @ 0x20 | n @ 0x30 | n @ 0x28 | n @ 0x38 => {
                let cond = match n {
                    0x20 => Condition::NZ,
                    0x30 => Condition::NC,
                    0x28 => Condition::Z,
                    0x38 => Condition::C,
                    _ => unreachable!(),
                };
                Self::Jump(JumpOp::CheckedJump(cond, data[1] as i8))
            }
            0x40..=0x7F => LoadOp::parse(data).into(),
            _ => todo!(),
        }
    }

    /// Returns the number of clock cycles it takes to perform the operation (this is not reduced
    /// by 4).
    pub const fn ticks(&self) -> u8 {
        match self {
            Instruction::Control(op) => op.ticks(),
            Instruction::Load(_) => todo!(),
            Instruction::ArithLog(_) => todo!(),
            Instruction::Jump(_) => todo!(),
            Instruction::Ret() => todo!(),
            Instruction::Call() => todo!(),
            Instruction::Daa => todo!(),
            Instruction::Cpl => todo!(),
            _ => todo!(),
        }
    }

    /// Returns the number of bytes the operation takes up in the Gameboy's memory.
    pub const fn size(&self) -> u8 {
        match self {
            Instruction::Control(op) => op.size(),
            Instruction::Load(op) => todo!(),
            Instruction::ArithLog(op) => todo!(),
            Instruction::Jump(op) => todo!(),
            Instruction::Ret() => todo!(),
            Instruction::Call() => todo!(),
            Instruction::Daa => todo!(),
            Instruction::Cpl => todo!(),
            _ => todo!(),
        }
    }
}

/// Encapsulates which condition an operation checks for.
///  - Z -- Execute if Z is set.
///  - NZ -- Execute if Z is not set.
///  - C -- Execute if C is set.
///  - NC -- Execute if C is not set.
#[derive(Debug, Clone, Copy)]
pub enum Condition {
    Z,
    NZ,
    C,
    NC,
}

#[derive(Debug, Clone, Copy)]
pub enum ControlOp {
    /// Do nothing
    NoOp,
    /// Halt
    Stop(u8),
    /// Stop
    Halt,
    /// Disable interrupts
    DI,
    /// Enable interrupts
    EI,
    /// Set the CY flag
    Scf,
    /// Flip the CY flag
    Ccf,
}

impl ControlOp {
    fn parse(data: &[u8]) -> Self {
        match data[0] {
            0x00 => Self::NoOp,
            0x10 => ControlOp::Stop(data[1]),
            0x37 => ControlOp::Scf,
            0x76 => ControlOp::Halt,
            0xF3 => ControlOp::DI,
            0xFB => ControlOp::EI,
            _ => unreachable!(),
        }
    }

    /// Returns the number of clock cycles it takes to perform the operation (this is not reduced
    /// by 4).
    const fn ticks(&self) -> u8 {
        // All CPU control operations take 4 cycles.
        4
    }

    /// Returns the number of bytes the operation takes up in the Gameboy's memory.
    const fn size(&self) -> u8 {
        // All operations except STOP, which has a size of 2, have a size of 1. STOP
        // 1 + ((discriminant(self) as u8) == (discriminant(&Self::Stop(0) as u8))) as u8
        todo!()
    }
}

/// Registers used for pushing onto and popping from the stack
#[derive(Debug, Clone, Copy)]
pub enum StackReg {
    AF,
    BC,
    DE,
    HL,
}

/// Communicates if an operand is a register, a value pointed at by the HL register, or a literal
#[derive(Debug, Clone, Copy)]
pub enum ArithLogValue {
    /// The operand is another register
    Reg(HalfRegister),
    /// The operand a byte that is pointed at by the HL register
    Pointer,
    /// The operand is a byte literal
    Literal(u8),
}

impl ArithLogValue {
    fn parse(reg: u8) -> Self {
        match reg {
            0 => Self::Reg(HalfRegister::B),
            1 => Self::Reg(HalfRegister::C),
            2 => Self::Reg(HalfRegister::D),
            3 => Self::Reg(HalfRegister::E),
            4 => Self::Reg(HalfRegister::H),
            5 => Self::Reg(HalfRegister::L),
            6 => Self::Pointer,
            7 => Self::Reg(HalfRegister::A),
            x => unreachable!("Unknown arith log value: {x}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum JumpOp {
    /// Jump the specified number of steps.
    Jump(i8),
    /// If the specified condition is met, then move the given number of steps from the program
    /// counter. Otherwise, continue to the next instruction
    CheckedJump(Condition, i8),
    /// The given value is loaded into the SP register.
    Load(u16),
    /// The value in the HL register is loaded into the SP register.
    HLLoad(u16),
    /// If the specified flag is 0, then the given value is stored into the SP register. Otherwise,
    /// the SP register is incremented.
    CheckedLoad(RegisterFlags, u16),
    /// Push the current value of the program counter PC onto the memory stack, and load into PC
    /// the nth byte of page 0 memory addresses. The next instruction is fetched from the address
    /// specified by the new content of PC (as usual).
    Rst(u8),
}

#[derive(Debug, Clone, Copy)]
pub enum ArithLogOp {
    /// Increments a full register by one
    IncFull(DirectFullReg),
    /// Decrements a full register by one
    DecFull(DirectFullReg),
    /// Increments a half register by one
    IncHalf(HalfRegister),
    /// Decrements a half register by one
    DecHalf(HalfRegister),
    /// Uses the HL register as a pointer to increment the value that is being pointed at
    IncHL,
    /// Uses the HL register as a pointer to decrement the value that is being pointed at
    DecHL,
    /// Adds the contents of the given register to register HL and stores the result in register
    /// HL.
    HLAdd(DirectFullReg),
    /// Adds the contents of the A register and the given value and stores them in the A register
    Add(ArithLogValue),
    /// Like `Add` but also includes the carry flag
    AddCarry(ArithLogValue),
    /// Like `Add` by performs subtraction
    Sub(ArithLogValue),
    /// Like `Sub` but also includes the carry flag
    SubCarry(ArithLogValue),
    /// Like `Add` by performs logical AND
    And(ArithLogValue),
    /// Like `Add` by performs logical OR
    Or(ArithLogValue),
    /// Like `Add` by performs logical XOR
    Xor(ArithLogValue),
    /// Compare the contents of register B and the contents of register A by calculating A - B, and
    /// set the Z flag if they are equal.
    ///
    /// This does not affect the contents of register A.
    Cmp(ArithLogValue),
}

impl ArithLogOp {
    fn parse(data: &[u8]) -> Self {
        match data[0] {
            x @ 0x03 | x @ 0x13 | x @ 0x23 | x @ 0x33 => {
                Self::IncFull(DirectFullReg::parse(x >> 4))
            }
            x @ 0x0B | x @ 0x1B | x @ 0x2B | x @ 0x3B => {
                Self::DecFull(DirectFullReg::parse(x >> 4))
            }
            0x04 => Self::IncHalf(HalfRegister::B),
            0x14 => Self::IncHalf(HalfRegister::D),
            0x24 => Self::IncHalf(HalfRegister::H),
            0x34 => Self::IncHL,
            0x0C => Self::IncHalf(HalfRegister::C),
            0x1C => Self::IncHalf(HalfRegister::E),
            0x2C => Self::IncHalf(HalfRegister::L),
            0x3C => Self::IncHalf(HalfRegister::A),
            0x05 => Self::DecHalf(HalfRegister::B),
            0x15 => Self::DecHalf(HalfRegister::D),
            0x25 => Self::DecHalf(HalfRegister::H),
            0x35 => Self::DecHL,
            0x0D => Self::DecHalf(HalfRegister::C),
            0x1D => Self::DecHalf(HalfRegister::E),
            0x2D => Self::DecHalf(HalfRegister::L),
            0x3D => Self::DecHalf(HalfRegister::A),
            x @ 0x09 | x @ 0x19 | x @ 0x29 | x @ 0x39 => Self::HLAdd(DirectFullReg::parse(x >> 4)),
            x @ 0x80..=0x87 => Self::Add(ArithLogValue::parse(x & 0x07)),
            0xC6 => Self::Add(ArithLogValue::Literal(data[1])),
            x @ 0x88..=0x8F => Self::AddCarry(ArithLogValue::parse(x & 0x07)),
            0xCE => Self::AddCarry(ArithLogValue::Literal(data[1])),
            x @ 0x90..=0x97 => Self::Sub(ArithLogValue::parse(x & 0x07)),
            0xD6 => Self::Sub(ArithLogValue::Literal(data[1])),
            x @ 0x98..=0x9F => Self::SubCarry(ArithLogValue::parse(x & 0x07)),
            0xDE => Self::SubCarry(ArithLogValue::Literal(data[1])),
            x @ 0xA0..=0xA7 => Self::And(ArithLogValue::parse(x & 0x07)),
            0xE6 => Self::And(ArithLogValue::Literal(data[1])),
            x @ 0xA8..=0xAF => Self::Xor(ArithLogValue::parse(x & 0x07)),
            0xEE => Self::Xor(ArithLogValue::Literal(data[1])),
            x @ 0xB0..=0xB7 => Self::Or(ArithLogValue::parse(x & 0x07)),
            0xF6 => Self::Or(ArithLogValue::Literal(data[1])),
            x @ 0xB8..=0xBF => Self::Cmp(ArithLogValue::parse(x & 0x07)),
            0xFE => Self::Cmp(ArithLogValue::Literal(data[1])),
            0xE8 => todo!(), // TODO: This was `(Self::parse),`
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum LoadOp {
    /// Takes 16 bits and loads them into a register
    FullLoad(DirectFullReg, u16),
    /// Takes 16 bits and loads them into a register
    HalfLoad { reg: HalfRegister, data: u8 },
    /// Takes the contains of one register and copies them into another.
    SwapHalfReg {
        src: HalfRegister,
        dest: HalfRegister,
    },
    /// Uses the HL register as a pointer to access RAM. Stores a copy of the source at the byte that
    /// HL points to.
    Store(RegOrPointer),
    /// Uses the HL register as a pointer to access RAM. Copies the value that the HL register
    /// points into the given register
    Load(RegOrPointer),
    /// Similar to `HLStore(A)` but also increments the HL register
    HLIncStore,
    /// Similar to `HLStore(A)` but also decrements the HL register
    HLDecStore,
    /// Similar to `HLLoad(A)` but also increments the HL register
    HLIncLoad,
    /// Similar to `HLLoad(A)` but also decrements the HL register
    HLDecLoad,
    /// Takes a u16 and treats it as a pointer. Loads the data in the SP register into the
    /// specified location.
    StoreSP(u16),
    /// Copies the contents of the HL register into the SP register
    SPLoad,
    /// Adds the signed i8 to the stack pointer and stores the result in SP register
    SignedPointerLoad(i8),
    /// Pops the top byte from the stack and loads it into a register
    Pop(StackReg),
    /// Push the contents of a register onto the stack
    Push(StackReg),
    // TODO: IO ports
}

impl LoadOp {
    #[inline]
    fn parse(data: &[u8]) -> Self {
        todo!()
    }

    #[inline]
    const fn ticks(&self) -> u8 {
        todo!()
    }

    #[inline]
    const fn size(&self) -> u8 {
        todo!()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum DirectFullReg {
    BC,
    DE,
    HL,
    SP,
}

impl DirectFullReg {
    fn parse(reg: u8) -> Self {
        match reg {
            0 => Self::BC,
            1 => Self::DE,
            2 => Self::HL,
            3 => Self::SP,
            _ => unreachable!(),
        }
    }
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

#[cfg(test)]
mod tests {
    use super::ArithLogOp;

    #[test]
    fn arith_logic_sanity() {
        let mut buff = [0u8, 0u8];
        for i in 0x80..0xC0 {
            buff[0] = i;
            println!("Parsing instruction: {buff:X?}");
            let _ = ArithLogOp::parse(&buff);
        }
    }
}

/// A module that implements a lookup table of instructions from op codes.
mod lookup {
    use super::*;
    use array_concat::concat_arrays;
    type OpArray<const N: usize> = [fn(&[u8]) -> Instruction; N];

    macro_rules! define_op {
        () => {
            |data| {
                unreachable!(
                    "Op code '{}' does not correspond to any valid operation",
                    data[0]
                )
            }
        };
        (LD, $r1: ident, $r2: ident) => {
            |_| todo!()
        };
        (LD, $r: ident, [HL]) => {
            |_| Instruction::Load(LoadOp::Store(RegOrPointer::$r))
        };
        (LD, [HL], $r: ident) => {
            |_| Instruction::Load(LoadOp::Load(RegOrPointer::$r))
        };
        (LD, Pointer, Pointer) => {
            |_| Instruction::Control(ControlOp::Halt)
        };
        (RL, $r: ident) => {
            |_| Instruction::BitShift(BitShiftOp::Rl(RegOrPointer::$r))
        };
        (RLC, $r: ident) => {
            |_| Instruction::BitShift(BitShiftOp::Rlc(RegOrPointer::$r))
        };
        (RRC, $r: ident) => {
            |_| todo!()
        };
        (RR, $r: ident) => {
            |_| todo!()
        };
        (SLA, $r: ident) => {
            |_| todo!()
        };
        (SRA, $r: ident) => {
            |_| todo!()
        };
        (SWAP, $r: ident) => {
            |_| todo!()
        };
        (SRL, $r: ident) => {
            |_| todo!()
        };
        (BIT, $b: literal, $r: ident) => {
            |_| todo!()
        };
        (RES, $b: literal, $r: ident) => {
            |_| todo!()
        };
        (SET, $b: literal, $r: ident) => {
            |_| todo!()
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
        (LD, [HL]) => {{
            let ops: OpArray<8> = [
                define_op!(LD, [HL], B),
                define_op!(LD, [HL], C),
                define_op!(LD, [HL], D),
                define_op!(LD, [HL], E),
                define_op!(LD, [HL], H),
                define_op!(LD, [HL], L),
                define_op!(LD, [HL], [HL]),
                define_op!(LD, [HL], A),
            ];
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
                define_op!($x, $i, A)
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
                define_op!($x, $r, B),
                define_op!($x, $r, C),
                define_op!($x, $r, D),
                define_op!($x, $r, E),
                define_op!($x, $r, H),
                define_op!($x, $r, L),
                define_op!($x, $r, Pointer),
                define_op!($x, $r, A),
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
        (CHUNK_ONE) => ({
            const CHUNK: OpArray<0> = [];
            CHUNK
        });
        (CHUNK_TWO) => {
            define_op_chunk!(LD)
        };
        (CHUNK_THREE) => ({
            const CHUNK: OpArray<0> = [];
            CHUNK
        });
        (CHUNK_FOUR) => ({
            const CHUNK: OpArray<0> = [];
            CHUNK
        });
    }

    static OP_LOOKUP: OpArray<0x40> = define_op_lookup_table!();
    static PREFIXED_OP_LOOKUP: OpArray<0x100> = define_op_lookup_table!(PREFIXED);

    pub fn parse_instruction(data: &[u8]) -> Instruction {
        OP_LOOKUP[data[0] as usize](data)
    }

    #[cfg(test)]
    mod test {
        use crate::instruction::lookup::PREFIXED_OP_LOOKUP;

        use super::OP_LOOKUP;

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
}
