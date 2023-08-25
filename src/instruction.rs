use crate::cpu::{HalfRegister, RegisterFlags};

fn map_tuple<A, B, F: FnOnce(A) -> B>((x, a): (usize, A), f: F) -> (usize, B) {
    (x, f(a))
}

/// The main instruction type. Used to parse a byte slice into, well, instructions.
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
    Rlca,
    Rrca,
    Rla,
    Rra,
}

impl Instruction {
    /// Takes a ROM's binary instruction sequence and constructs an instruction from it. Since
    /// instructions can consist of different numbers of bytes, the number of bytes used is also
    /// returned. This is used to inform the reader head how far to progress.
    pub fn parse(data: &[u8]) -> (usize, Self) {
        match data[0] {
            #[rustfmt::skip]
            0x00 | 0x10 | 0x37 | 0x76 | 0xF3 | 0xFB => map_tuple(ControlOp::parse(data), Self::Control),
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
            | 0x80..=0xBF | 0xE8 => map_tuple(ArithLogOp::parse(data), Self::ArithLog),
            _ => unreachable!(),
        }
    }
}

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
    fn parse(data: &[u8]) -> (usize, Self) {
        match data[0] {
            0x00 => (1, Self::NoOp),
            0x10 => (2, ControlOp::Stop(data[1])),
            0x37 => (1, ControlOp::Scf),
            0x76 => (1, ControlOp::Halt),
            0xF3 => (1, ControlOp::DI),
            0xFB => (1, ControlOp::EI),
            _ => unreachable!(),
        }
    }
}

/// Registers used for pushing onto and popping from the stack
pub enum StackReg {
    AF,
    BC,
    DE,
    HL,
}

/// Communicates if an operand is a register, a value pointed at by the HL register, or a literal
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

pub enum JumpOp {
    /// Jump the specified number of steps.
    Shift(i8),
    /// If the specified flag is 0, then move the given number of steps from the program counter.
    /// Otherwise, continue to the next instruction
    CheckedShift(RegisterFlags, i8),
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
    fn parse(data: &[u8]) -> (usize, Self) {
        match data[0] {
            x @ 0x03 | x @ 0x13 | x @ 0x23 | x @ 0x33 => (1, Self::IncFull(DirectFullReg::parse(x >> 4))),
            x @ 0x0B | x @ 0x1B | x @ 0x2B | x @ 0x3B => (1, Self::DecFull(DirectFullReg::parse(x >> 4))),
            0x04 => (1, Self::IncHalf(HalfRegister::B)),
            0x14 => (1, Self::IncHalf(HalfRegister::D)),
            0x24 => (1, Self::IncHalf(HalfRegister::H)),
            0x34 => (1, Self::IncHL),
            0x0C => (1, Self::IncHalf(HalfRegister::C)),
            0x1C => (1, Self::IncHalf(HalfRegister::E)),
            0x2C => (1, Self::IncHalf(HalfRegister::L)),
            0x3C => (1, Self::IncHalf(HalfRegister::A)),
            0x05 => (1, Self::DecHalf(HalfRegister::B)),
            0x15 => (1, Self::DecHalf(HalfRegister::D)),
            0x25 => (1, Self::DecHalf(HalfRegister::H)),
            0x35 => (1, Self::DecHL),
            0x0D => (1, Self::DecHalf(HalfRegister::C)),
            0x1D => (1, Self::DecHalf(HalfRegister::E)),
            0x2D => (1, Self::DecHalf(HalfRegister::L)),
            0x3D => (1, Self::DecHalf(HalfRegister::A)),
            x @ 0x09 | x @ 0x19 | x @ 0x29 | x @ 0x39 => (1, Self::HLAdd(DirectFullReg::parse(x >> 4))),
            x @ 0x80..=0x87 => (1, Self::Add(ArithLogValue::parse(x & 0x07))),
            0xC6 => (2, Self::Add(ArithLogValue::Literal(data[1]))),
            x @ 0x88..=0x8F => (1, Self::AddCarry(ArithLogValue::parse(x & 0x07))),
            0xCE => (2, Self::AddCarry(ArithLogValue::Literal(data[1]))),
            x @ 0x90..=0x97 => (1, Self::Sub(ArithLogValue::parse(x & 0x07))),
            0xD6 => (2, Self::Sub(ArithLogValue::Literal(data[1]))),
            x @ 0x98..=0x9F => (1, Self::SubCarry(ArithLogValue::parse(x & 0x07))),
            0xDE => (2, Self::SubCarry(ArithLogValue::Literal(data[1]))),
            x @ 0xA0..=0xA7 => (1, Self::And(ArithLogValue::parse(x & 0x07))),
            0xE6 => (2, Self::And(ArithLogValue::Literal(data[1]))),
            x @ 0xA8..=0xAF => (1, Self::Xor(ArithLogValue::parse(x & 0x07))),
            0xEE => (2, Self::Xor(ArithLogValue::Literal(data[1]))),
            x @ 0xB0..=0xB7 => (1, Self::Or(ArithLogValue::parse(x & 0x07))),
            0xF6 => (2, Self::Or(ArithLogValue::Literal(data[1]))),
            x @ 0xB8..=0xBF => (1, Self::Cmp(ArithLogValue::parse(x & 0x07))),
            0xFE => (2, Self::Cmp(ArithLogValue::Literal(data[1]))),
            //0xE8 => (Self::parse),
            _ => unreachable!(),
        }
    }
}

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
    HLStore(HLStore),
    /// Uses the HL register as a pointer to access RAM. Copies the value that the HL register
    /// points into the given register
    HLLoad(HalfRegister),
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

pub enum HLStore {
    HalfReg(HalfRegister),
    Data(u8),
}

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
