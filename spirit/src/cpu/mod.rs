use std::hash::Hash;

use serde::Deserialize;
use serde::Serialize;

use crate::instruction::AddrAction;
use crate::instruction::AluOp;
use crate::instruction::AluSignal;
use crate::instruction::DataLocation;
use crate::instruction::HalfRegister;
use crate::instruction::IduSignal;
use crate::instruction::Instruction;
use crate::instruction::MCycle;
use crate::instruction::PointerReg;
use crate::instruction::PrefixedInstruction;
use crate::instruction::ReadLocation;
use crate::instruction::RegOrPointer;
use crate::instruction::WideReg;
use crate::instruction::WideRegWithoutSP;
use crate::lookup::parse_instruction;
use crate::lookup::parse_prefixed_instruction;
use crate::mem::MemoryLike;
// use crate::mem::MemoryLikeExt;
use crate::utils::Wrapping;

#[cfg(test)]
mod cpu_tests;

#[derive(
    Debug, Default, Hash, Clone, PartialEq, Eq, derive_more::Display, Serialize, Deserialize,
)]
#[display(
    "CPU {{ A=0x{:0>2X} F={} B=0x{:0>2X} C=0x{:0>2X} D=0x{:0>2X} E=0x{:0>2X} H=0x{:0>2X} L=0x{:0>2X}, Z={:0>2X}, W={:0>2X}, IR={:0>2X} SP=0x{:0>4X} PC=0x{:0>4X} IME={} Done={} }}",
    a,
    f,
    b,
    c,
    d,
    e,
    h,
    l,
    z,
    w,
    ir,
    sp,
    pc,
    ime,
    state
)]
pub struct Cpu {
    pub a: Wrapping<u8>,
    pub f: Flags,
    pub b: Wrapping<u8>,
    pub c: Wrapping<u8>,
    pub d: Wrapping<u8>,
    pub e: Wrapping<u8>,
    pub h: Wrapping<u8>,
    pub l: Wrapping<u8>,
    /// The SP register
    pub sp: Wrapping<u16>,
    /// The PC register
    pub pc: Wrapping<u16>,
    pub ime: bool,
    pub to_set_ime: bool,
    /// Once the gameboy has halted, this flag is set. Note that the gameboy can continue to be
    /// ticked, but the stack pointer is not moved, so it will continue to cycle without change.
    pub state: CpuState,
    /// These two registers are "ghost registers". Z is used as the one byte data bus in most
    /// cases; however, there are times where a buffer of a second byte is needed.
    /// No assumptions about the state of these registers should be made between instructions.
    pub z: Wrapping<u8>,
    pub w: Wrapping<u8>,
    /// The instruction register. At the end of an instruction, a read is done to load the op code
    /// for the next instruction. That data is put into this register then decoded into the next
    /// operation.
    pub ir: Wrapping<u8>,
}

#[derive(
    Debug, Default, Hash, Clone, Copy, PartialEq, Eq, derive_more::Display, Serialize, Deserialize,
)]
#[repr(u8)]
pub enum CpuState {
    #[default]
    Running = 0x11,
    Halted = 0x01,
    Stopped = 0x00,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FullRegister {
    AF,
    BC,
    DE,
    HL,
    SP,
    PC,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterFlags {
    Z,
    N,
    H,
    C,
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, Default, Hash, derive_more::Display, Serialize, Deserialize,
)]
#[display(
    "Flags(Self=0x{:0>2X}, Z={} N={} H={} C={})",
    self.as_byte(),
    *z as u8,
    *n as u8,
    *h as u8,
    *c as u8
)]
pub struct Flags {
    /// The zero flag
    pub z: bool,
    /// The substraction flag
    pub n: bool,
    /// The half-carry flag
    pub h: bool,
    /// The full carry flag
    pub c: bool,
}

impl From<u8> for Flags {
    fn from(value: u8) -> Self {
        Self {
            z: check_bit_const::<7>(value),
            n: check_bit_const::<6>(value),
            h: check_bit_const::<5>(value),
            c: check_bit_const::<4>(value),
        }
    }
}

impl Flags {
    pub fn set_from_byte(&mut self, val: u8) {
        *self = val.into();
    }

    pub fn set_for_byte_shift_op(&mut self, z: bool, c: bool) {
        self.z = z;
        self.n = false;
        self.h = false;
        self.c = c;
    }

    pub const fn as_byte(&self) -> u8 {
        bool_to_mask::<7>(self.z)
            | bool_to_mask::<6>(self.n)
            | bool_to_mask::<5>(self.h)
            | bool_to_mask::<4>(self.c)
    }
}

const fn bit_select<const B: u8>() -> u8 {
    const {
        match B {
            n @ 0..=7 => 0x1 << n,
            _ => panic!("You must select between the 0th and 7th bit!"),
        }
    }
}

const fn u16_bit_select<const B: u8>() -> u16 {
    const {
        match B {
            n @ 0..=15 => 0x1 << n,
            _ => panic!("You must select between the 0th and 7th bit!"),
        }
    }
}

const fn bool_to_mask<const B: u8>(val: bool) -> u8 {
    (val as u8) << B
}

pub(crate) const fn check_bit(bit: u8, src: u8) -> bool {
    let bit = 0x1 << bit;
    (src & bit) == bit
}

const fn select_bit<const B: u8>(src: u8) -> u8 {
    src & bit_select::<B>()
}

pub const fn check_bit_const<const B: u8>(src: u8) -> bool {
    (src & bit_select::<B>()) == bit_select::<B>()
}

pub(crate) const fn u16_check_bit_const<const B: u8>(src: u16) -> bool {
    (src & u16_bit_select::<B>()) == u16_bit_select::<B>()
}

/// Takes a byte that is in standard binary representation and converts it to binary coded decimal.
fn to_bcd(mut val: u8, flags: &mut Flags) -> u8 {
    if !flags.n {
        // after an addition, adjust if (half-)carry occurred or if result is out of bounds
        if flags.c || val > 0x99 {
            val = val.wrapping_add(0x60);
            flags.c = true;
        }
        if flags.h || (val & 0x0f) > 0x09 {
            val = val.wrapping_add(0x6);
        }
    } else {
        if flags.c {
            val = val.wrapping_sub(0x60);
        }
        if flags.h {
            val = val.wrapping_sub(0x6);
        }
    }
    flags.z = val == 0;
    flags.h = false;
    val
}

impl Cpu {
    /// Constructs a new CPU with each register set to 0.
    pub fn new() -> Self {
        Self {
            ..Default::default()
        }
    }

    /// Get the top four bits of the F register
    pub fn flags(&self) -> &Flags {
        &self.f
    }

    pub fn flags_mut(&mut self) -> &mut Flags {
        &mut self.f
    }

    /// Returns the value of the Z flag
    pub fn zero_flag(&self) -> bool {
        self.f.z
    }

    /// Returns the value of the N flag
    pub fn subtraction_flag(&self) -> bool {
        self.f.n
    }

    /// Returns the value of the H flag
    pub fn half_carry_flag(&self) -> bool {
        self.f.h
    }

    /// Returns the value of the Z flag
    pub fn carry_flag(&self) -> bool {
        self.f.c
    }

    /// Determines what the CPU should do next. Included in this, is a check for interrupts.
    pub fn read_op(&self) -> Instruction {
        match self.state {
            CpuState::Running => parse_instruction(self.ir.0),
            CpuState::Halted => Instruction::Stall,
            CpuState::Stopped => Instruction::Stopped,
        }
    }

    /// Determines what the CPU should do next
    pub fn read_prefixed_op(&self) -> PrefixedInstruction {
        parse_prefixed_instruction(self.ir.0)
    }

    pub fn inc_pc(&mut self) {
        self.pc += 1u16;
    }

    pub fn execute(&mut self, cycle: MCycle, mem: &mut impl MemoryLike) {
        let MCycle {
            addr_bus,
            action,
            idu,
            alu,
        } = cycle;
        let addr = match addr_bus {
            PointerReg::PC => self.pc.0,
            PointerReg::SP => self.sp.0,
            PointerReg::HL => self.ptr(),
            PointerReg::BC => self.bc(),
            PointerReg::DE => self.de(),
            PointerReg::Ghost => self.ghost_addr(),
        };
        let addr = Wrapping(addr);
        // TODO: There might be a better way to sequence this. For reads (and noops), we want to
        // perform the action then execute the IDU and ALU signals. For writes, we want the
        // reverse. Verify this...
        match action {
            AddrAction::Read(loc) => {
                let byte = Wrapping(mem.read_byte(addr.0));
                match loc {
                    ReadLocation::InstrRegister => self.ir = byte,
                    ReadLocation::RegisterZ => self.z = byte,
                    ReadLocation::RegisterW => self.w = byte,
                    ReadLocation::Other(DataLocation::Bus) => self.z = byte,
                    ReadLocation::Other(DataLocation::Register(reg)) => self.write_reg(reg, byte),
                    ReadLocation::Other(DataLocation::Literal(_)) => todo!(),
                }
            }
            AddrAction::Write(loc) => {
                let byte = match loc {
                    DataLocation::Bus => self.z.0,
                    DataLocation::Register(reg) => self.read_reg(reg).0,
                    DataLocation::Literal(val) => val,
                };
                mem.write_byte(addr.0, byte);
            }
            AddrAction::Noop => {}
        }
        if let Some((signal, reg)) = idu {
            let addr = match signal {
                IduSignal::Inc => addr + 1u16,
                IduSignal::Dec => addr - 1u16,
                IduSignal::Noop => addr,
            };
            match reg {
                FullRegister::AF => self.write_af(addr.0),
                FullRegister::BC => self.write_bc(addr.0),
                FullRegister::DE => self.write_de(addr.0),
                FullRegister::HL => self.write_hl(addr.0),
                FullRegister::SP => self.sp = addr,
                FullRegister::PC => self.pc = addr,
            }
        }
        let Some(signal) = alu else { return };
        let AluSignal {
            input_one,
            input_two,
            op,
            output,
        } = signal;
        let val_one = match input_one {
            DataLocation::Bus => self.z,
            DataLocation::Register(reg) => self.read_reg(reg),
            DataLocation::Literal(val) => Wrapping(val),
        };
        let val_two = match input_two {
            DataLocation::Bus => self.z,
            DataLocation::Register(reg) => self.read_reg(reg),
            DataLocation::Literal(val) => Wrapping(val),
        };
        let byte = match op {
            AluOp::Move => val_one,
            AluOp::SignedAdd => val_one.add_signed(val_two.0 as i8),
            AluOp::Add => {
                let (val, carry) = val_one.overflowing_add(val_two);
                self.f.z = val == 0;
                self.f.n = false;
                self.f.h = (val_one.0 & 0x0F) + (val_two.0 & 0x0F) > 0x0F;
                self.f.c = carry;
                val
            }
            AluOp::Adc => {
                let h = (val_one.0 & 0x0F) + (val_two.0 & 0x0F) + (self.f.c as u8) > 0x0F;
                let (val, carry) = val_one.overflowing_add(val_two);
                let (val, c) = val.overflowing_add(self.f.c as u8);
                self.f.z = val == 0;
                self.f.n = false;
                self.f.h = h;
                self.f.c = carry | c;
                val
            }
            AluOp::Subtract => {
                let (val, carry) = val_one.overflowing_sub(val_two);
                self.f.z = val == 0;
                self.f.n = true;
                self.f.h = (val_one.0 & 0x0F).overflowing_sub(val_two.0 & 0x0F).1;
                self.f.c = carry;
                val
            }
            AluOp::Sbc => {
                let (val, carry) = val_one.overflowing_sub(val_two);
                let (val, c) = val.overflowing_sub(self.f.c as u8);
                let (v, h1) = (val_one.0 & 0x0F).overflowing_sub(val_two.0 & 0x0F);
                let (_, h2) = v.overflowing_sub(self.f.c as u8);
                self.f.z = val == 0;
                self.f.n = true;
                self.f.h = h1 | h2;
                self.f.c = carry | c;
                val
            }
            AluOp::And => {
                let val = val_one & val_two;
                self.f.z = val == 0;
                self.f.n = false;
                self.f.h = true;
                self.f.c = false;
                val
            }
            AluOp::Or => {
                let val = val_one | val_two;
                self.f.z = val == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
                val
            }
            AluOp::Xor => {
                let val = val_one ^ val_two;
                self.f.z = val == 0;
                self.f.n = false;
                self.f.h = false;
                self.f.c = false;
                val
            }
            AluOp::Cp => {
                let (val, carry) = val_one.overflowing_sub(val_two);
                self.f.z = val == 0;
                self.f.n = true;
                self.f.h = (val_one.0 & 0x0F).overflowing_sub(val_two.0 & 0x0F).1;
                self.f.c = carry;
                val_one
            }
            AluOp::Bit(bit) => {
                self.f.z = !check_bit(bit, val_one.0);
                self.f.n = false;
                self.f.h = true;
                val_one
            }
            AluOp::Res(bit) => Wrapping(val_one.0 & (!(1 << bit))),
            AluOp::Set(bit) => Wrapping(val_one.0 | (1 << bit)),
            AluOp::Rl => {
                let carry = self.f.c as u8;
                let mask = carry | (carry << 7);
                let [c, new] = (u16::from_be_bytes([mask, val_one.0]).rotate_left(1)).to_be_bytes();
                let new_carry = c & 1 != 0;
                self.f.set_for_byte_shift_op(val_one == 0, new_carry);
                new.into()
            }
            AluOp::Rlc => {
                let byte = val_one.0.rotate_left(1);
                let carry = check_bit_const::<0>(byte);
                self.f.set_for_byte_shift_op(byte == 0, carry);
                byte.into()
            }
            AluOp::Rr => {
                let mask = self.f.c as u8;
                let [new, c] = u16::from_be_bytes([val_one.0, mask])
                    .rotate_right(1)
                    .to_be_bytes();
                let new_carry = c == 0x80;
                self.f.set_for_byte_shift_op(new == 0, new_carry);
                new.into()
            }
            AluOp::Rrc => {
                let byte = val_one.0.rotate_right(1);
                let carry = check_bit_const::<7>(byte);
                self.f.set_for_byte_shift_op(byte == 0, carry);
                byte.into()
            }
            AluOp::Sla => {
                let [c, new] = u16::from_be_bytes([0, val_one.0])
                    .rotate_left(1)
                    .to_be_bytes();
                let new_carry = c != 0;
                self.f.set_for_byte_shift_op(new == 0, new_carry);
                new.into()
            }
            AluOp::Sra => {
                let bit = select_bit::<7>(val_one.0);
                let [mut new, c] = u16::from_be_bytes([val_two.0, 0])
                    .rotate_right(1)
                    .to_be_bytes();
                new |= bit;
                let carry = c == 0x80;
                self.f.set_for_byte_shift_op(new == 0, carry);
                new.into()
            }
            AluOp::Swap => {
                let lw = (val_one.0 & 0xF0) >> 4;
                let hi = (val_one.0 & 0x0F) << 4;
                let byte = hi | lw;
                self.f.set_for_byte_shift_op(byte == 0, false);
                byte.into()
            }
            AluOp::Srl => {
                let [new, c] = u16::from_be_bytes([val_one.0, 0])
                    .rotate_right(1)
                    .to_be_bytes();
                let carry = c == 0x80;
                self.f.set_for_byte_shift_op(new == 0, carry);
                new.into()
            }
            AluOp::Daa => Wrapping(to_bcd(self.a.0, &mut self.f)),
            AluOp::Rla => {
                let byte = self.a.0;
                let carry = self.f.c as u8;
                let mask = carry | (carry << 7);
                let [c, new] = (u16::from_be_bytes([mask, byte]).rotate_left(1)).to_be_bytes();
                self.f.set_for_byte_shift_op(false, c & 1 != 0);
                Wrapping(new)
            }
            AluOp::Rlca => {
                let byte = self.a.0;
                let [c, new] = u16::from_be_bytes([0, byte]).rotate_left(1).to_be_bytes();
                self.f.set_for_byte_shift_op(false, c == 1);
                Wrapping(c | new)
            }
            AluOp::Rra => {
                let mask = self.f.c as u8;
                let byte = self.a.0;
                let [new, c] = u16::from_be_bytes([byte, mask])
                    .rotate_right(1)
                    .to_be_bytes();
                self.f.set_for_byte_shift_op(false, c == 0x80);
                Wrapping(new)
            }
            AluOp::Rrca => {
                let byte = self.a.0;
                let [new, c] = u16::from_be_bytes([byte, 0]).rotate_right(1).to_be_bytes();
                let carry = c == 0x80;
                self.f.set_for_byte_shift_op(false, carry);
                Wrapping(c | new)
            }
        };
        match output {
            DataLocation::Bus => self.z = byte,
            DataLocation::Register(reg) => self.write_reg(reg, byte),
            DataLocation::Literal(_) => todo!(),
        }
    }

    /// Moves the data in the WZ "ghost" registers into the specified wide register.
    pub fn ghost_move(&mut self, reg: WideReg) {
        let addr = self.ghost_addr();
        match reg {
            WideReg::BC => self.write_bc(addr),
            WideReg::DE => self.write_de(addr),
            WideReg::HL => self.write_hl(addr),
            WideReg::SP => self.sp = Wrapping(addr),
        }
    }

    /// Moves the data in the WZ "ghost" registers into the specified wide register.
    pub fn ghost_move_wide_reg(&mut self, reg: WideRegWithoutSP) {
        let addr = self.ghost_addr();
        match reg {
            WideRegWithoutSP::BC => self.write_bc(addr),
            WideRegWithoutSP::DE => self.write_de(addr),
            WideRegWithoutSP::HL => self.write_hl(addr),
            WideRegWithoutSP::AF => self.write_af(addr),
        }
    }

    pub fn ghost_addr(&self) -> u16 {
        u16::from_be_bytes([self.w.0, self.z.0])
    }

    pub fn ptr(&self) -> u16 {
        u16::from_be_bytes([self.h.0, self.l.0])
    }

    pub fn copy_byte(&self, mem: &impl MemoryLike, reg: RegOrPointer) -> u8 {
        match reg {
            RegOrPointer::Reg(reg) => self.read_reg(reg).0,
            RegOrPointer::Pointer => mem.read_byte(self.ptr()),
        }
    }

    fn write_af(&mut self, val: u16) {
        let [a, f] = val.to_be_bytes();
        self.a = Wrapping(a);
        self.f.set_from_byte(f);
    }

    pub(crate) fn bc(&self) -> u16 {
        u16::from_be_bytes([self.b.0, self.c.0])
    }

    fn write_bc(&mut self, val: u16) {
        let [b, c] = val.to_be_bytes().map(Wrapping);
        self.b = b;
        self.c = c;
    }

    pub(crate) fn de(&self) -> u16 {
        u16::from_be_bytes([self.d.0, self.e.0])
    }

    fn write_de(&mut self, val: u16) {
        let [d, e] = val.to_be_bytes().map(Wrapping);
        self.d = d;
        self.e = e;
    }

    fn write_hl(&mut self, val: u16) {
        let [h, l] = val.to_be_bytes().map(Wrapping);
        self.h = h;
        self.l = l;
    }

    const fn read_reg(&self, index: HalfRegister) -> Wrapping<u8> {
        match index {
            HalfRegister::A => self.a,
            HalfRegister::B => self.b,
            HalfRegister::C => self.c,
            HalfRegister::D => self.d,
            HalfRegister::E => self.e,
            HalfRegister::H => self.h,
            HalfRegister::L => self.l,
            HalfRegister::F => Wrapping(self.f.as_byte()),
        }
    }

    fn write_reg(&mut self, index: HalfRegister, val: impl Into<Wrapping<u8>>) {
        let val = val.into();
        match index {
            HalfRegister::A => self.a = val,
            HalfRegister::B => self.b = val,
            HalfRegister::C => self.c = val,
            HalfRegister::D => self.d = val,
            HalfRegister::E => self.e = val,
            HalfRegister::H => self.h = val,
            HalfRegister::L => self.l = val,
            HalfRegister::F => self.f.set_from_byte(val.0),
        }
    }
}
