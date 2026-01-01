use std::hash::Hash;
use std::ops::Index;
use std::ops::IndexMut;

use serde::Deserialize;
use serde::Serialize;

use crate::instruction::AddrAction;
use crate::instruction::AluOp;
use crate::instruction::AluSignal;
use crate::instruction::ArithmeticOp;
use crate::instruction::BitOp;
use crate::instruction::BitShiftOp;
use crate::instruction::Condition;
use crate::instruction::ControlOp;
use crate::instruction::DataLocation;
use crate::instruction::HalfRegister;
use crate::instruction::IduSignal;
use crate::instruction::Instruction;
use crate::instruction::InterruptOp;
use crate::instruction::JumpOp;
use crate::instruction::LoadOp;
use crate::instruction::MCycle;
use crate::instruction::PointerReg;
use crate::instruction::ReadLocation;
use crate::instruction::RegOrPointer;
use crate::instruction::SomeByte;
use crate::instruction::WideReg;
use crate::instruction::WideRegWithoutSP;
use crate::lookup::parse_instruction;
use crate::mem::MemoryLikeExt;
use crate::utils::Wrapping;

#[cfg(test)]
mod cpu_tests;

#[derive(
    Debug, Default, Hash, Clone, PartialEq, Eq, derive_more::Display, Serialize, Deserialize,
)]
#[display(
    "CPU {{ A=0x{:0>2X} F={} B=0x{:0>2X} C=0x{:0>2X} D=0x{:0>2X} E=0x{:0>2X} H=0x{:0>2X} L=0x{:0>2X}, Z={:0>2X}, W={:0>2X}, IR={:0>2X} SP=0x{:0>2X} PC=0x{:0>2X} IME={} Done={} }}",
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
    "Flags(Z={} N={} H={} C={})",
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

    pub fn as_byte(&self) -> u8 {
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

fn addition_operation(val: &mut u8, op: u8, flags: &mut Flags) {
    let (a, carry) = val.overflowing_add(op);
    flags.z = a == 0;
    flags.n = false;
    flags.h = (*val & 0x0F) + (op & 0x0F) > 0x0F;
    flags.c = carry;
    *val = a;
}

fn subtraction_operation(val: &mut u8, op: u8, flags: &mut Flags) {
    let (a, _carry) = val.overflowing_sub(op);
    flags.z = a == 0;
    flags.n = true;
    flags.h = (*val & 0x0F).overflowing_sub(op & 0x0F).1;
    flags.c = a > *val;
    *val = a;
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
        parse_instruction(self.ir.0)
    }

    pub fn inc_pc(&mut self) {
        self.pc += 1u16;
    }

    pub fn execute(&mut self, cycle: MCycle, mem: &mut impl MemoryLikeExt) {
        // println!("Executing cycle: {cycle:?}");
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
        // TODO: There might be a better way to sequence this. For reads (and noops), we want to
        // perform the action then execute the IDU and ALU signals. For writes, we want the
        // reverse. Verify this...
        match action {
            AddrAction::Read(loc) => {
                let byte = Wrapping(mem.read_byte(addr));
                match loc {
                    ReadLocation::InstrRegister => self.ir = byte,
                    ReadLocation::RegisterZ => self.z = byte,
                    ReadLocation::RegisterW => self.w = byte,
                    ReadLocation::Other(DataLocation::Bus) => self.z = byte,
                    ReadLocation::Other(DataLocation::Register(reg)) => self[reg] = byte,
                    ReadLocation::Other(DataLocation::Literal(_)) => todo!(),
                }
            }
            AddrAction::Write(loc) => {
                let byte = match loc {
                    DataLocation::Bus => self.z.0,
                    DataLocation::Register(reg) => self[reg].0,
                    DataLocation::Literal(val) => val,
                };
                mem.write_byte(addr, byte);
            }
            AddrAction::Noop => {}
        }
        if let Some((signal, reg)) = idu {
            let addr = match signal {
                IduSignal::Inc => addr + 1,
                IduSignal::Dec => addr - 1,
                IduSignal::Noop => addr,
            };
            match reg {
                FullRegister::AF => self.write_af(addr),
                FullRegister::BC => self.write_bc(addr),
                FullRegister::DE => self.write_de(addr),
                FullRegister::HL => self.write_hl(addr),
                FullRegister::SP => self.sp = Wrapping(addr),
                FullRegister::PC => self.pc = Wrapping(addr),
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
            DataLocation::Register(reg) => self[reg],
            DataLocation::Literal(val) => Wrapping(val),
        };
        let val_two = match input_two {
            DataLocation::Bus => self.z,
            DataLocation::Register(reg) => self[reg],
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
        };
        match output {
            DataLocation::Bus => self.z = byte,
            DataLocation::Register(reg) => self[reg] = byte,
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

    /*
    pub fn execute(&mut self, instr: Instruction, mem: &mut impl MemoryLikeExt) {
        // info!("{instr}");
        let len = instr.size();
        self.pc += (0x1 & self.state as u16) * (len as u16);
        match instr {
            Instruction::Load(op) => self.execute_load_op(op, mem),
            Instruction::BitShift(op) => self.execute_bit_shift_op(op, mem),
            Instruction::ControlOp(op) => self.execute_control_op(op, mem),
            Instruction::Bit(op) => self.execute_bit_op(op, mem),
            Instruction::Jump(op) => self.execute_jump_op(op, mem),
            Instruction::Arithmetic(op) => self.execute_arithmetic_op(op, mem),
            Instruction::Interrupt(op) => self.execute_interrupt(op, mem),
            Instruction::Daa => self.a = Wrapping(to_bcd(self.a.0, &mut self.f)),
            Instruction::Scf => {
                self.f.n = false;
                self.f.h = false;
                self.f.c = true;
            }
            Instruction::Cpl => {
                self.a = !self.a;
                self.f.n = true;
                self.f.h = true;
            }
            Instruction::Ccf => {
                self.f.n = false;
                self.f.h = false;
                self.f.c = !self.f.c;
            }
            Instruction::Di => self.disable_interupts(),
            Instruction::Ei => return self.enable_interupts(),
            Instruction::Transfer => {
                if matches!(self.state, CpuState::Running) {
                    mem.vram_transfer()
                }
            }
        }
        self.ime |= self.to_set_ime;
        self.to_set_ime = false;
    }
    */

    pub(crate) fn enable_interupts(&mut self) {
        self.to_set_ime = true;
    }

    pub(crate) fn disable_interupts(&mut self) {
        self.ime = false;
    }

    pub fn ptr(&self) -> u16 {
        u16::from_be_bytes([self.h.0, self.l.0])
    }

    fn pc_bytes(&self) -> [u8; 2] {
        u16::to_be_bytes(self.pc.0)
    }

    fn set_ptr(&mut self, val: u16) {
        let [h, l] = u16::to_be_bytes(val).map(Wrapping);
        self.h = h;
        self.l = l;
    }

    fn read_wide_reg(&self, reg: WideReg) -> u16 {
        match reg {
            WideReg::BC => self.bc(),
            WideReg::DE => self.de(),
            WideReg::HL => self.ptr(),
            WideReg::SP => self.sp.0,
        }
    }

    fn write_wide_reg(&mut self, reg: WideReg, val: u16) {
        match reg {
            WideReg::BC => self.write_bc(val),
            WideReg::DE => self.write_de(val),
            WideReg::HL => self.set_ptr(val),
            WideReg::SP => self.sp = Wrapping(val),
        }
    }

    fn update_wide_reg<F>(&mut self, reg: WideReg, update: F)
    where
        F: FnOnce(&mut u16),
    {
        let mut value = self.read_wide_reg(reg);
        update(&mut value);
        self.write_wide_reg(reg, value);
    }

    fn write_wide_reg_without_sp(&mut self, reg: WideRegWithoutSP, val: u16) {
        match reg {
            WideRegWithoutSP::BC => self.write_bc(val),
            WideRegWithoutSP::DE => self.write_de(val),
            WideRegWithoutSP::HL => self.set_ptr(val),
            WideRegWithoutSP::AF => self.write_af(val),
        }
    }

    fn execute_arithmetic_op(&mut self, op: ArithmeticOp, mem: &mut impl MemoryLikeExt) {
        /*
            match op {
                ArithmeticOp::AddSP(val) => {
                    let (sp, _c) = self.sp.0.overflowing_add_signed(val as i16);
                    self.f.z = false;
                    self.f.n = false;
                    self.f.h = (self.sp.0 << 12) > (sp << 12);
                    self.f.c = (self.sp.0 << 8) > (sp << 8);
                    self.sp = Wrapping(sp);
                }
                ArithmeticOp::Inc16(reg) => {
                    self.update_wide_reg(reg, |value| *value = value.wrapping_add(1))
                }
                ArithmeticOp::Dec16(reg) => {
                    self.update_wide_reg(reg, |value| *value = value.wrapping_sub(1))
                }
                ArithmeticOp::Add16(reg) => {
                    let value = self.read_wide_reg(reg);
                    let ptr = self.ptr();
                    self.f.h = (ptr & 0x0FFF) + (value & 0x0FFF) > 0x0FFF;
                    let (ptr, carry) = ptr.overflowing_add(value);
                    self.set_ptr(ptr);
                    self.f.n = false;
                    self.f.c = carry;
                }
                ArithmeticOp::Add(byte) => {
                    let byte = self.unwrap_some_byte(mem, byte);
                    addition_operation(&mut self.a.0, byte, &mut self.f);
                }
                ArithmeticOp::Adc(byte) => {
                    let byte = self.unwrap_some_byte(mem, byte);
                    let h = (byte & 0x0F) + (self.f.c as u8) + (self.a.0 & 0x0F) > 0x0F;
                    let (byte, carry) = byte.overflowing_add(self.f.c as u8);
                    addition_operation(&mut self.a.0, byte, &mut self.f);
                    self.f.h = h;
                    self.f.c |= carry;
                }
                ArithmeticOp::Sub(byte) => {
                    let byte = self.unwrap_some_byte(mem, byte);
                    subtraction_operation(&mut self.a.0, byte, &mut self.f);
                }
                ArithmeticOp::Sbc(byte) => {
                    let byte = self.unwrap_some_byte(mem, byte);
                    let (a, carry1) = self.a.0.overflowing_sub(byte);
                    let (a, carry2) = a.overflowing_sub(self.f.c as u8);
                    let (val, h1) = (self.a.0 & 0x0F).overflowing_sub(byte & 0x0F);
                    let h2 = (val & 0x0F).overflowing_sub(self.f.c as u8).1;
                    self.f.z = a == 0;
                    self.f.n = true;
                    self.f.h = h1 | h2;
                    self.f.c = carry1 | carry2;
                    self.a = a.into();
                }
                ArithmeticOp::And(byte) => {
                    let byte = self.unwrap_some_byte(mem, byte);
                    self.a &= byte;
                    self.f.z = self.a.0 == 0;
                    self.f.n = false;
                    self.f.h = true;
                    self.f.c = false;
                }
                ArithmeticOp::Xor(byte) => {
                    let byte = self.unwrap_some_byte(mem, byte);
                    self.a ^= byte;
                    self.f.z = self.a.0 == 0;
                    self.f.n = false;
                    self.f.h = false;
                    self.f.c = false;
                }
                ArithmeticOp::Or(byte) => {
                    let byte = self.unwrap_some_byte(mem, byte);
                    self.a |= byte;
                    self.f.z = self.a.0 == 0;
                    self.f.n = false;
                    self.f.h = false;
                    self.f.c = false;
                }
                ArithmeticOp::Cp(byte) => {
                    let byte = self.unwrap_some_byte(mem, byte);
                    let mut a = self.a;
                    subtraction_operation(&mut a.0, byte, &mut self.f);
                }
                ArithmeticOp::Inc(reg) => {
                    let mut h = false;
                    let val = self.update_byte(reg, mem, |byte| {
                        let b = *byte;
                        h = b & 0x0F == 0x0F;
                        *byte = b.wrapping_add(1);
                    });
                    self.f.z = val == 0;
                    self.f.n = false;
                    self.f.h = h;
                }
                ArithmeticOp::Dec(reg) => {
                    let mut h = false;
                    let val = self.update_byte(reg, mem, |byte| {
                        let b = *byte;
                        h = check_bit_const::<4>(b);
                        *byte = b.wrapping_sub(1);
                    });
                    self.f.z = val == 0;
                    self.f.n = true;
                    self.f.h = h ^ check_bit_const::<4>(val);
                }
            }
        */
        panic!()
    }

    fn unwrap_some_byte(&self, _mem: &impl MemoryLikeExt, _byte: SomeByte) -> u8 {
        /*
        match byte {
            SomeByte::Direct(byte) => byte,
            SomeByte::Referenced(reg) => self.copy_byte(mem, reg),
        }
        */
        panic!("This method will no longer be used")
    }

    pub fn copy_byte(&self, mem: &impl MemoryLikeExt, reg: RegOrPointer) -> u8 {
        match reg {
            RegOrPointer::Reg(reg) => self[reg].0,
            RegOrPointer::Pointer => mem.read_byte(self.ptr()),
        }
    }

    #[track_caller]
    fn update_byte(
        &mut self,
        reg: RegOrPointer,
        mem: &mut impl MemoryLikeExt,
        update: impl FnOnce(&mut u8),
    ) -> u8 {
        match reg {
            RegOrPointer::Reg(reg) => {
                update(&mut self[reg].0);
                self[reg].0
            }
            RegOrPointer::Pointer => mem.update_byte(self.ptr(), update),
        }
    }

    /// Stores the given byte into either an (half) register or into the MemoryMap using the HL
    /// register as an index.
    fn write_byte(&mut self, reg: RegOrPointer, mem: &mut impl MemoryLikeExt, val: u8) {
        match reg {
            RegOrPointer::Reg(reg) => self[reg].0 = val,
            RegOrPointer::Pointer => mem.write_byte(self.ptr(), val),
        }
    }

    fn matches(&self, cond: Condition) -> bool {
        match cond {
            Condition::Zero => self.f.z,
            Condition::NotZero => !self.f.z,
            Condition::Carry => self.f.c,
            Condition::NotCarry => !self.f.c,
        }
    }

    fn push_pc(&mut self, mem: &mut impl MemoryLikeExt) {
        let [hi, lo] = self.pc_bytes();
        self.sp -= 1u16;
        mem.write_byte(self.sp.0, hi);
        self.sp -= 1u16;
        mem.write_byte(self.sp.0, lo);
    }

    fn pop_pc(&mut self, mem: &mut impl MemoryLikeExt) -> u16 {
        let lo = mem.read_byte(self.sp.0);
        self.sp += 1u16;
        let hi = mem.read_byte(self.sp.0);
        self.sp += 1u16;
        u16::from_be_bytes([hi, lo])
    }

    fn execute_interrupt(&mut self, op: InterruptOp, mem: &mut impl MemoryLikeExt) {
        mem.clear_interrupt_req(op);
        self.disable_interupts();
        self.push_pc(mem);
        self.pc = Wrapping(op as u16);
    }

    fn execute_jump_op(&mut self, op: JumpOp, mem: &mut impl MemoryLikeExt) {
        /*
        match op {
            JumpOp::ConditionalRelative(cond, val) => {
                if self.matches(cond) {
                    if val < 0 {
                        self.pc -= val.unsigned_abs() as u16;
                    } else {
                        self.pc += val as u16;
                    }
                }
            }
            JumpOp::Relative(val) => {
                if val < 0 {
                    self.pc -= val.unsigned_abs() as u16;
                } else {
                    self.pc += val as u16;
                }
            }
            JumpOp::ConditionalAbsolute(cond, dest) => {
                if self.matches(cond) {
                    self.pc = Wrapping(dest);
                }
            }
            JumpOp::Absolute(dest) => self.pc = Wrapping(dest),
            JumpOp::JumpToHL => self.pc = Wrapping(self.ptr()),
            JumpOp::Call(ptr) => {
                self.push_pc(mem);
                self.pc = Wrapping(ptr);
            }
            JumpOp::ConditionalCall(cond, dest) => {
                if self.matches(cond) {
                    self.push_pc(mem);
                    self.pc = Wrapping(dest);
                }
            }
            JumpOp::Return => self.pc = Wrapping(self.pop_pc(mem)),
            JumpOp::ConditionalReturn(cond) => {
                if self.matches(cond) {
                    self.pc = Wrapping(self.pop_pc(mem));
                }
            }
            JumpOp::ReturnAndEnable => {
                self.enable_interupts();
                self.state = CpuState::Running;
                self.pc = Wrapping(self.pop_pc(mem));
            }
            JumpOp::RST00 => self.rst::<0x00>(mem),
            JumpOp::RST08 => self.rst::<0x08>(mem),
            JumpOp::RST10 => self.rst::<0x10>(mem),
            JumpOp::RST18 => self.rst::<0x18>(mem),
            JumpOp::RST20 => self.rst::<0x20>(mem),
            JumpOp::RST28 => self.rst::<0x28>(mem),
            JumpOp::RST30 => self.rst::<0x30>(mem),
            JumpOp::RST38 => self.rst::<0x38>(mem),
        }
        */
        panic!("This function will soon be removed");
    }

    fn rst<const N: u16>(&mut self, mem: &mut impl MemoryLikeExt) {
        self.push_pc(mem);
        self.pc = Wrapping(N);
    }

    fn execute_control_op(&mut self, op: ControlOp, _mem: &mut impl MemoryLikeExt) {
        match op {
            ControlOp::Noop => {}
            ControlOp::Halt => self.halt(),
            ControlOp::Stop => self.state = CpuState::Stopped,
        }
    }

    fn execute_bit_op(&mut self, op: BitOp, mem: &mut impl MemoryLikeExt) {
        /*
        let BitOp { bit, reg, op } = op;
        debug_assert!(bit < 8);
        let byte = self.copy_byte(mem, reg);
        match op {
            BitOpInner::Bit => {
                self.f.z = !check_bit(bit, byte);
                self.f.n = false;
                self.f.h = true;
            }
            BitOpInner::Res => {
                self.update_byte(reg, mem, |byte| *byte &= !(0x1 << bit));
            }
            BitOpInner::Set => {
                self.update_byte(reg, mem, |byte| *byte |= 0x1 << bit);
            }
        }
        */
        panic!()
    }

    fn execute_bit_shift_op(&mut self, op: BitShiftOp, mem: &mut impl MemoryLikeExt) {
        match op {
            BitShiftOp::Rlc(reg) => {
                let mut carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    *byte = (*byte).rotate_left(1);
                    carry = check_bit_const::<0>(*byte);
                });
                self.f.set_for_byte_shift_op(byte == 0, carry)
            }
            BitShiftOp::Rlca => {
                let byte = self.a.0;
                let [c, new] = u16::from_be_bytes([0, byte]).rotate_left(1).to_be_bytes();
                self.a = Wrapping(c | new);
                self.f.set_for_byte_shift_op(false, c == 1)
            }
            BitShiftOp::Rrc(reg) => {
                let mut carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    *byte = (*byte).rotate_right(1);
                    carry = check_bit_const::<7>(*byte);
                });
                self.f.set_for_byte_shift_op(byte == 0, carry)
            }
            BitShiftOp::Rrca => {
                let byte = self.a.0;
                let [new, c] = u16::from_be_bytes([byte, 0]).rotate_right(1).to_be_bytes();
                let carry = c == 0x80;
                self.a = Wrapping(c | new);
                self.f.set_for_byte_shift_op(false, carry)
            }
            BitShiftOp::Rl(reg) => {
                let carry = self.f.c as u8;
                let mask = carry | (carry << 7);
                let mut new_carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let [c, new] = (u16::from_be_bytes([mask, *byte]).rotate_left(1)).to_be_bytes();
                    new_carry = c & 1 != 0;
                    *byte = new;
                });
                self.f.set_for_byte_shift_op(byte == 0, new_carry);
            }
            BitShiftOp::Rla => {
                let byte = self.a.0;
                let carry = self.f.c as u8;
                let mask = carry | (carry << 7);
                let [c, new] = (u16::from_be_bytes([mask, byte]).rotate_left(1)).to_be_bytes();
                self.a = Wrapping(new);
                self.f.set_for_byte_shift_op(false, c & 1 != 0);
            }
            BitShiftOp::Rr(reg) => {
                let mask = self.f.c as u8;
                let mut new_carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let [new, c] = u16::from_be_bytes([*byte, mask])
                        .rotate_right(1)
                        .to_be_bytes();
                    new_carry = c == 0x80;
                    *byte = new;
                });
                self.f.set_for_byte_shift_op(byte == 0, new_carry)
            }
            BitShiftOp::Rra => {
                let mask = self.f.c as u8;
                let byte = self.a.0;
                let [new, c] = u16::from_be_bytes([byte, mask])
                    .rotate_right(1)
                    .to_be_bytes();
                self.a = Wrapping(new);
                self.f.set_for_byte_shift_op(false, c == 0x80)
            }
            BitShiftOp::Sla(reg) => {
                let mut new_carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let [c, new] = u16::from_be_bytes([0, *byte]).rotate_left(1).to_be_bytes();
                    new_carry = c != 0;
                    *byte = new;
                });
                self.f.set_for_byte_shift_op(byte == 0, new_carry)
            }
            BitShiftOp::Sra(reg) => {
                let mut carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let bit = select_bit::<7>(*byte);
                    let [new, c] = u16::from_be_bytes([*byte, 0]).rotate_right(1).to_be_bytes();
                    carry = c == 0x80;
                    *byte = new | bit;
                });
                self.f.set_for_byte_shift_op(byte == 0, carry)
            }
            BitShiftOp::Swap(reg) => {
                let byte = self.update_byte(reg, mem, |byte| {
                    let lw = (*byte & 0xF0) >> 4;
                    let hi = (*byte & 0x0F) << 4;
                    *byte = hi | lw;
                });
                self.f.set_for_byte_shift_op(byte == 0, false)
            }
            BitShiftOp::Srl(reg) => {
                let mut carry = false;
                let byte = self.update_byte(reg, mem, |byte| {
                    let [new, c] = u16::from_be_bytes([*byte, 0]).rotate_right(1).to_be_bytes();
                    carry = c == 0x80;
                    *byte = new;
                });
                self.f.set_for_byte_shift_op(byte == 0, carry)
            }
        }
    }

    fn halt(&mut self) {
        self.state = CpuState::Halted;
        self.pc -= Wrapping(ControlOp::Halt.size() as u16);
    }

    fn execute_load_op(&mut self, op: LoadOp, mem: &mut impl MemoryLikeExt) {
        /*
            match op {
                LoadOp::Basic {
                    dest: RegOrPointer::Pointer,
                    src: RegOrPointer::Pointer,
                } => self.halt(),
                LoadOp::Basic { dest, src } => self.write_byte(dest, mem, self.copy_byte(mem, src)),
                LoadOp::Direct16(reg, val) => self.write_wide_reg(reg, val),
                LoadOp::Direct(reg, val) => self.write_byte(reg, mem, val),
                LoadOp::LoadIntoA(ptr) => {
                    let index = match ptr {
                        LoadAPointer::BC => self.bc(),
                        LoadAPointer::DE => self.de(),
                        LoadAPointer::Hli => {
                            let digest = self.ptr();
                            self.inc_ptr(1);
                            digest
                        }
                        LoadAPointer::Hld => {
                            let digest = self.ptr();
                            self.dec_ptr(1);
                            digest
                        }
                    };
                    self.a.0 = mem.read_byte(index);
                }
                LoadOp::StoreFromA(ptr) => {
                    let index = match ptr {
                        LoadAPointer::BC => self.bc(),
                        LoadAPointer::DE => self.de(),
                        LoadAPointer::Hli => {
                            let digest = self.ptr();
                            self.inc_ptr(1);
                            digest
                        }
                        LoadAPointer::Hld => {
                            let digest = self.ptr();
                            self.dec_ptr(1);
                            digest
                        }
                    };
                    mem.write_byte(index, self.a.0)
                }
                LoadOp::StoreSP(val) => {
                    let [hi, lo] = self.sp.0.to_be_bytes();
                    mem.write_byte(val, lo);
                    mem.write_byte(val + 1, hi);
                }
                LoadOp::HLIntoSP => self.sp = Wrapping(self.ptr()),
                LoadOp::SPIntoHL(val) => {
                    let sp = self.sp.0.wrapping_add_signed(val as i16);
                    self.f.z = false;
                    self.f.n = false;
                    self.f.h = (self.sp.0 << 12) > (sp << 12);
                    self.f.c = (self.sp.0 << 8) > (sp << 8);
                    let [h, l] = sp.to_be_bytes().map(Wrapping);
                    self.h = h;
                    self.l = l;
                }
                LoadOp::Pop(reg) => {
                    let ptr = self.pop_pc(mem);
                    self.write_wide_reg_without_sp(reg, ptr);
                }
                LoadOp::Push(reg) => {
                    let [a, b] = match reg {
                        WideRegWithoutSP::BC => [self.b.0, self.c.0],
                        WideRegWithoutSP::DE => [self.d.0, self.e.0],
                        WideRegWithoutSP::HL => [self.h.0, self.l.0],
                        WideRegWithoutSP::AF => [self.a.0, self.f.as_byte()],
                    };
                    self.sp -= 1u16;
                    mem.write_byte(self.sp.0, a);
                    self.sp -= 1u16;
                    mem.write_byte(self.sp.0, b);
                }
                LoadOp::LoadHigh(val) => {
                    trace!("LoadHigh(0x{val:0>2X})");
                    mem.write_byte(u16::from_be_bytes([0xFF, val]), self.a.0);
                }
                LoadOp::StoreHigh(val) => self.a.0 = mem.read_byte(u16::from_be_bytes([0xFF, val])),
                LoadOp::Ldhca => mem.write_byte(u16::from_be_bytes([0xFF, self.c.0]), self.a.0),
                LoadOp::Ldhac => self.a.0 = mem.read_byte(u16::from_be_bytes([0xFF, self.c.0])),
                LoadOp::LoadA { ptr } => mem.write_byte(ptr, self.a.0),
                LoadOp::StoreA { ptr } => self.a.0 = mem.read_byte(ptr),
            }
        */
        panic!()
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

    fn inc_ptr(&mut self, val: u16) {
        let [h, l] = self.ptr().wrapping_add(val).to_be_bytes().map(Wrapping);
        self.h = h;
        self.l = l;
    }

    fn dec_ptr(&mut self, val: u16) {
        let [h, l] = self.ptr().wrapping_sub(val).to_be_bytes().map(Wrapping);
        self.h = h;
        self.l = l;
    }

    /*
    /// A method similar to `Self::execute`, but is ran during start up, when the ROM that is
    /// burned-into the CPU is mapped over normal memory.
    ///
    /// Returns a the next operation should the start up process not be completed.
    pub fn start_up_execute(
        &mut self,
        instr: Instruction,
        mem: &mut impl MemoryLikeExt,
    ) -> Option<Instruction> {
        self.execute(instr, mem);
        (mem.read_byte(0xFF50) == 0).then(|| self.read_op(mem))
    }
    */
}

impl Index<HalfRegister> for Cpu {
    type Output = Wrapping<u8>;

    fn index(&self, index: HalfRegister) -> &Self::Output {
        match index {
            HalfRegister::A => &self.a,
            HalfRegister::B => &self.b,
            HalfRegister::C => &self.c,
            HalfRegister::D => &self.d,
            HalfRegister::E => &self.e,
            HalfRegister::H => &self.h,
            HalfRegister::L => &self.l,
            HalfRegister::F => todo!(),
        }
    }
}

impl IndexMut<HalfRegister> for Cpu {
    fn index_mut(&mut self, index: HalfRegister) -> &mut Self::Output {
        match index {
            HalfRegister::A => &mut self.a,
            HalfRegister::B => &mut self.b,
            HalfRegister::C => &mut self.c,
            HalfRegister::D => &mut self.d,
            HalfRegister::E => &mut self.e,
            HalfRegister::H => &mut self.h,
            HalfRegister::L => &mut self.l,
            HalfRegister::F => todo!(),
        }
    }
}
