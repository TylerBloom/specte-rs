use std::num::Wrapping;

use spirit::{cpu::Cpu, lookup::{BitShiftOp, HalfRegister, Instruction, RegOrPointer}, mbc::MemoryMap};


#[test]
fn test_bit_shift_ops() {
    const A: HalfRegister = HalfRegister::A;
    const B: HalfRegister = HalfRegister::B;
    let mut cpu = Cpu::default();
    let mut mem = MemoryMap::construct();
    let mem = &mut mem;
    // Testing RL* ops
    for (i, b) in [false, true]
        .into_iter()
        .flat_map(|b| (0u8..=u8::MAX).map(move |i| (i, b)))
    {
        // RLA section
        cpu.flags_mut().c = b;
        cpu[A] = Wrapping(i);
        let instr = Instruction::BitShift(BitShiftOp::Rla);
        cpu.execute(instr, mem);
        let a = (i << 1) | b as u8;
        let carry = (i & 0x80) == 0x80;
        assert_eq!(cpu[A].0, a);
        assert_eq!(cpu.flags_mut().c, carry);
        // RL section
        cpu[B] = Wrapping(i);
        cpu.flags_mut().c = b;
        let instr = Instruction::BitShift(BitShiftOp::Rl(RegOrPointer::Reg(HalfRegister::B)));
        cpu.execute(instr, mem);
        assert_eq!(cpu[B].0, a);
        assert_eq!(cpu.flags().c, carry);
        // RLCA section
        cpu[A] = Wrapping(i);
        cpu.flags_mut().c = b;
        let instr = Instruction::BitShift(BitShiftOp::Rlca);
        cpu.execute(instr, mem);
        let a = i.rotate_left(1);
        let carry = (i & 0x80) == 0x80;
        assert_eq!(cpu[A].0, a);
        assert_eq!(cpu.flags().c, carry);
        // RLC section
        cpu[B] = Wrapping(i);
        cpu.flags_mut().c = b;
        let instr = Instruction::BitShift(BitShiftOp::Rlc(RegOrPointer::Reg(HalfRegister::B)));
        cpu.execute(instr, mem);
        assert_eq!(cpu[B].0, a);
        assert_eq!(cpu.flags().c, carry);
    }
    // Testing RR* ops
    for (i, b) in [false, true]
        .into_iter()
        .flat_map(|b| (0u8..=u8::MAX).map(move |i| (i, b)))
    {
        // Rra section
        cpu.flags_mut().c = b;
        cpu[A] = Wrapping(i);
        let instr = Instruction::BitShift(BitShiftOp::Rra);
        cpu.execute(instr, mem);
        let a = (i >> 1) | (0x80 * (b as u8));
        let carry = (i & 0x01) == 0x01;
        assert_eq!(cpu[A].0, a);
        assert_eq!(cpu.flags().c, carry);
        // Rr section
        cpu[B] = Wrapping(i);
        cpu.flags_mut().c = b;
        let instr = Instruction::BitShift(BitShiftOp::Rr(RegOrPointer::Reg(HalfRegister::B)));
        cpu.execute(instr, mem);
        assert_eq!(cpu[B].0, a);
        assert_eq!(cpu.flags().c, carry);
        // RRCA section
        cpu[A] = Wrapping(i);
        cpu.flags_mut().c = b;
        let instr = Instruction::BitShift(BitShiftOp::Rrca);
        cpu.execute(instr, mem);
        let a = i.rotate_right(1);
        let carry = (i & 0x01) == 1;
        assert_eq!(cpu[A].0, a);
        assert_eq!(cpu.flags().c, carry);
        // RRC section
        cpu[B] = Wrapping(i);
        cpu.flags_mut().c = b;
        let instr = Instruction::BitShift(BitShiftOp::Rrc(RegOrPointer::Reg(HalfRegister::B)));
        cpu.execute(instr, mem);
        assert_eq!(cpu[B].0, a);
        assert_eq!(cpu.flags().c, carry);
    }
    // Testing Shift ops
    for i in 0u8..=u8::MAX {
        // SLA section
        cpu[A] = Wrapping(i);
        let instr = Instruction::BitShift(BitShiftOp::Sla(RegOrPointer::Reg(HalfRegister::A)));
        cpu.execute(instr, mem);
        let a = i << 1;
        let carry = (i & 0x80) == 0x80;
        assert_eq!(cpu[A].0, a);
        assert_eq!(cpu.flags().c, carry);
        // SRA section
        cpu[A] = Wrapping(i);
        let instr = Instruction::BitShift(BitShiftOp::Sra(RegOrPointer::Reg(HalfRegister::A)));
        cpu.execute(instr, mem);
        let a = (i & 0x80) | (i >> 1);
        let carry = (i & 0x01) == 0x01;
        assert_eq!(cpu[A].0, a);
        assert_eq!(cpu.flags().c, carry);
        // SRL section
        cpu[A] = Wrapping(i);
        let instr = Instruction::BitShift(BitShiftOp::Srl(RegOrPointer::Reg(HalfRegister::A)));
        cpu.execute(instr, mem);
        let a = i >> 1;
        let carry = (i & 0x01) == 0x01;
        assert_eq!(cpu[A].0, a);
        assert_eq!(cpu.flags().c, carry);
    }
    // Testing SWAP op
    for i in 0u8..=u8::MAX {
        cpu[A] = Wrapping(i);
        let instr = Instruction::BitShift(BitShiftOp::Swap(RegOrPointer::Reg(HalfRegister::A)));
        cpu.execute(instr, mem);
        let a = ((i & 0x0F) << 4) | ((i & 0xF0) >> 4);
        assert_eq!(cpu[A].0, a);
        assert!(!cpu.flags().c);
    }
}
