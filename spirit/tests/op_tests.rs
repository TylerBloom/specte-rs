use spirit::cpu::Cpu;
use spirit::instruction::BitShiftOp;
use spirit::instruction::HalfRegister;
use spirit::instruction::Instruction;
use spirit::instruction::LoadOp;
use spirit::instruction::RegOrPointer;
use spirit::mem::MemoryMap;
use spirit::utils::Wrapping;

const WRAM: u16 = 0xC000;

fn init() -> (Cpu, MemoryMap) {
    static CART: &[u8] = include_bytes!("roms/acid/which.gb");
    let cpu = Cpu::default();
    let mem = MemoryMap::new(CART.into());
    (cpu, mem)
}

fn reg_iter() -> &'static [RegOrPointer] {
    static REGS: &[RegOrPointer] = &[
        RegOrPointer::Reg(HalfRegister::A),
        RegOrPointer::Reg(HalfRegister::B),
        RegOrPointer::Reg(HalfRegister::C),
        RegOrPointer::Reg(HalfRegister::D),
        RegOrPointer::Reg(HalfRegister::E),
        RegOrPointer::Reg(HalfRegister::H),
        RegOrPointer::Reg(HalfRegister::L),
        RegOrPointer::Pointer,
    ];
    REGS
}

#[test]
fn test_cpl() {
    let (mut cpu, mut mem) = init();
    assert_eq!(cpu.pc.0, 0);
    cpu.execute(Instruction::Cpl, &mut mem);
    assert_eq!(cpu.pc.0, 1);
    assert_eq!(cpu.a.0, u8::MAX);
    assert!(cpu.subtraction_flag(), "{cpu:#X?}");
    assert!(cpu.half_carry_flag(), "{cpu:#X?}");
    cpu.a = Wrapping(1);
    cpu.execute(Instruction::Cpl, &mut mem);
    assert_eq!(cpu.a.0, u8::MAX << 1);
    assert_eq!(cpu.pc.0, 2);
}

/*
pub enum LoadOp {
    /// Used for opcodes 0xX1
    Direct16(WideReg, u16),
    /// Used for opcodes 0x_A
    LoadIntoA(LoadAPointer),
    /// Used for opcodes 0x_2
    StoreFromA(LoadAPointer),
    /// Opcode: 0x08
    /// Store SP & $FF at address n16 and SP >> 8 at address n16 + 1.
    StoreSP(u16),
    /// Opcode: 0xF9
    HLIntoSP,
    /// Opcode: 0xF8
    /// Add the signed value e8 to SP and store the result in HL.
    SPIntoHL(i8),
    /// Used for opcodes 0x_1
    Pop(WideRegWithoutSP),
    /// Used for opcodes 0x_5
    Push(WideRegWithoutSP),
    /// Used for opcode 0xE0
    LoadHigh(u8),
    /// Used for opcode 0xF0
    StoreHigh(u8),
    /// Used for opcode 0xE2
    LoadHighCarry,
    /// Used for opcode 0xF2
    StoreHighCarry,
    /// Used for opcode 0xEA
    LoadA { ptr: u16 },
    /// Used for opcode 0xFA
    StoreA { ptr: u16 },
}
*/
#[test]
fn test_load_ops() {
    test_basic_load_op();
    test_direct_load_op();
}

fn test_direct_load_op() {
    let (mut cpu, mut mem) = init();
    let mem = &mut mem;
    for reg in reg_iter().iter().copied() {
        if reg == RegOrPointer::Pointer {
            let [h, l] = WRAM.to_be_bytes().map(Wrapping);
            cpu.h = h;
            cpu.l = l;
        }
        let op = Instruction::Load(LoadOp::Direct(reg, 0xAB));
        cpu.execute(op, mem);
        assert_eq!(cpu.copy_byte(mem, reg), 0xAB);
    }
}

fn test_basic_load_op() {
    let (mut cpu, mut mem) = init();
    let iter = reg_iter().iter().copied();
    for (dest, src) in iter
        .clone()
        .flat_map(|r| iter.clone().map(move |rr| (r, rr)))
    {
        let op = Instruction::Load(LoadOp::Basic { dest, src });
        cpu.execute(op, &mut mem);
    }
}

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
