use spirit::{
    Gameboy,
    lookup::{HalfRegister, Instruction, LoadOp, RegOrPointer},
};

#[test]
fn bully() {
    let mut gb = Gameboy::new(include_bytes!("roms/bully/bully.gb")).complete();

    const REG_B: RegOrPointer = RegOrPointer::Reg(HalfRegister::B);
    const END_OP: Instruction = Instruction::Load(LoadOp::Basic {
        dest: REG_B,
        src: REG_B,
    });

    while !matches!(gb.read_op(), END_OP) {
        gb.step().complete();
    }
    const REGS: [(HalfRegister, u8); 6] = [
        (HalfRegister::B, 3),
        (HalfRegister::C, 5),
        (HalfRegister::D, 8),
        (HalfRegister::E, 13),
        (HalfRegister::H, 21),
        (HalfRegister::L, 34),
    ];
    let cpu = gb.cpu();
    for (reg, value) in REGS {
        // TODO: Can we extract the test from the display?
        assert_eq!(cpu[reg].0, value, "{cpu:?}");
    }
}
