/* FIXME: Runs forever without reaching the end op. When running manually, this is because "the
 * initial DIV" is incorrect.
use spirit::Gameboy;
use spirit::lookup::HalfRegister;
use spirit::lookup::Instruction;
use spirit::lookup::LoadOp;
use spirit::lookup::RegOrPointer;

#[test]
fn bully() {
    let mut gb = Gameboy::new(include_bytes!("roms/bully/bully.gb").into()).complete();

    const REG_B: RegOrPointer = RegOrPointer::Reg(HalfRegister::B);
    const END_OP: Instruction = Instruction::Load(LoadOp::Basic {
        dest: REG_B,
        src: REG_B,
    });

    while !matches!(gb.read_op(), END_OP) {
        gb.step();
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
*/
