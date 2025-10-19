/* FIXME: These tests take minutes to run. They either need to be optimized or deleted.
use spirit::Gameboy;
use spirit::lookup::HalfRegister;
use spirit::lookup::Instruction;
use spirit::lookup::LoadOp;
use spirit::lookup::RegOrPointer;

const REG_B: RegOrPointer = RegOrPointer::Reg(HalfRegister::B);
const END_OP: Instruction = Instruction::Load(LoadOp::Basic {
    dest: REG_B,
    src: REG_B,
});

fn run(cart: &[u8]) {
    let mut gb = Gameboy::new(cart.into()).complete();
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
        assert_eq!(cpu[reg].0, value, "{cpu:?}");
    }
}

#[test]
fn vram_read() {
    run(include_bytes!("roms/age/vram/vram-read-cgbBCE.gb"));
}

#[test]
fn ei_halt() {
    run(include_bytes!("roms/age/halt/ei-halt-dmgC-cgbBCE.gb"));
}
*/
