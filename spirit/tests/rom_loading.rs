use spirit::Gameboy;

macro_rules! include_roms {
    ($($file:literal),+) => {{
        &[$(($file, include_bytes!($file))),+]
    }};
}

pub const TEST_ROMS: &[(&str, &[u8])] = include_roms!(
    "roms/acid/which.gb",
    "roms/acid/dmg-acid2.gb",
    "roms/acid/cgb-acid2.gbc",
    "roms/acid/cgb-acid-hell.gbc",
    "roms/blarg/cpu_instrs/01-special.gb",
    "roms/blarg/cpu_instrs/02-interrupts.gb",
    "roms/blarg/cpu_instrs/03-op_sp,hl.gb",
    "roms/blarg/cpu_instrs/04-op_r,imm.gb",
    "roms/blarg/cpu_instrs/05-op_rp.gb",
    "roms/blarg/cpu_instrs/06-ld_r,r.gb",
    "roms/blarg/cpu_instrs/07-jr,jp,call,ret,rst.gb",
    "roms/blarg/cpu_instrs/08-misc_instrs.gb",
    "roms/blarg/cpu_instrs/09-op_r,r.gb",
    "roms/blarg/cpu_instrs/10-bit_ops.gb",
    "roms/blarg/cpu_instrs/11-op_a,(hl).gb",
    "roms/blarg/halt_bug.gb",
    "roms/blarg/instr_timing.gb",
    "roms/blarg/interrupt_time.gb",
    "roms/blarg/mem_timing/read_timing_1.gb",
    "roms/blarg/mem_timing/read_timing_2.gb",
    "roms/blarg/mem_timing/write_timing_1.gb",
    "roms/blarg/mem_timing/write_timing_2.gb",
    "roms/blarg/mem_timing/modify_timing_1.gb",
    "roms/blarg/mem_timing/modify_timing_2.gb"
);

#[test]
fn run_test_roms() {
    for (name, cart) in TEST_ROMS {
        println!("Running ROM from file '{name}'");
        let mut gb = Gameboy::new(*cart);
        gb.start_up().complete();
    }
}
