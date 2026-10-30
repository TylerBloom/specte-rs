macro_rules! blarg_test {
    ($file:literal, $frames:literal) => {
        let rom = include_bytes!(concat!("roms/blarg/cpu_instrs/", $file, ".gb"));
        let mut gb = spirit::Gameboy::load_cartridge(rom.into()).complete();
        (0..$frames).for_each(|_| gb.next_frame());
        let screen: Vec<u8> = gb
            .ppu
            .screen
            .iter()
            .flatten()
            .flat_map(|p| [p.r, p.g, p.b])
            .collect();
        insta::assert_binary_snapshot!(concat!("blarg-cpu-instrs-", $file, "-screen.bin"), screen);
    };
}

#[test]
fn blarg_special_instr_test() {
    blarg_test!("01-special", 150);
}

#[test]
fn blarg_interrupt_instr_test() {
    blarg_test!("02-interrupts", 40);
}

#[test]
fn blarg_sp_hl_instr_test() {
    blarg_test!("03-op_sp,hl", 100);
}

#[test]
fn blarg_r_imm_instr_test() {
    blarg_test!("04-op_r,imm", 110);
}

#[test]
fn blarg_rp_instr_test() {
    blarg_test!("05-op_rp", 150);
}

#[test]
fn blarg_ld_r_r_instr_test() {
    blarg_test!("06-ld_r,r", 50);
}

#[test]
fn blarg_jr_jp_call_ret_rst_instr_test() {
    blarg_test!("07-jr,jp,call,ret,rst", 50);
}

#[test]
fn blarg_misc_instr_test() {
    blarg_test!("08-misc_instrs", 40);
}

#[test]
fn blarg_r_r_instr_test() {
    blarg_test!("09-op_r,r", 300);
}

#[test]
fn blarg_bit_ops_instr_test() {
    blarg_test!("10-bit_ops", 430);
}

#[test]
fn blarg_a_hl_instr_test() {
    blarg_test!("11-op_a,(hl)", 570);
}
