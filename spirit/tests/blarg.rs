macro_rules! blarg_test {
    ($file:literal) => {
        let rom = include_bytes!(concat!("roms/blarg/cpu_instrs/", $file, ".gb"));
        let mut gb = spirit::Gameboy::new(rom).complete();
        (0..10).for_each(|_| gb.next_frame().complete());
        let expected_mem = postcard::from_bytes(include_bytes!(concat!(
            "data/blarg-cpu-instr-",
            $file,
            "-memory-map.postcard"
        )))
        .unwrap();
        assert_eq!(gb.mem, expected_mem);
        let expected_screen: Vec<Vec<_>> =
            postcard::from_bytes(include_bytes!(concat!("data/blarg-cpu-instr-", $file, "-screen.postcard")))
                .unwrap();
        assert_eq!(gb.ppu.screen, expected_screen);
    };
}

#[test]
#[should_panic]
fn blarg_special_instr_test() {
    println!("{}", env!("CARGO_MANIFEST_DIR"));
    blarg_test!("01-special");
}

#[test]
#[should_panic]
fn blarg_interrupt_instr_test() {
    todo!()
        // blarg_test!("02-interrupts");
}

#[test]
#[should_panic]
fn blarg_sp_hl_instr_test() {
    todo!() // blarg_test!("03-op_sp,hl");
}

#[test]
#[should_panic]
fn blarg_r_imm_instr_test() {
    todo!() // blarg_test!("04-op_r,imm");
}

#[test]
#[should_panic]
fn blarg_rp_instr_test() {
    todo!() // blarg_test!("05-op_rp");
}

#[test]
#[should_panic]
fn blarg_ld_r_r_instr_test() {
    todo!() // blarg_test!("06-ld_r,r");
}

#[test]
#[should_panic]
fn blarg_jr_jp_call_ret_rst_instr_test() {
    todo!() // blarg_test!("07-jr,jp,call,ret,rst");
}

#[test]
#[should_panic]
fn blarg_misc_instr_test() {
    todo!() // blarg_test!("08-misc_instrs");
}

#[test]
#[should_panic]
fn blarg_r_r_instr_test() {
    todo!() // blarg_test!("09-op_r,r");
}

#[test]
#[should_panic]
fn blarg_bit_ops_instr_test() {
    todo!() // blarg_test!("10-bit_ops");
}

#[test]
#[should_panic]
fn blarg_a_hl_instr_test() {
    todo!()
        // blarg_test!("11-op_a,(hl)");
}
