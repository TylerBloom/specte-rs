use spirit::Gameboy;

pub const ACID_WHICH: &[u8] = include_bytes!("roms/acid/which.gb");
pub const ACID_DMG_2: &[u8] = include_bytes!("roms/acid/dmg-acid2.gb");
pub const ACID_CGB_2: &[u8] = include_bytes!("roms/acid/cgb-acid2.gbc");
pub const ACID_CGM_HELL: &[u8] = include_bytes!("roms/acid/cgb-acid-hell.gbc");

pub const BLARG_INSTRS_01: &[u8] = include_bytes!("roms/blarg/cpu_instrs/01-special.gb");
pub const BLARG_INSTRS_02: &[u8] = include_bytes!("roms/blarg/cpu_instrs/02-interrupts.gb");
pub const BLARG_INSTRS_03: &[u8] = include_bytes!("roms/blarg/cpu_instrs/03-op_sp,hl.gb");
pub const BLARG_INSTRS_04: &[u8] = include_bytes!("roms/blarg/cpu_instrs/04-op_r,imm.gb");
pub const BLARG_INSTRS_05: &[u8] = include_bytes!("roms/blarg/cpu_instrs/05-op_rp.gb");
pub const BLARG_INSTRS_06: &[u8] = include_bytes!("roms/blarg/cpu_instrs/06-ld_r,r.gb");
pub const BLARG_INSTRS_07: &[u8] = include_bytes!("roms/blarg/cpu_instrs/07-jr,jp,call,ret,rst.gb");
pub const BLARG_INSTRS_08: &[u8] = include_bytes!("roms/blarg/cpu_instrs/08-misc_instrs.gb");
pub const BLARG_INSTRS_09: &[u8] = include_bytes!("roms/blarg/cpu_instrs/09-op_r,r.gb");
pub const BLARG_INSTRS_10: &[u8] = include_bytes!("roms/blarg/cpu_instrs/10-bit_ops.gb");
pub const BLARG_INSTRS_11: &[u8] = include_bytes!("roms/blarg/cpu_instrs/11-op_a,(hl).gb");

#[test]
fn load_acid_which() {
    let gb = Gameboy::new(ACID_WHICH);
}

#[test]
fn load_dmg_acid_2() {
    let gb = Gameboy::new(ACID_DMG_2);
}

#[test]
fn load_cgb_acid_2() {
    let gb = Gameboy::new(ACID_CGB_2);
}

#[test]
fn load_cgb_acid_hell() {
    let gb = Gameboy::new(ACID_CGM_HELL);
}

#[test]
fn load_blarg_instrs_01() {
    let gb = Gameboy::new(BLARG_INSTRS_01);
}

#[test]
fn load_blarg_instrs_02() {
    let gb = Gameboy::new(BLARG_INSTRS_02);
}

#[test]
fn load_blarg_instrs_03() {
    let gb = Gameboy::new(BLARG_INSTRS_03);
}

#[test]
fn load_blarg_instrs_04() {
    let gb = Gameboy::new(BLARG_INSTRS_04);
}

#[test]
fn load_blarg_instrs_05() {
    let gb = Gameboy::new(BLARG_INSTRS_05);
}

#[test]
fn load_blarg_instrs_06() {
    let gb = Gameboy::new(BLARG_INSTRS_06);
}

#[test]
fn load_blarg_instrs_07() {
    let gb = Gameboy::new(BLARG_INSTRS_07);
}

#[test]
fn load_blarg_instrs_08() {
    let gb = Gameboy::new(BLARG_INSTRS_08);
}

#[test]
fn load_blarg_instrs_09() {
    let gb = Gameboy::new(BLARG_INSTRS_09);
}

#[test]
fn load_blarg_instrs_10() {
    let gb = Gameboy::new(BLARG_INSTRS_10);
}

#[test]
fn load_blarg_instrs_11() {
    let gb = Gameboy::new(BLARG_INSTRS_11);
}
