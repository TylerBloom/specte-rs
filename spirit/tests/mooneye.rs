use spirit::Gameboy;
use spirit::lookup::HalfRegister;
use spirit::lookup::Instruction;
use spirit::lookup::LoadOp;
use spirit::lookup::RegOrPointer;
use spirit::utils::Wrapping;

const STOP_OP: Instruction = Instruction::Load(LoadOp::Basic {
    dest: RegOrPointer::Reg(HalfRegister::B),
    src: RegOrPointer::Reg(HalfRegister::B),
});

const GOAL_B_REG: Wrapping<u8> = Wrapping(3u8);
const GOAL_C_REG: Wrapping<u8> = Wrapping(5u8);
const GOAL_D_REG: Wrapping<u8> = Wrapping(8u8);
const GOAL_E_REG: Wrapping<u8> = Wrapping(13u8);
const GOAL_H_REG: Wrapping<u8> = Wrapping(21u8);
const GOAL_L_REG: Wrapping<u8> = Wrapping(34u8);

macro_rules! mooneye_test {
    ($path: literal) => {{
        let mut gb =
            Gameboy::new(include_bytes!(concat!("roms/mooneye/acceptance/", $path)).into())
                .complete();
        while {
            assert!(!gb.is_stopped());
            !matches!(gb.read_op(), STOP_OP)
        } {
            gb.step();
        }
        let cpu = gb.cpu();
        assert_eq!(cpu.b, GOAL_B_REG);
        assert_eq!(cpu.c, GOAL_C_REG);
        assert_eq!(cpu.d, GOAL_D_REG);
        assert_eq!(cpu.e, GOAL_E_REG);
        assert_eq!(cpu.h, GOAL_H_REG);
        assert_eq!(cpu.l, GOAL_L_REG);
    }};
}

#[test]
fn timer_div_write() {
    mooneye_test!("timer/div_write.gb")
}

#[test]
fn timer_tim_00() {
    mooneye_test!("timer/tim00.gb")
}

#[test]
fn timer_tim_00_div_trigger() {
    mooneye_test!("timer/tim00_div_trigger.gb")
}

#[test]
fn timer_tim01() {
    mooneye_test!("timer/tim01.gb")
}

#[test]
fn timer_tim01_div_trigger() {
    mooneye_test!("timer/tim01_div_trigger.gb")
}

#[test_log::test]
fn timer_tim10_() {
    mooneye_test!("timer/tim10.gb")
}

#[test]
fn timer_tim10_div_trigger() {
    mooneye_test!("timer/tim10_div_trigger.gb")
}

#[test]
fn timer_tim11() {
    mooneye_test!("timer/tim11.gb")
}

#[test]
fn timer_tim11_div_trigger() {
    mooneye_test!("timer/tim11_div_trigger.gb")
}

#[test]
fn timer_tima_reload() {
    mooneye_test!("timer/tima_reload.gb")
}

// This test and the similarly named TIMA test are expected to panic currently. These tests require
// sub-intruction levels of percision. While this is a long-term goal of the project, it is far
// from necessary. Once the project has reached on MVP, this is be revisited.
#[test]
#[should_panic]
fn timer_tma_write_reloading() {
    mooneye_test!("timer/tma_write_reloading.gb")
}

#[test]
#[should_panic]
fn timer_tima_write_reloading() {
    mooneye_test!("timer/tima_write_reloading.gb")
}

// Similar to the prior two tests, (I think) this test requires to sub-instruction precision.
#[test]
#[should_panic]
fn timer_rapid_toggle() {
    mooneye_test!("timer/rapid_toggle.gb")
}

#[test]
fn bits_reg_f() {
    mooneye_test!("bits/reg_f.gb")
}

#[test]
fn bits_mem_oam() {
    mooneye_test!("bits/mem_oam.gb")
}

#[test]
fn instr_daa() {
    mooneye_test!("instr/daa.gb")
}

#[test]
#[should_panic]
fn oam_dma_basic() {
    mooneye_test!("oam_dma/basic.gb")
}

#[test]
#[should_panic]
fn oam_dma_reg_read() {
    mooneye_test!("oam_dma/reg_read.gb")
}

#[test]
#[should_panic]
fn interrups_ie_push() {
    mooneye_test!("interrupts/ie_push.gb")
}

/* FIXME: These take more than a minute to run...
#[test]
fn ppu_intr_2_0_timing() {
    mooneye_test!("ppu/intr_2_0_timing.gb")
}

#[test]
fn ppu_intr_2_mode0_timing_a() {
    mooneye_test!("ppu/intr_2_mode0_timing.gb")
}

#[test]
fn ppu_intr_2_mode0_timing_sprites() {
    mooneye_test!("ppu/intr_2_mode0_timing_sprites.gb")
}

#[test]
fn ppu_intr_2_mode3_timing() {
    mooneye_test!("ppu/intr_2_mode3_timing.gb")
}

#[test]
fn ppu_intr_2_oam_ok_timing() {
    mooneye_test!("ppu/intr_2_oam_ok_timing.gb")
}
*/

#[test]
#[should_panic]
fn ppu_stat_irq_blocking() {
    mooneye_test!("ppu/stat_irq_blocking.gb")
}

#[test]
#[should_panic]
fn ppu_stat_lyc_onoff() {
    mooneye_test!("ppu/stat_lyc_onoff.gb")
}

#[test]
fn div_timing() {
    mooneye_test!("div_timing.gb")
}

#[test]
#[should_panic]
fn add_sp_e_timing() {
    mooneye_test!("add_sp_e_timing.gb")
}

#[test]
#[should_panic]
fn call_cc_timing() {
    mooneye_test!("call_cc_timing.gb")
}

#[test]
#[should_panic]
fn call_cc_timing2() {
    mooneye_test!("call_cc_timing2.gb")
}

#[test]
#[should_panic]
fn call_timing() {
    mooneye_test!("call_timing.gb")
}

#[test]
#[should_panic]
fn call_timing2() {
    mooneye_test!("call_timing2.gb")
}

#[test]
#[should_panic]
fn ei_sequence() {
    mooneye_test!("ei_sequence.gb")
}

#[test]
fn ei_timing() {
    mooneye_test!("ei_timing.gb")
}

#[test]
fn halt_ime0_ei() {
    mooneye_test!("halt_ime0_ei.gb")
}

/* FIXME: Takes more than 60 seconds...
#[test]
fn halt_ime0_nointr_timing() {
    mooneye_test!("halt_ime0_nointr_timing.gb")
}
*/

#[test]
fn halt_ime1_timing() {
    mooneye_test!("halt_ime1_timing.gb")
}

#[test]
#[should_panic]
fn if_ie_registers() {
    mooneye_test!("if_ie_registers.gb")
}

#[test]
#[should_panic]
fn intr_timing() {
    mooneye_test!("intr_timing.gb")
}

#[test]
#[should_panic]
fn jp_cc_timing() {
    mooneye_test!("jp_cc_timing.gb")
}

#[test]
#[should_panic]
fn jp_timing() {
    mooneye_test!("jp_timing.gb")
}

#[test]
#[should_panic]
fn ld_hl_sp_e_timing() {
    mooneye_test!("ld_hl_sp_e_timing.gb")
}

#[test]
#[should_panic]
fn oam_dma_restart() {
    mooneye_test!("oam_dma_restart.gb")
}

#[test]
#[should_panic]
fn oam_dma_start() {
    mooneye_test!("oam_dma_start.gb")
}

#[test]
#[should_panic]
fn oam_dma_timing() {
    mooneye_test!("oam_dma_timing.gb")
}

#[test]
#[should_panic]
fn pop_timing() {
    mooneye_test!("pop_timing.gb")
}

#[test]
#[should_panic]
fn push_timing() {
    mooneye_test!("push_timing.gb")
}

#[test]
#[should_panic]
fn rapid_di_ei() {
    mooneye_test!("rapid_di_ei.gb")
}

#[test]
#[should_panic]
fn ret_cc_timing() {
    mooneye_test!("ret_cc_timing.gb")
}

#[test]
#[should_panic]
fn ret_timing() {
    mooneye_test!("ret_timing.gb")
}

#[test]
fn reti_intr_timing() {
    mooneye_test!("reti_intr_timing.gb")
}

#[test]
#[should_panic]
fn reti_timing() {
    mooneye_test!("reti_timing.gb")
}

#[test]
#[should_panic]
fn rst_timing() {
    mooneye_test!("rst_timing.gb")
}

#[test]
fn mbc1_bits_bank1() {
    mooneye_test!("../emulator-only/mbc1/bits_bank1.gb")
}

#[test]
fn mbc1_bits_bank2() {
    mooneye_test!("../emulator-only/mbc1/bits_bank2.gb")
}

#[test]
#[should_panic]
fn mbc1_bits_mode() {
    mooneye_test!("../emulator-only/mbc1/bits_mode.gb")
}

#[test]
#[should_panic]
fn mbc1_bits_ramg() {
    mooneye_test!("../emulator-only/mbc1/bits_ramg.gb")
}

#[test]
#[should_panic]
fn mbc1_multicart_rom_8mb() {
    mooneye_test!("../emulator-only/mbc1/multicart_rom_8Mb.gb")
}

#[test]
#[should_panic]
fn mbc1_ram_64kb() {
    mooneye_test!("../emulator-only/mbc1/ram_64kb.gb")
}

#[test]
#[should_panic]
fn mbc1_ram_256kb() {
    mooneye_test!("../emulator-only/mbc1/ram_256kb.gb")
}

#[test]
#[should_panic]
fn mbc1_rom_1mb() {
    mooneye_test!("../emulator-only/mbc1/rom_1Mb.gb")
}

#[test]
#[should_panic]
fn mbc1_rom_2mb() {
    mooneye_test!("../emulator-only/mbc1/rom_2Mb.gb")
}

#[test]
fn mbc1_rom_4mb() {
    mooneye_test!("../emulator-only/mbc1/rom_4Mb.gb")
}

#[test]
#[should_panic]
fn mbc1_rom_8mb() {
    mooneye_test!("../emulator-only/mbc1/rom_8Mb.gb")
}

#[test]
#[should_panic]
fn mbc1_rom_16mb() {
    mooneye_test!("../emulator-only/mbc1/rom_16Mb.gb")
}

#[test]
#[should_panic]
fn mbc1_rom_512kb() {
    mooneye_test!("../emulator-only/mbc1/rom_512kb.gb")
}

#[test]
#[should_panic]
fn mbc2_bits_ramg() {
    mooneye_test!("../emulator-only/mbc2/bits_ramg.gb")
}

#[test]
#[should_panic]
fn mbc2_bits_romb() {
    mooneye_test!("../emulator-only/mbc2/bits_romb.gb")
}

#[test]
#[should_panic]
fn mbc2_bits_unused() {
    mooneye_test!("../emulator-only/mbc2/bits_unused.gb")
}

#[test]
#[should_panic]
fn mbc2_ram() {
    mooneye_test!("../emulator-only/mbc2/ram.gb")
}

#[test]
#[should_panic]
fn mbc2_rom_1mb() {
    mooneye_test!("../emulator-only/mbc2/rom_1Mb.gb")
}

#[test]
#[should_panic]
fn mbc2_rom_2mb() {
    mooneye_test!("../emulator-only/mbc2/rom_2Mb.gb")
}

#[test]
#[should_panic]
fn mbc2_rom_512kb() {
    mooneye_test!("../emulator-only/mbc2/rom_512kb.gb")
}

#[test]
#[should_panic]
fn mbc5_rom_1mb() {
    mooneye_test!("../emulator-only/mbc5/rom_1Mb.gb")
}

#[test]
#[should_panic]
fn mbc5_rom_2mb() {
    mooneye_test!("../emulator-only/mbc5/rom_2Mb.gb")
}

#[test]
#[should_panic]
fn mbc5_rom_4mb() {
    mooneye_test!("../emulator-only/mbc5/rom_4Mb.gb")
}

#[test]
#[should_panic]
fn mbc5_rom_8mb() {
    mooneye_test!("../emulator-only/mbc5/rom_8Mb.gb")
}

#[test]
#[should_panic]
fn mbc5_rom_16mb() {
    mooneye_test!("../emulator-only/mbc5/rom_16Mb.gb")
}

#[test]
#[should_panic]
fn mbc5_rom_32mb() {
    mooneye_test!("../emulator-only/mbc5/rom_32Mb.gb")
}

#[test]
#[should_panic]
fn mbc5_rom_64mb() {
    mooneye_test!("../emulator-only/mbc5/rom_64Mb.gb")
}

#[test]
#[should_panic]
fn mbc5_rom_512kb() {
    mooneye_test!("../emulator-only/mbc5/rom_512kb.gb")
}
