use serde::{Deserialize, Serialize};

use crate::{cpu::check_bit_const, instruction::Instruction, mem::vram::PpuMode};

#[derive(Default, Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct VramDma {
    src_hi: u8,
    src_lo: u8,
    curr_src: u16,
    dest_hi: u8,
    dest_lo: u8,
    curr_dest: u16,
    trigger: u8,
    /// The amount of data left to transfer.
    len_left: u8,
    ly: u8,
}

impl VramDma {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn read_byte(&self, index: u16) -> u8 {
        if index == 0xFF55 {
            self.trigger
        } else {
            0xFF
        }
    }

    pub fn write_byte(&mut self, index: u16, value: u8) {
        match index {
            0xFF51 => self.src_lo = value,
            0xFF52 => self.src_hi = value,
            0xFF53 => self.dest_lo = value,
            0xFF54 => self.dest_hi = value,
            0xFF55 => self.trigger(value),
            index => unreachable!("Called VramDma::write_byte with addr 0x{index:0>4X}"),
        }
    }

    pub fn read_trigger(&self) -> u8 {
        self.len_left.wrapping_sub(1)
    }

    pub fn trigger(&mut self, value: u8) {
        tracing::info!("Writing to VRAM DMA transfer");
        if !check_bit_const::<7>(value) && self.len_left > 0 {
            tracing::info!("Cancelling VRAM DMA transfer");
            self.len_left |= 1 << 7;
        } else {
            tracing::info!("Cancelling VRAM DMA transfer");
            self.trigger = value;
            self.len_left = value & 0x7F;
            self.curr_src = 0xFFF0 & u16::from_be_bytes([self.src_hi, self.src_hi]);
            self.curr_dest = 0x8000 + (0x1FF0 & u16::from_be_bytes([self.dest_hi, self.dest_lo]));
        }
    }

    /// Returns the next pair of addresses to transfer data from and into, respectively. These
    /// addresses represent an entire tile's worth data that needs to be transferred, not just a
    /// byte.
    pub fn next_addrs(&mut self) -> Option<(u16, u16)> {
        if self.len_left > 0 {
            self.len_left -= 1;
            let src = self.curr_src;
            let dest = self.curr_dest;
            self.curr_src += 16;
            self.curr_dest += 16;
            Some((src, dest))
        } else {
            None
        }
    }

    pub fn get_op(&self, ly: u8, state: PpuMode) -> Option<Instruction> {
        if check_bit_const::<7>(self.read_trigger()) {
            return None;
        }
        if check_bit_const::<7>(self.trigger) {
            if self.ly == ly && matches!(state, PpuMode::HBlank) {
                Some(Instruction::Transfer)
            } else {
                None
            }
        } else {
            Some(Instruction::Transfer)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::VramDma;

    /// The source and destination registers (FF51-FF54) are all write-only. Reads should return
    /// 0xFF
    #[test]
    fn src_dest_write() {
        let mut dma = VramDma::default();
        assert_eq!(dma.read_byte(0xFF51), 0xFF);
        assert_eq!(dma.read_byte(0xFF52), 0xFF);
        assert_eq!(dma.read_byte(0xFF53), 0xFF);
        assert_eq!(dma.read_byte(0xFF54), 0xFF);
        assert_eq!(dma.read_byte(0xFF55), 0);

        dma.write_byte(0xFF51, 0x80);
        assert_eq!(dma.read_byte(0xFF51), 0xFF);
        dma.write_byte(0xFF52, 0x80);
        assert_eq!(dma.read_byte(0xFF52), 0xFF);
        dma.write_byte(0xFF53, 0x80);
        assert_eq!(dma.read_byte(0xFF53), 0xFF);
        dma.write_byte(0xFF54, 0x80);
        assert_eq!(dma.read_byte(0xFF54), 0xFF);
        dma.write_byte(0xFF55, 0x80);
        assert_eq!(dma.read_byte(0xFF55), 0x80);
    }

    /// Once a VRAM DMA transfer is completed, the value read from the "trigger" register (FF55)
    /// should be 0xFF. This is true for both the direct and HBlank transfers.
    #[test]
    fn completed_vram_dma_transfer() {
        todo!()
    }

    /// If a HBlank transfer is active, it can be stopped by unsetting the topmost bit.
    #[test]
    fn vram_dma_halting() {
        todo!()
    }

    /// Once triggered, the OAM produce a sequence of src/dest indices for reading and writing data
    /// from memory. This ensures that the sequence produces the correct indices in order.
    #[test]
    fn vram_dma_indexing() {
        let mut dma = VramDma::default();
        todo!()
    }
}
