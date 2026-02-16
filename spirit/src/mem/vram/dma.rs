use serde::{Deserialize, Serialize};

use crate::{cpu::check_bit_const, instruction::Instruction, mem::vram::PpuMode};

#[derive(Default, Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) struct VramDma {
    src_hi: u8,
    src_lo: u8,
    dest_hi: u8,
    dest_lo: u8,
    trigger: u8,
    state: Option<TransferState>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
struct TransferState {
    src: u16,
    dest: u16,
    length: u8,
    kind: TransferKind,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Serialize, Deserialize)]
enum TransferKind {
    HBlank,
    Direct,
}

impl VramDma {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn read_byte(&self, index: u16) -> u8 {
        if index == 0xFF55 {
            println!("{:?}", self.state);
            self.state
                .as_ref()
                .map(|state| state.length.wrapping_sub(1))
                .unwrap_or(self.trigger)
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

    /// This affects the value in the trigger register and potentially starts or stops a transfer.
    ///
    /// A stop only occurs if there is an active HBlank transfer happening. If this is the case,
    /// the trigger register will contain the number of "blocks" left to transfer but with the top
    /// bit set.
    ///
    /// Otherwise, this triggers a new transfer to occur.
    pub fn trigger(&mut self, value: u8) {
        tracing::info!("Writing to VRAM DMA transfer");
        let kind = if check_bit_const::<7>(value) {
            TransferKind::HBlank
        } else {
            TransferKind::Direct
        };

        match &self.state {
            Some(state) if check_bit_const::<7>(value) => {
                tracing::info!("Cancelling HBlank VRAM DMA transfer");
                self.trigger = state.length.wrapping_sub(1);
                self.state = None;
            }
            _ => {
                let dest_hi = 0x80 | (0x1F & self.dest_hi);
                let state = TransferState {
                    src: u16::from_be_bytes([self.src_hi, 0xF0 & self.src_lo]),
                    dest: u16::from_be_bytes([dest_hi, 0xF0 & self.dest_lo]),
                    length: 0x7F & value, // Ignore to the top bit
                    kind,
                };
                self.state = Some(state);
            }
        }
    }

    /// Returns the next pair of addresses to transfer data from and into, respectively. These
    /// addresses represent an entire tile's worth data that needs to be transferred, not just a
    /// byte.
    pub fn next_addrs(&mut self) -> Option<(u16, u16)> {
        let state = self.state.as_mut()?;
        if state.length == 0 {
            self.state = None;
            self.trigger = 0xFF;
            return None;
        }
        let src = state.src;
        let dest = state.dest;
        state.src += 0x10;
        state.dest += 0x10;
        // The dest addr must always be in VRAM. If we are at A000, the takes us back to 8000
        state.dest &= 0x1FFF;
        state.dest |= 0x8000;
        state.length -= 1;
        Some((src, dest))
    }

    pub fn get_op(&self, ly: u8, mode: PpuMode) -> Option<Instruction> {
        match self.state.as_ref()?.kind {
            TransferKind::Direct => Some(Instruction::Transfer),
            TransferKind::HBlank => {
                (ly < 144 && matches!(mode, PpuMode::HBlank)).then_some(Instruction::Transfer)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{instruction::Instruction, mem::vram::PpuMode};

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

    /// Once triggered, the OAM produce a sequence of src/dest indices for reading and writing data
    /// from memory. This ensures that the sequence produces the correct indices in order.
    // TODO: Test that the bottom bits of the src and dest bytes are being ignored.
    // TODO: Test a range of src and dest addresses
    // TODO: Test both kind of transfer modes
    #[test]
    fn vram_dma_indexing() {
        let mut dma = VramDma::default();

        let [src_hi, src_lo] = 0x5000u16.to_be_bytes();
        dma.write_byte(0xFF51, src_lo);
        dma.write_byte(0xFF52, src_hi);

        let [dest_hi, dest_lo] = 0x8000u16.to_be_bytes();
        dma.write_byte(0xFF53, dest_lo);
        dma.write_byte(0xFF54, dest_hi);

        // Trigger
        let length = 0x10;
        dma.write_byte(0xFF55, length);

        // Check that DMA is primed (read FF55 and check that get_op returns an op)
        let trigger_reg = dma.read_byte(0xFF55);
        assert_eq!(
            trigger_reg, 0x0F,
            "Trigger register should be 0x0F but was 0x{trigger_reg:0>2X}"
        );
        let op = dma.get_op(0, PpuMode::HBlank);
        assert!(
            matches!(op, Some(Instruction::Transfer)),
            "DMA should return a transfer instruction but returned {op:?}"
        );

        let mut expected_length = length - 1;
        // Check indices
        for i in 0u16..(length as u16) {
            let expected_src = 0x5000 + i * 0x10;
            let expected_dest = 0x8000 | (0x1FFF & (0x8000 + i * 0x10));
            println!("Src: {expected_src:0>4X}, Dest: 0x{expected_dest:0>4X}");

            let (src, dest) = dma.next_addrs().unwrap();
            assert_eq!(
                src, expected_src,
                "Source address should be 0x{expected_src:0>4X} but was 0x{src:0>4X}"
            );
            assert_eq!(
                dest, expected_dest,
                "Destination address should be 0x{expected_dest:0>4X} but was 0x{dest:0>4X}"
            );

            expected_length = expected_length.wrapping_sub(1);
            let length = dma.read_byte(0xFF55);
            assert_eq!(
                length, expected_length,
                "The length of the remaining transfer should be {expected_length} but was {length}"
            );
        }

        // Check that the DMA is completed
        assert!(dma.next_addrs().is_none());
        let length = dma.read_byte(0xFF55);
        assert_eq!(
            length, 0xFF,
            "Length read after completed transfer should be 0xFF but was 0x{length:0>2X}"
        );
    }

    /// If a HBlank transfer is active, it can be stopped by unsetting the topmost bit.
    #[test]
    fn vram_dma_halting() {
        todo!()
    }

    /// In HBlank mode, transfers do not occur when the line's Y is more than 144. Similarly,
    /// transfers only occur during HBlank PPU mode
    #[test]
    fn hblank_transfer_pause() {
        todo!()
    }
}
