use crate::mem::{
    vram::{PpuMode, VRam},
    MemoryMap,
};

/// The Pixel Processing Unit
#[derive(Debug, Hash)]
pub struct Ppu {
    pub h_count: u16, // Max of 456
    pub v_count: u8,  // Max of 153
}

impl Ppu {
    pub(crate) fn new() -> Self {
        Self {
            h_count: 0,
            v_count: 0,
        }
    }

    pub(crate) fn tick(&mut self, mem: &mut MemoryMap) {
        self.h_count += 1;
        match self.h_count {
            80 => mem.vram.inc_status(PpuMode::Drawing),
            // TODO: There is variability in this mode. Figure out how to emulate that.
            172 => mem.vram.inc_status(PpuMode::HBlank),
            456 => {
                self.h_count = 0;
                self.v_count += 1;
                match self.v_count {
                    144 => {
                        mem.vram.inc_status(PpuMode::VBlank);
                        mem.request_vblank_int();
                    }
                    153 => {
                        self.v_count = 0;
                        mem.vram.reset_status();
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }
}
