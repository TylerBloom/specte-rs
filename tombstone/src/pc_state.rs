use std::collections::HashMap;

use ratatui::{
    Frame,
    layout::Rect,
    widgets::{Block, Paragraph},
};
use spirit::{Gameboy, lookup::Instruction};

use crate::{config::GameConfig, state::InnerAppState};

#[derive(Debug, Default)]
pub struct PcState {
    pub start: usize,
    ops: HashMap<u16, Instruction>,
}

impl PcState {
    pub fn new() -> Self {
        Self {
            start: 0,
            ops: HashMap::new(),
        }
    }

    pub fn render(&mut self, state: &InnerAppState, frame: &mut Frame, area: Rect) {
        let block = Block::bordered()
            .title(" PC Area ")
            .title_alignment(ratatui::layout::Alignment::Center);
        let len = block.inner(area).height as usize;
        let para = Paragraph::new(self.text_body(len, &state.config, &state.gb)).block(block);
        frame.render_widget(para, area);
    }

    fn text_body(&mut self, len: usize, config: &GameConfig, gb: &Gameboy) -> String {
        self.ops.extend(gb.op_iter().take(len));
        let mut prior_ops = Vec::with_capacity(self.start);
        let mut pc = gb.cpu().pc.0;
        loop {
            if prior_ops.len() == self.start {
                break;
            }
            let Some((addr, _)) = self.find_prior_op(pc) else {
                break;
            };
            prior_ops.push(addr);
            pc = addr;
        }
        prior_ops
            .into_iter()
            .rev()
            .chain(gb.op_iter().map(|(pc, _)| pc))
            // NOTE: This should never return None
            .filter_map(|pc| self.ops.get_key_value(&pc))
            .map(|(pc, op)| {
                let prefix = if config.is_breakpoint(*pc) {
                    "BP"
                } else {
                    "  "
                };
                let ptr = if *pc == gb.cpu().pc.0 { ">" } else { " " };
                format!("{prefix} {ptr} 0x{pc:0>4X} -> {op}\n")
            })
            .take(len)
            .collect()
    }

    fn find_prior_op(&self, addr: u16) -> Option<(u16, Instruction)> {
        let op: heapless::Vec<(u16, Instruction), 3> = (addr.saturating_sub(3)..addr)
            .filter_map(|addr| self.ops.get_key_value(&addr))
            .map(|(addr, op)| (*addr, *op))
            .collect();
        assert!(op.iter().map(|(_, op)| op.size()).sum::<u8>() <= 3);
        op.last().copied()
    }
}
