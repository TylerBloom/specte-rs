#![allow(dead_code, unused)]
use std::fmt::Write;
use std::{error::Error, io::Write as _};

use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout, Rect},
    text::Text,
    widgets::{Block, Paragraph},
    Frame, Terminal,
};
use spirit::{cpu::Cpu, lookup::Instruction, mem::MemoryMap, ppu::Ppu, Gameboy, StartUpSequence};

mod command;
mod state;
use command::*;
use state::*;

// TODO: handle ctrl-C and ctrl-d

static TMP_ROM: &[u8] = include_bytes!("../../spirit/tests/roms/acid/which.gb");

fn main() -> Result<(), Box<dyn Error>> {
    let mut state = AppState::default();
    state.mem_start = 0x8000;
    let backend = CrosstermBackend::new(std::io::stdout());
    let mut term = Terminal::new(backend)?;
    term.clear()?;
    let mut gb = Gameboy::new(TMP_ROM);
    run_until_complete(gb.start_up(), &mut state, &mut term)?;
    println!("Startup sequence complete!");
    run_until_complete(gb, &mut state, &mut term)?;
    Ok(())
}

/// This function abstracts over running the startup sequence and the gameboy itself.
fn run_until_complete<GB, B>(
    mut gb: GB,
    state: &mut AppState,
    term: &mut Terminal<B>,
) -> Result<(), Box<dyn Error>>
where
    GB: GameBoyLike,
    B: Backend,
{
    while !gb.is_complete() {
        term.clear()?;
        term.draw(|frame| render_frame(frame, state, gb.gb()))?;
        // print!("{} $ ", GB::PROMPT);
        // std::io::stdout().flush().unwrap();
        get_input(state).process(&mut gb);
    }
    Ok(())
}

fn render_frame(frame: &mut Frame, state: &mut AppState, gb: &Gameboy) {
    let sections = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Fill(1), Constraint::Length(40)])
        .split(frame.size());
    let left = sections[0];
    let right = sections[1];
    let left = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Fill(1), Constraint::Fill(1)])
        .split(left);
    let right = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Length(10),
            Constraint::Length(20),
            Constraint::Length(4),
            Constraint::Fill(1),
        ])
        .split(right);
    render_cli(frame, state, left[0]);
    // render_pc_area(frame, state, left[1], gb);
    render_mem(frame, state, left[1], &gb.mem);
    render_cpu(frame, state, right[0], gb.cpu());
    render_ppu(frame, state, right[1], &gb.ppu);
    render_interrupts(frame, state, right[2], &gb.mem);
    render_stack(frame, state, right[3], &gb.mem);
    // render_mem(frame, state, right[2]);
}

fn render_cli(frame: &mut Frame, state: &mut AppState, area: Rect) {
    let block = Block::bordered()
        .title("CLI")
        .title_alignment(ratatui::layout::Alignment::Center);
    let Rect { x, y, height, .. } = block.inner(area);
    let cursor_y = y + std::cmp::min(state.cli_history.len(), (height - 1) as usize) as u16;
    let iter = state
        .cli_history
        .iter()
        .rev()
        .take((height - 1) as usize)
        .rev()
        .map(|s| s.as_str());
    let para = Paragraph::new(Text::from_iter(iter)).block(block);
    frame.set_cursor(x, cursor_y);
    frame.render_widget(para, area);
}

fn render_pc_area(frame: &mut Frame, state: &mut AppState, area: Rect, gb: &Gameboy) {
    let block = Block::bordered()
        .title(format!(" PC Area {} ", area.width))
        .title_alignment(ratatui::layout::Alignment::Center);
    let len = area.height - 2;
    let para = Paragraph::new(Text::from_iter(
        gb.op_iter()
            .map(|(pc, op)| format!("0x{pc:0>4X} -> {op}\n"))
            .take(len as usize),
    ))
    .block(block);
    frame.render_widget(para, area);
}

/* This is the right column. In order, the CPU, PPU, Interrupts, and then the stack are rendered */

fn render_cpu(frame: &mut Frame, state: &mut AppState, area: Rect, cpu: &Cpu) {
    let block = Block::bordered()
        .title("CPU")
        .title_alignment(ratatui::layout::Alignment::Center);
    let para = Paragraph::new(Text::from_iter([
        format!("A : 0x{:0>2X}", cpu.a),
        format!(
            "F : Z={} N={} H={} C={}",
            cpu.f.z as u8, cpu.f.n as u8, cpu.f.h as u8, cpu.f.c as u8,
        ),
        format!("BC: 0x{:0>2X} 0x{:0>2X}", cpu.b, cpu.c),
        format!("DE: 0x{:0>2X} 0x{:0>2X}", cpu.d, cpu.e),
        format!("HL: 0x{:0>2X} 0x{:0>2X}", cpu.h, cpu.l),
        format!("PC: 0x{:0>4X}", cpu.pc),
        format!("SP: 0x{:0>4X}", cpu.sp),
        format!("IME: {}", cpu.ime),
    ]))
    .block(block);
    frame.render_widget(para, area);
}

fn render_ppu(frame: &mut Frame, state: &mut AppState, area: Rect, ppu: &Ppu) {
    let block = Block::bordered()
        .title("PPU")
        .title_alignment(ratatui::layout::Alignment::Center);
    let para = Paragraph::new(format!("{:#?}", ppu.inner)).block(block);
    frame.render_widget(para, area);
}

fn render_interrupts(frame: &mut Frame, state: &mut AppState, area: Rect, mem: &MemoryMap) {
    let block = Block::bordered()
        .title("Interrupts")
        .title_alignment(ratatui::layout::Alignment::Center);
    let para = Paragraph::new(Text::from_iter([
        format!("Enabled   : 0b{0:0>8b}"), // , mem.ie),
        format!("Requested : 0b{0:0>8b}"), // , mem.io.interrupt_flags),
    ]))
    .block(block);
    frame.render_widget(para, area);
}

fn render_stack(frame: &mut Frame, state: &mut AppState, area: Rect, mem: &MemoryMap) {
    let block = Block::bordered()
        .title("Stack")
        .title_alignment(ratatui::layout::Alignment::Center);
    // let iter = [].into_iter();
    let text = format!(
        "0x{:0>2X}{:0>2X} 0x{:0>2X}{:0>2X}",
        mem[0xFFFE], mem[0xFFFD], mem[0xFFFC], mem[0xFFFB]
    );
    let para = Paragraph::new(Text::from(text)).block(block);
    frame.render_widget(para, area);
}

// TODO: Move mem

fn render_mem(frame: &mut Frame, state: &mut AppState, area: Rect, mem: &MemoryMap) {
    let block = Block::bordered()
        .title("Memory")
        .title_alignment(ratatui::layout::Alignment::Center);
    let mem_start = state.mem_start & 0xFFF0;
    let lines = area.height - 2;
    let mut buffer = vec![0; 16 * lines as usize];
    (mem_start..)
        .zip(buffer.iter_mut())
        .for_each(|(i, byte)| *byte = std::panic::catch_unwind(|| mem[i]).unwrap_or_default());

    let mut data = String::from("  ADDR | 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n");
    data.push_str("-------|------------------------------------------------\n");
    for i in 0..lines {
        let start = i * 16;
        let end = (i + 1) * 16;
        write!(data, "0x{:0>4X} |", mem_start + start);
        for i in start..end {
            write!(data, " {:0>2X}", buffer[i as usize]);
        }
        data.push('\n');
    }
    let para = Paragraph::new(data).block(block);
    frame.render_widget(para, area);
}

trait GameBoyLike {
    const PROMPT: &'static str;

    fn gb(&self) -> &Gameboy;

    fn next_op(&self) -> Instruction;

    fn step(&mut self);

    fn step_frame(&mut self);

    fn is_complete(&self) -> bool;
}

impl GameBoyLike for Gameboy {
    const PROMPT: &'static str = "running";

    fn gb(&self) -> &Gameboy {
        self
    }

    fn next_op(&self) -> Instruction {
        self.cpu().read_op(&self.mem)
    }

    fn step(&mut self) {
        self.step().complete()
    }

    fn step_frame(&mut self) {
        self.next_frame().complete()
    }

    fn is_complete(&self) -> bool {
        self.cpu().state
    }
}

impl<'a> GameBoyLike for StartUpSequence<'a> {
    const PROMPT: &'static str = "init";

    fn gb(&self) -> &Gameboy {
        self.gb()
    }

    fn next_op(&self) -> Instruction {
        *self.next_op()
    }

    fn step(&mut self) {
        self.step()
    }

    fn step_frame(&mut self) {
        self.frame_step().complete()
    }

    fn is_complete(&self) -> bool {
        self.is_complete()
    }
}
