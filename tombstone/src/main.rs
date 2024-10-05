#![allow(dead_code, unused)]
use std::fmt::Write;
use std::sync::{Arc, Mutex};
use std::{error::Error, io::Write as _};

use ghast::state::Emulator;
use ratatui::layout::Position;
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout, Rect},
    text::Text,
    widgets::{Block, Paragraph},
    Frame, Terminal,
};
use spirit::cpu::CpuState;
use spirit::{cpu::Cpu, lookup::Instruction, mem::MemoryMap, ppu::Ppu, Gameboy, StartUpSequence};

pub mod command;
mod repl;
mod state;
mod window;
use command::*;
use state::*;
use tokio::sync::broadcast;
use window::WindowState;

// TODO: handle ctrl-C and ctrl-d

static TMP_ROM: &[u8] = include_bytes!("../../spirit/tests/roms/acid/which.gb");

fn main() {
    let backend = CrosstermBackend::new(std::io::stdout());
    let mut term = Terminal::new(backend).unwrap();
    term.clear().unwrap();
    let mut gb = Arc::new(Mutex::new(Emulator::default()));
    let (send, recv) = broadcast::channel(100);
    let mut state = AppState::new(gb.clone());
    state.mem_start = 0x8000;
    std::thread::spawn(move || state.run(&mut term));
    WindowState::new(gb, recv).run();
}

fn render_frame(frame: &mut Frame, gb: &Gameboy) {
    let sections = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Fill(1), Constraint::Length(40)])
        .split(frame.area());
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
    render_cli(frame, left[0]);
    // render_pc_area(frame, left[1], gb);
    render_mem(frame, left[1], &gb.mem);
    render_cpu(frame, right[0], gb.cpu());
    render_ppu(frame, right[1], &gb.ppu);
    render_interrupts(frame, right[2], &gb.mem);
    render_stack(frame, right[3], &gb.mem);
    // render_mem(frame, right[2]);
}

fn render_cli(frame: &mut Frame, area: Rect) {
    let block = Block::bordered()
        .title("CLI")
        .title_alignment(ratatui::layout::Alignment::Center);
    let Rect { x, y, height, .. } = block.inner(area);
    /*
    let cursor_y = y + std::cmp::min(state.cli_history.len(), (height - 1) as usize) as u16;
    let iter = state
        .cli_history
        .iter()
        .rev()
        .take((height - 1) as usize)
        .rev()
        .map(|s| s.as_str());
    */
    let para = Paragraph::new(Text::from_iter(std::iter::once(String::new()))).block(block);
    frame.set_cursor_position(Position::new(x, 0));
    frame.render_widget(para, area);
}

fn render_pc_area(frame: &mut Frame, area: Rect, gb: &Gameboy) {
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

fn render_cpu(frame: &mut Frame, area: Rect, cpu: &Cpu) {
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

fn render_ppu(frame: &mut Frame, area: Rect, ppu: &Ppu) {
    let block = Block::bordered()
        .title("PPU")
        .title_alignment(ratatui::layout::Alignment::Center);
    let para = Paragraph::new(format!("{:#?}", ppu.inner)).block(block);
    frame.render_widget(para, area);
}

fn render_interrupts(frame: &mut Frame, area: Rect, mem: &MemoryMap) {
    let block = Block::bordered()
        .title("Interrupts")
        .title_alignment(ratatui::layout::Alignment::Center);
    let para = Paragraph::new(Text::from_iter([
        format!("Enabled   : 0b{:0>8b}", mem.ie), // , mem.ie),
        format!("Requested : 0b{:0>8b}", mem.io().interrupt_flags), // , mem.io.interrupt_flags),
    ]))
    .block(block);
    frame.render_widget(para, area);
}

fn render_stack(frame: &mut Frame, area: Rect, mem: &MemoryMap) {
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

fn render_mem(frame: &mut Frame, area: Rect, mem: &MemoryMap) {
    let block = Block::bordered()
        .title("Memory")
        .title_alignment(ratatui::layout::Alignment::Center);
    // let mem_start = state.mem_start & 0xFFF0;
    let mem_start = 0x8000;
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
