#![allow(dead_code, unused)]
use std::{error::Error, io::Write};

use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout, Rect},
    text::Text,
    widgets::{Block, Paragraph},
    Frame, Terminal,
};
use spirit::{cpu::Cpu, lookup::Instruction, Gameboy, StartUpSequence};

mod command;
use command::*;

// TODO: handle ctrl-C and ctrl-d

static TMP_ROM: &[u8] = include_bytes!("../../spirit/tests/roms/acid/which.gb");

fn main() -> Result<(), Box<dyn Error>> {
    let backend = CrosstermBackend::new(std::io::stdout());
    let mut term = Terminal::new(backend)?;
    term.clear()?;
    let mut gb = Gameboy::new(TMP_ROM);
    run_until_complete(gb.start_up(), &mut term)?;
    println!("Startup sequence complete!");
    run_until_complete(gb, &mut term)?;
    Ok(())
}

/// This function abstracts over running the startup sequence and the gameboy itself.
fn run_until_complete<GB, B>(mut gb: GB, term: &mut Terminal<B>) -> Result<(), Box<dyn Error>>
where
    GB: GameBoyLike,
    B: Backend,
{
    while !gb.is_complete() {
        term.clear()?;
        term.draw(|frame| render_frame(frame, gb.gb()))?;
        // print!("{} $ ", GB::PROMPT);
        std::io::stdout().flush().unwrap();
        let output = match get_input() {
            Ok(cmd) => cmd.process(&mut gb),
            Err(e) => e.to_string(),
        };
    }
    Ok(())
}

fn render_frame(frame: &mut Frame, gb: &Gameboy) {
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
            Constraint::Fill(1),
            Constraint::Fill(1),
            Constraint::Fill(1),
        ])
        .split(right);
    render_cli(frame, left[0]);
    render_pc_area(frame, left[1], gb);
    render_cpu(frame, right[0], gb.cpu());
    render_stack(frame, right[1]);
    render_mem(frame, right[2]);
}

fn render_cli(frame: &mut Frame, area: Rect) {
    let block = Block::bordered()
        .title("CLI")
        .title_alignment(ratatui::layout::Alignment::Center);
    frame.render_widget(block, area);
}

fn render_pc_area(frame: &mut Frame, area: Rect, gb: &Gameboy) {
    let block = Block::bordered()
        .title("PC Area")
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
        format!("PC: 0x{:0>4X}", cpu.sp),
        format!("SP: 0x{:0>4X}", cpu.pc),
        format!("IME: {}", cpu.ime),
    ]))
    .block(block);
    frame.render_widget(para, area);
}

fn render_stack(frame: &mut Frame, area: Rect) {
    let block = Block::bordered()
        .title("Stack")
        .title_alignment(ratatui::layout::Alignment::Center);
    frame.render_widget(block, area);
}

fn render_mem(frame: &mut Frame, area: Rect) {
    let block = Block::bordered()
        .title("Memory")
        .title_alignment(ratatui::layout::Alignment::Center);
    frame.render_widget(block, area);
}

trait GameBoyLike {
    const PROMPT: &'static str;

    fn gb(&self) -> &Gameboy;

    fn next_op(&self) -> Instruction;

    fn step(&mut self);

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

    fn is_complete(&self) -> bool {
        self.cpu().done
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

    fn is_complete(&self) -> bool {
        self.is_complete()
    }
}
