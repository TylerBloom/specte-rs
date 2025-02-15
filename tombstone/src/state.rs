use clap::Parser;
use crossterm::event::{Event as CrosstermEvent, KeyCode, KeyEvent, KeyEventKind, KeyModifiers};
use crossterm::execute;
use crossterm::terminal::LeaveAlternateScreen;
use ghast::state::Emulator;
use indexmap::IndexSet;
use ratatui::layout::Position;
use ratatui::{
    backend::{Backend, CrosstermBackend},
    layout::{Constraint, Direction, Layout, Rect},
    text::Text,
    widgets::{Block, Paragraph},
    Frame, Terminal,
};
use spirit::cpu::CpuState;
use spirit::lookup::{InterruptOp, JumpOp};
use spirit::{cpu::Cpu, lookup::Instruction, mem::MemoryMap, ppu::Ppu, Gameboy, StartUpSequence};
use std::cell::Cell;
use std::collections::HashMap;
use std::fmt::Write;
use std::sync::mpsc;
use std::{
    cell::RefCell,
    io,
    rc::Rc,
    sync::{Arc, Mutex},
};
use std::{error::Error, io::Write as _};
use tokio::sync::broadcast::Sender;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::{
    fmt::{
        format::{Compact, DefaultFields, Format},
        MakeWriter,
    },
    FmtSubscriber,
};

use crate::{
    Command, IndexOptions, LoopKind, ReplCommand, RunFor, RunLength, RunUntil, ViewCommand,
    WindowMessage,
};

/// This is the app's state which holds all of the CLI data. This includes all previous commands
/// that were ran and all data to be displayed in the TUI (prompts, inputs, command outputs).
///
/// Command outputs are collected from the tracing within the gameboy. Because this can be slow and
/// noisy, whether or not we collect logs from the emulator is togglable. Because we want to
/// redirect the logs from stdout into memory, we must maintain a buffer that the tracing
/// subscriber can write into. Then, we can pull out that log, append it to the history, and
/// replace the buffer.
///
/// Unfortunately, subscribers are designed with threading in mind. Even though this application is
/// strickly single-threaded, we must wrap the buffer in an Arc<Mutex> (rather than an
/// Rc<RefCell>).
pub(crate) struct AppState {
    // TODO: Add command history (don't store the actual command, store the string that then get
    // parsed into commands.
    // TODO: We should limit the command and output history. We don't want it to grow forever.
    gb: Arc<Mutex<Emulator>>,
    processor: CommandProcessor,
    outbound: Sender<WindowMessage>,
    pub(crate) cli_history: Vec<String>,
    pub(crate) subscriber:
        Option<FmtSubscriber<DefaultFields, Format<Compact, ()>, LevelFilter, GameboySubscriber>>,
    buffer: Arc<Mutex<Vec<u8>>>,
    pub(crate) mem_start: u16,
    /// Tracks where the user input is at. This used to maintain the input data.
    cursor_position: Cell<Position>,
    pc_state: PcState,
}

pub struct PcState {
    pub start: usize,
    ops: HashMap<u16, Instruction>,
}

impl PcState {
    fn new() -> Self {
        Self {
            start: 0,
            ops: HashMap::new(),
        }
    }

    fn render(&mut self, frame: &mut Frame, area: Rect, gb: &Gameboy) {
        let block = Block::bordered()
            .title(" PC Area ")
            .title_alignment(ratatui::layout::Alignment::Center);
        let len = block.inner(area).height as usize;
        let para = Paragraph::new(self.text_body(len, gb)).block(block);
        frame.render_widget(para, area);
    }

    fn text_body(&mut self, len: usize, gb: &Gameboy) -> String {
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
                if *pc == gb.cpu().pc.0 {
                    format!("PC > 0x{pc:0>4X} -> {op}\n")
                } else {
                    format!("     0x{pc:0>4X} -> {op}\n")
                }
            })
            .take(len)
            .collect()
    }

    fn find_prior_op(&self, addr: u16) -> Option<(u16, Instruction)> {
        let mut op: heapless::Vec<(u16, Instruction), 3> = (addr.saturating_sub(3)..addr)
            .filter_map(|addr| self.ops.get_key_value(&addr))
            .map(|(addr, op)| (*addr, *op))
            .collect();
        assert!(op.iter().map(|(_, op)| op.size()).sum::<u8>() <= 3);
        op.last().copied()
    }
}

pub struct CommandProcessor {
    inbound: mpsc::Receiver<CrosstermEvent>,
    index: usize,
    buffer: String,
    last_longest_input: usize,
}

/// This captures the different levels of verbosity, current, there is only two, but others will be
/// added to adjust the max filter level of the inner subscriber.
pub enum Verbosity {
    Quite,
    Verbose,
}

impl AppState {
    pub fn new(
        gb: Arc<Mutex<Emulator>>,
        inbound: mpsc::Receiver<CrosstermEvent>,
        outbound: Sender<WindowMessage>,
    ) -> Self {
        Self {
            gb,
            cli_history: Vec::new(),
            subscriber: None,
            buffer: Arc::new(Mutex::new(Vec::new())),
            mem_start: 0,
            outbound,
            processor: CommandProcessor {
                inbound,
                index: 0,
                buffer: String::new(),
                last_longest_input: 0,
            },
            cursor_position: Cell::new(Position::default()),
            pc_state: PcState::new(),
        }
    }

    pub fn run<B: Backend>(mut self, mut term: Terminal<B>) {
        self.draw(&mut term);
        loop {
            if let Some(cmd) = self.processor.next_event(&mut self.cli_history) {
                self.process(cmd);
                self.draw(&mut term);
            }
            self.draw_input_line(&mut term);
        }
    }

    /// Draws the entire TUI
    fn draw<B: Backend>(&mut self, term: &mut Terminal<B>) {
        term.clear().unwrap();
        term.draw(|frame| self.render_frame(frame)).unwrap();
    }

    /// (Re)Draws the line that users input. This allows the input to be rendered without needing
    /// to rerender the entire TUI.
    fn draw_input_line<B: Backend>(&self, term: &mut Terminal<B>) {
        let mut pos = self.cursor_position.get();
        term.set_cursor_position(pos);
        let mut stdout = std::io::stdout();
        let input = self
            .processor
            .buffer
            .chars()
            .chain(std::iter::repeat(' '))
            .take(self.processor.last_longest_input)
            .collect::<String>();
        write!(stdout, "{input}").unwrap();
        stdout.flush().unwrap();
        pos.x += self.processor.buffer.len() as u16;
        term.set_cursor_position(pos);
    }

    fn process(&mut self, cmd: Command) {
        match cmd {
            Command::Read { index } => {
                let val = self.gb.lock().unwrap().gb().mem.read_byte(index);
                self.cli_history
                    .push(format!("0x{index:0>4X} -> 0b{val:0>8b}"));
            }
            Command::Redraw => {}
            Command::Exit => {
                execute!(std::io::stdout(), LeaveAlternateScreen);
                std::process::exit(0)
            }
            Command::Step { count } => {
                let mut gb = self.gb.lock().unwrap();
                (0..count).for_each(|_| gb.step_op());
            }
            Command::Info => todo!(),
            Command::Index(options) => match options {
                IndexOptions::Single { addr } => self.mem_start = addr,
                IndexOptions::Range { .. } => todo!(),
            },
            Command::Run(length) => match length {
                RunLength::For(foor) => match foor {
                    RunFor::Frames { count } => {
                        self.outbound.send(WindowMessage::Frames(count)).unwrap();
                    }
                },
                RunLength::Until(until) => match until {
                    RunUntil::Loop(kind) => match kind {
                        LoopKind::CpuAndMem => todo!(),
                        LoopKind::Screen => self.loop_screen(),
                    },
                    RunUntil::Return => {
                        let mut gb = self.gb.lock().unwrap();
                        while !matches!(
                            gb.gb().op_iter().next().unwrap().1,
                            Instruction::Jump(JumpOp::Return | JumpOp::ReturnAndEnable)
                        ) {
                            gb.step_op()
                        }
                    }
                    RunUntil::Frame => self.gb.lock().unwrap().next_screen(),
                    RunUntil::Pause => {
                        self.outbound.send(WindowMessage::Run).unwrap();
                    }
                    RunUntil::Interupt => {
                        let mut gb = self.gb.lock().unwrap();
                        let ints = gb.gb().mem.io().interrupt_flags;
                        while gb.gb().mem.io().interrupt_flags == ints {
                            gb.step_op();
                        }
                    }
                    RunUntil::Custom => {
                        let mut gb = self.gb.lock().unwrap();
                        let mut cond = custom_until_condition(gb.gb());
                        while cond(gb.gb()) {
                            gb.step_op();
                        }
                    }
                },
            },
            Command::Interrupt(_) => todo!(),
            Command::Stash(_) => todo!(),
            Command::Pause => {
                self.outbound.send(WindowMessage::Pause).unwrap();
            }
            Command::View(ViewCommand::PC { start }) => {
                self.pc_state.start = start;
            }
        }
        /*
        match msg {
            Command::Info => gb.gb().cpu().to_string(),
            Command::Step(n) => {
                (0..n).any(|_| {
                    gb.step();
                    gb.is_complete()
                });
                String::new()
            }
            Command::Index(opts) => match opts {
                IndexOptions::Single(i) => {
                    format!("ADDR=0x{i:0>4X} -> {:0>2X}", gb.gb().mem[i as u16])
                }
                IndexOptions::Range(mut rng) => {
                    let mut digest = format!("ADDR=0x{:0>4X}..0x{:0>4X}", rng.start, rng.end);
                    digest.push('[');
                    if let Some(i) = rng.next() {
                        write!(digest, "0x{:0>2}", gb.gb().mem[i as u16]).unwrap();
                    }
                    for i in rng {
                        write!(digest, ", 0x{:0>2}", gb.gb().mem[i as u16]);
                    }
                    digest.push(']');
                    digest
                }
            },
            Command::Run(until) => match until {
                RunUntil::Loop => {
                    let mut ops = Vec::new();
                    let mut cpu_map: HashMap<_, HashMap<u64, usize>> = HashMap::new();
                    let mut index = 0;
                    while !gb.is_complete() {
                        let cpu = gb.gb().cpu().clone();
                        let ptr = cpu.pc.0;
                        ops.push((cpu, ptr, gb.next_op()));
                        match cpu_map.entry(gb.gb().cpu().clone()) {
                            Entry::Occupied(mut entry) => {
                                let mut hasher = DefaultHasher::new();
                                gb.gb().mem.hash(&mut hasher);
                                match entry.get_mut().entry(hasher.finish()) {
                                    Entry::Vacant(entry) => {
                                        entry.insert(ops.len());
                                    }
                                    Entry::Occupied(entry) => {
                                        index = *entry.get();
                                        break;
                                    }
                                }
                            }
                            Entry::Vacant(entry) => {
                                entry.insert(HashMap::new());
                            }
                        }
                        gb.step()
                    }
                    if !gb.is_complete() {
                        let mut digest = "Infinite loop detected! Here are the instructions and CPU states in the loop:".to_string();
                        for (state, ptr, op) in &ops[index..] {
                            writeln!(digest, "\n{state}");
                            write!(digest, "0x{ptr:0>4X} -> {op}");
                        }
                        digest
                    } else {
                        "No loop detect during start up!".to_owned()
                    }
                }
                RunUntil::Return => {
                    while !matches!(gb.next_op(), Instruction::Jump(JumpOp::Return)) {
                        gb.step()
                    }
                    "End of subroutine. Next operation is `ret`.".to_owned()
                }
                RunUntil::Frame => {
                    gb.step_frame();
                    "Starting new frame".to_owned()
                }
            },
            Command::Stash(_stash) => {
                todo!()
            }
            Command::Interrupt(_interrupt) => todo!(),
        }
        */
    }

    /// Runs the emulator screen by screen collecting them until a duplicate is seen. The
    /// collection is sent to the UI state to be rendered.
    fn loop_screen(&mut self) {
        let mut screens = IndexSet::new();
        let mut gb = self.gb.lock().unwrap();
        loop {
            if !screens.insert(gb.gb().ppu.screen.clone()) {
                break;
            }
            gb.next_screen();
        }
        self.outbound.send(WindowMessage::DuplicateScreens(screens));
    }

    fn render_frame(&mut self, frame: &mut Frame) {
        let gb = self.gb.lock().unwrap();
        let gb = gb.gb();
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
        self.cursor_position
            .set(render_cli(frame, &self.cli_history, left[0]));
        self.render_mem(frame, left[1], &gb.mem);
        render_cpu(frame, right[0], gb.cpu());
        render_ppu(frame, right[1], &gb.ppu);
        render_interrupts(frame, right[2], &gb.mem);
        // render_stack(frame, right[3], &gb.mem);
        self.pc_state.render(frame, right[3], gb);
    }

    fn render_mem(&self, frame: &mut Frame, area: Rect, mem: &MemoryMap) {
        let block = Block::bordered()
            .title("Memory")
            .title_alignment(ratatui::layout::Alignment::Center);
        // let mem_start = state.mem_start & 0xFFF0;
        let mem_start = self.mem_start;
        let lines = area.height - 2;
        let mut buffer = vec![0; 16 * lines as usize];
        (mem_start..).zip(buffer.iter_mut()).for_each(|(i, byte)| {
            *byte = std::panic::catch_unwind(|| mem.read_byte(i)).unwrap_or_default()
        });

        let mut data = String::from("  ADDR | 00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F\n");
        data.push_str("-------|------------------------------------------------\n");
        for i in 0..lines {
            let start = i * 16;
            let end = (i + 1) * 16;
            write!(data, "0x{:0>4X} |", mem_start + start);
            (start..end).try_for_each(|i| write!(data, " {:0>2X}", buffer[i as usize]));
            data.push('\n');
        }
        let para = Paragraph::new(data).block(block);
        frame.render_widget(para, area);
    }

    // TODO: Because the state now tracks the command history and any output from the emulator, we
    // ought to move the logic that parses out commands and runs them here. This will ensure that
    // all of the bookkeeping is done in one place.
    //
    // This will also reduce the API footprint, only need to have methods that return references to
    // the data within and a single method to process input, run the command/display errors, and
    // store the input and output.

    pub fn set_verbosity(&mut self, level: Verbosity) {
        match level {
            Verbosity::Quite => self.subscriber = None,
            Verbosity::Verbose => self.subscriber = Some(self.construct_sub()),
        }
    }

    fn construct_sub(
        &self,
    ) -> FmtSubscriber<DefaultFields, Format<Compact, ()>, LevelFilter, GameboySubscriber> {
        FmtSubscriber::builder()
            .compact()
            .without_time()
            .with_max_level(LevelFilter::TRACE)
            .with_writer(GameboySubscriber(Arc::clone(&self.buffer)))
            .finish()
    }

    pub fn get_log(&mut self) -> Option<&str> {
        // There should never be non-utf8 characters writen out.
        let mut lock = self.buffer.lock().unwrap();
        if lock.is_empty() {
            return None;
        }
        let log = String::from_utf8(std::mem::take(&mut lock)).unwrap();
        self.cli_history.push(log);
        Some(self.cli_history.last().unwrap().as_str())
    }
}

#[derive(Debug, Clone)]
pub(crate) struct GameboySubscriber(Arc<Mutex<Vec<u8>>>);

impl<'a> MakeWriter<'a> for GameboySubscriber {
    type Writer = &'a Self;

    fn make_writer(&'a self) -> Self::Writer {
        self
    }
}

impl io::Write for &'_ GameboySubscriber {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.lock().unwrap().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.lock().unwrap().flush()
    }
}

fn render_cli(frame: &mut Frame, history: &[String], area: Rect) -> Position {
    let block = Block::bordered()
        .title("CLI")
        .title_alignment(ratatui::layout::Alignment::Center);
    let Rect { x, y, height, .. } = block.inner(area);
    let cursor_y = y + std::cmp::min(history.len(), (height - 1) as usize) as u16;
    let iter = history
        .iter()
        .rev()
        .take((height - 1) as usize)
        .rev()
        .map(|s| s.as_str());
    //  .chain(std::iter::once(user_input));
    let para = Paragraph::new(Text::from_iter(iter)).block(block);
    let pos = Position::new(x, cursor_y);
    frame.set_cursor_position(pos);
    frame.render_widget(para, area);
    pos
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
        mem.read_byte(0xFFFE),
        mem.read_byte(0xFFFD),
        mem.read_byte(0xFFFC),
        mem.read_byte(0xFFFB)
    );
    let para = Paragraph::new(Text::from(text)).block(block);
    frame.render_widget(para, area);
}

// TODO: Move mem

impl CommandProcessor {
    fn update_last_longest(&mut self) {
        self.last_longest_input = std::cmp::max(self.last_longest_input, self.buffer.len());
    }

    fn next_event(&mut self, cli_history: &mut Vec<String>) -> Option<Command> {
        match self.inbound.recv().unwrap() {
            CrosstermEvent::Resize(_, _) => Some(Command::Redraw),
            CrosstermEvent::Paste(data) => {
                self.buffer.push_str(&data);
                self.update_last_longest();
                None
            }
            CrosstermEvent::Key(key) if matches!(key.kind, KeyEventKind::Press) => match key.code {
                KeyCode::Backspace => {
                    self.buffer.pop();
                    None
                }
                KeyCode::Enter => {
                    let cmd = ReplCommand::try_parse_from(self.buffer.split_whitespace());
                    cli_history.push(std::mem::take(&mut self.buffer));
                    self.index = 0;
                    self.last_longest_input = 0;
                    Some(
                        cmd.map(|cmd| cmd.command)
                            .unwrap_or_else(|_| Command::Redraw),
                    )
                }
                KeyCode::Up if self.index < cli_history.len() => {
                    self.index = std::cmp::min(self.index + 1, cli_history.len());
                    self.buffer
                        .clone_from(&cli_history[cli_history.len() - self.index]);
                    self.update_last_longest();
                    None
                }
                KeyCode::Down if self.index > 0 => {
                    self.index -= 1;
                    if self.index == 0 {
                        self.buffer.clear();
                    } else {
                        self.buffer
                            .clone_from(&cli_history[cli_history.len() - self.index]);
                    }
                    self.update_last_longest();
                    None
                }
                KeyCode::Char(c) => {
                    if key.modifiers.contains(KeyModifiers::CONTROL) {
                        if c == 'c' {
                            cli_history.push(std::mem::take(&mut self.buffer));
                            return None;
                        } else if c == 'd' {
                            return Some(Command::Exit);
                        }
                    }
                    self.buffer.push(c);
                    self.update_last_longest();
                    None
                }
                // Not used (yet)
                _ => None,
            },
            CrosstermEvent::Key(_) => None,
            CrosstermEvent::Mouse(_) => None,
            CrosstermEvent::FocusGained => None,
            CrosstermEvent::FocusLost => None,
        }
    }
}

fn custom_until_condition(_gb: &Gameboy) -> impl 'static + FnMut(&Gameboy) -> bool {
    |gb| !matches!(gb.read_op(), Instruction::Ei)
}
