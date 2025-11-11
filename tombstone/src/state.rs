use std::collections::HashSet;
use std::fmt::Write as _;
use std::io;
use std::sync::Arc;
use std::sync::Mutex;

use crossterm::execute;
use crossterm::terminal::LeaveAlternateScreen;
use indexmap::IndexSet;
use ratatui::Frame;
use ratatui::Terminal;
use ratatui::backend::Backend;
use ratatui::layout::Constraint;
use ratatui::layout::Direction;
use ratatui::layout::Layout;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::FmtSubscriber;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::fmt::format::Compact;
use tracing_subscriber::fmt::format::DefaultFields;
use tracing_subscriber::fmt::format::FmtSpan;
use tracing_subscriber::fmt::format::Format;

use spirit::Gameboy;
use spirit::lookup::HalfRegister;
use spirit::lookup::Instruction;
use spirit::lookup::JumpOp;
use spirit::lookup::LoadOp;
use spirit::lookup::RegOrPointer;
use spirit::mem::MemoryLike;
use spirit::mem::vram::PpuMode;

use crate::Command;
use crate::IndexOptions;
use crate::LoopKind;
use crate::RunFor;
use crate::RunLength;
use crate::RunUntil;
use crate::ViewCommand;
use crate::cli::Cli;
use crate::command::BreakpointCommand;
use crate::config::Config;
use crate::config::GameConfig;
use crate::display_windows::render_cpu;
use crate::display_windows::render_interrupts;
use crate::display_windows::render_mem;
use crate::display_windows::render_oam_dma;
use crate::display_windows::render_ram;
use crate::pc_state::PcState;

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
    inner: InnerAppState,
    cli: Cli,
    log_buffer: Arc<Mutex<Vec<u8>>>,
    pc_state: PcState,
}

pub struct InnerAppState {
    pub gb: Gameboy,
    pub config: GameConfig,
    pub skipped_breakpoints: HashSet<u16>,
    pub mem_start: u16,
}

impl AppState {
    pub fn new(cart: Vec<u8>) -> Self {
        let config = Config::load().load_game_config(&cart);

        let log_buffer = Arc::new(Mutex::new(Vec::new()));
        let writer = GameboySubscriber(log_buffer.clone());
        tracing_subscriber::fmt()
            .with_span_events(FmtSpan::ACTIVE)
            .without_time()
            .with_max_level(LevelFilter::INFO)
            .with_writer(writer.clone())
            .init();

        let inner = InnerAppState {
            config,
            gb: Gameboy::load_cartridge(cart).complete(),
            mem_start: 0x8000,
            skipped_breakpoints: HashSet::new(),
        };
        Self {
            inner,
            log_buffer,
            cli: Cli::new(),
            pc_state: PcState::new(),
        }
    }

    pub fn run<B: Backend>(mut self, mut term: Terminal<B>) {
        self.draw(&mut term);
        self.cli.draw_input_line(&mut term);
        loop {
            if let Some(cmd) = self.cli.next_event() {
                self.process(cmd);
                let mut lock = self.log_buffer.lock().unwrap();
                String::from_utf8_lossy(&lock)
                    .lines()
                    .for_each(|line| self.cli.display(line.into()));
                lock.drain(0..);
                drop(lock);
                self.draw(&mut term);
            }
            self.cli.draw_input_line(&mut term);
        }
    }

    /// Draws the entire TUI
    fn draw<B: Backend>(&mut self, term: &mut Terminal<B>) {
        term.clear().unwrap();
        term.draw(|frame| self.render_frame(frame)).unwrap();
    }

    fn process(&mut self, cmd: Command) {
        self.inner.process(&mut self.cli, &mut self.pc_state, cmd);
    }

    fn render_frame(&mut self, frame: &mut Frame) {
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
        let mem = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([Constraint::Fill(1), Constraint::Fill(1)])
            .split(left[1]);
        let right = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(10),
                Constraint::Length(20),
                Constraint::Length(4),
                Constraint::Fill(1),
            ])
            .split(right);
        self.cli.render(frame, left[0]);
        render_mem(&self.inner, frame, mem[0]);
        render_ram(&self.inner, frame, mem[1]);
        render_cpu(&self.inner, frame, right[0]);
        render_oam_dma(&self.inner, frame, right[1]);
        render_interrupts(&self.inner, frame, right[2]);
        self.pc_state.render(&self.inner, frame, right[3]);
    }
}

impl InnerAppState {
    fn process(&mut self, cli: &mut Cli, pc: &mut PcState, cmd: Command) {
        match cmd {
            Command::Read { index } => {
                let val = self.gb.mem.read_byte(index);
                cli.push_to_history(format!("0x{index:0>4X} -> 0b{val:0>8b}"));
            }
            Command::Redraw => {}
            Command::Exit => {
                _ = execute!(std::io::stdout(), LeaveAlternateScreen);
                std::process::exit(0)
            }
            Command::Step { count } => {
                (0..count).for_each(|_| self.gb.step());
            }
            Command::Info => todo!(),
            Command::Index(options) => match options {
                IndexOptions::Single { addr } => self.mem_start = addr,
                IndexOptions::Range { .. } => todo!(),
            },
            Command::Run(length) => self.process_run_cmd(length),
            Command::Interrupt(_) => todo!(),
            Command::Stash(_) => todo!(),
            Command::Pause => {
                // self.outbound.send(WindowMessage::Pause).unwrap();
                todo!()
            }
            Command::View(ViewCommand::PC { start }) => {
                pc.start = start;
            }
            Command::Breakpoint(cmd) => match cmd {
                BreakpointCommand::List => {
                    let mut digest = String::new();
                    for pc in self.config.breakpoints() {
                        let suffix = if self.skipped_breakpoints.contains(&pc) {
                            " (SKIPPED)"
                        } else {
                            ""
                        };
                        writeln!(digest, " - 0x{pc:0>4X}{suffix}").unwrap();
                    }
                    cli.display(digest);
                }
                BreakpointCommand::Add { pc } => {
                    let bp = pc.unwrap_or(self.gb.cpu().pc.0);
                    self.config.add_breakpoint(bp);
                }
                BreakpointCommand::Remove { pc } => {
                    let bp = pc.unwrap_or(self.gb.cpu().pc.0);
                    self.config.remove_breakpoint(bp);
                }
                BreakpointCommand::Skip { pc } => {
                    self.skipped_breakpoints.insert(pc);
                }
            },
        }
    }

    fn process_run_cmd(&mut self, length: RunLength) {
        match length {
            RunLength::For(foor) => match foor {
                RunFor::Frames { count: _ } => {
                    // self.outbound.send(WindowMessage::Frames(count)).unwrap();
                    todo!()
                }
            },
            RunLength::Until(until) => match until {
                RunUntil::Loop(kind) => match kind {
                    LoopKind::CpuAndMem => todo!(),
                    LoopKind::Screen => self.loop_screen(),
                },
                RunUntil::Return => self.run_until(|gb| {
                    !matches!(
                        gb.read_op(),
                        Instruction::Jump(JumpOp::Return | JumpOp::ReturnAndEnable)
                    )
                }),
                RunUntil::Frame => {
                    let mut mode = self.gb.ppu.state();
                    self.run_until(move |gb| {
                        let digest =
                            !matches!((mode, gb.ppu.state()), (PpuMode::VBlank, PpuMode::OamScan));
                        mode = gb.ppu.state();
                        digest
                    })
                }
                RunUntil::Pause => {
                    // self.outbound.send(WindowMessage::Run).unwrap();
                    todo!()
                }
                RunUntil::Interupt => {
                    let ints = self.gb.mem.io().interrupt_flags;
                    self.run_until(|gb| gb.mem.io().interrupt_flags != ints)
                }
                RunUntil::Custom => {
                    let mut cond = custom_until_condition(&self.gb);
                    while cond(&self.gb) {
                        self.gb.step();
                    }
                }
                RunUntil::PassedLoop { count } => {
                    let mut count = count.unwrap_or(1);
                    let mut pc = self.gb.cpu().pc;
                    // FIXME: Even for the very loose definition of "loop" and "end", this is a
                    // rough implementation. This does not account for interrupt. Even worse, this
                    // does not account for loops with calls that also have loops. In such a case
                    // where the call site is at a lower position than the called "function", this
                    // would case the emulator to run for an indeterminant amount of time.
                    self.run_until(|gb| {
                        if count > 0
                            && let Instruction::Jump(
                                JumpOp::Relative(i8::MIN..0)
                                | JumpOp::ConditionalRelative(_, i8::MIN..0),
                            ) = gb.read_op()
                            && pc < gb.cpu().pc
                        {
                            pc = gb.cpu().pc;
                            count -= 1;
                        }
                        count == 0 && gb.cpu().pc > pc
                    });
                }
            },
        }
    }

    fn run_until(&mut self, mut predicate: impl FnMut(&Gameboy) -> bool) {
        loop {
            self.gb.step();
            if self.gb.is_stopped() || self.at_breakpoint(self.gb.cpu().pc.0) || predicate(&self.gb)
            {
                break;
            }
        }
    }

    fn at_breakpoint(&self, pc: u16) -> bool {
        !self.skipped_breakpoints.contains(&pc) && self.config.is_breakpoint(pc)
    }

    /// Runs the emulator screen by screen collecting them until a duplicate is seen. The
    /// collection is sent to the UI state to be rendered.
    fn loop_screen(&mut self) {
        let mut screens = IndexSet::new();
        loop {
            if !screens.insert(self.gb.ppu.screen.clone()) {
                break;
            }
            self.gb.next_frame();
        }
        // _ = self.outbound.send(WindowMessage::DuplicateScreens(screens));
    }
}

fn custom_until_condition(_gb: &Gameboy) -> impl 'static + FnMut(&Gameboy) -> bool {
    |gb| {
        !matches!(
            gb.read_op(),
            Instruction::Load(LoadOp::Direct(RegOrPointer::Reg(HalfRegister::A), _))
        )
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
