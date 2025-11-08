use crossterm::execute;
use crossterm::terminal::LeaveAlternateScreen;
use indexmap::IndexSet;
use ratatui::Frame;
use ratatui::Terminal;
use ratatui::backend::Backend;
use ratatui::layout::Constraint;
use ratatui::layout::Direction;
use ratatui::layout::Layout;
use spirit::Gameboy;
use spirit::lookup::HalfRegister;
use spirit::lookup::Instruction;
use spirit::lookup::JumpOp;
use spirit::lookup::LoadOp;
use spirit::lookup::RegOrPointer;
use spirit::mem::MemoryLike;
use std::io;
use std::sync::Arc;
use std::sync::Mutex;
use tracing::level_filters::LevelFilter;
use tracing_subscriber::FmtSubscriber;
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::fmt::format::Compact;
use tracing_subscriber::fmt::format::DefaultFields;
use tracing_subscriber::fmt::format::Format;

use crate::Command;
use crate::IndexOptions;
use crate::LoopKind;
use crate::RunFor;
use crate::RunLength;
use crate::RunUntil;
use crate::ViewCommand;
use crate::cli::Cli;
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
    #[allow(dead_code)]
    pub(crate) subscriber:
        Option<FmtSubscriber<DefaultFields, Format<Compact, ()>, LevelFilter, GameboySubscriber>>,
    buffer: Arc<Mutex<Vec<u8>>>,
    pc_state: PcState,
}

pub struct InnerAppState {
    pub gb: Gameboy,
    pub config: GameConfig,
    pub mem_start: u16,
}

impl AppState {
    pub fn new(cart: Vec<u8>) -> Self {
        let config = Config::load().load_game_config(&cart);
        let inner = InnerAppState {
            config,
            gb: Gameboy::load_cartridge(cart).complete(),
            mem_start: 0x8000,
        };
        Self {
            inner,
            subscriber: None,
            buffer: Arc::new(Mutex::new(Vec::new())),
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

    // TODO: Because the state now tracks the command history and any output from the emulator, we
    // ought to move the logic that parses out commands and runs them here. This will ensure that
    // all of the bookkeeping is done in one place.
    //
    // This will also reduce the API footprint, only need to have methods that return references to
    // the data within and a single method to process input, run the command/display errors, and
    // store the input and output.
    #[allow(dead_code)]
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
                RunUntil::Return => {
                    while !matches!(
                        self.gb.op_iter().next().unwrap().1,
                        Instruction::Jump(JumpOp::Return | JumpOp::ReturnAndEnable)
                    ) {
                        self.gb.step()
                    }
                }
                RunUntil::Frame => self.gb.next_frame(),
                RunUntil::Pause => {
                    // self.outbound.send(WindowMessage::Run).unwrap();
                    todo!()
                }
                RunUntil::Interupt => {
                    let ints = self.gb.mem.io().interrupt_flags;
                    while self.gb.mem.io().interrupt_flags == ints {
                        self.gb.step();
                    }
                }
                RunUntil::Custom => {
                    let mut cond = custom_until_condition(&self.gb);
                    while cond(&self.gb) {
                        self.gb.step();
                    }
                }
            },
        }
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
