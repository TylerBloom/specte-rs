use std::{
    cell::RefCell,
    io,
    rc::Rc,
    sync::{Arc, Mutex},
};

use ghast::state::Emulator;
use ratatui::{prelude::Backend, Terminal};
use tracing::level_filters::LevelFilter;
use tracing_subscriber::{
    fmt::{
        format::{Compact, DefaultFields, Format},
        MakeWriter,
    },
    FmtSubscriber,
};

use crate::{render_frame, Command};

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
    pub(crate) cli_history: Vec<String>,
    pub(crate) subscriber:
        Option<FmtSubscriber<DefaultFields, Format<Compact, ()>, LevelFilter, GameboySubscriber>>,
    buffer: Arc<Mutex<Vec<u8>>>,
    pub(crate) mem_start: u16,
}

/// This captures the different levels of verbosity, current, there is only two, but others will be
/// added to adjust the max filter level of the inner subscriber.
pub enum Verbosity {
    Quite,
    Verbose,
}

impl AppState {
    pub fn new(gb: Arc<Mutex<Emulator>>) -> Self {
        Self {
            gb,
            cli_history: Vec::new(),
            subscriber: None,
            buffer: Arc::new(Mutex::new(Vec::new())),
            mem_start: 0,
        }
    }

    pub fn run<B: Backend>(mut self, term: &mut Terminal<B>) {
        loop {
            term.clear().unwrap();
            term.draw(|frame| render_frame(frame, self.gb.lock().unwrap().gb()))
                .unwrap();
            // print!("{} $ ", GB::PROMPT);
            // std::io::stdout().flush().unwrap();
            let message = self.get_input();
            self.process(message);
        }
    }

    fn process(&mut self, msg: Command) {
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

    fn get_input(&mut self) -> Command {
        let mut input = String::new();
        std::io::stdin().read_line(&mut input).unwrap();
        let cmd = untwine::parse(crate::command::command, input.trim()).unwrap();
        self.cli_history.push(input);
        cmd
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

impl<'a> io::Write for &'a GameboySubscriber {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.0.lock().unwrap().write(buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.0.lock().unwrap().flush()
    }
}
