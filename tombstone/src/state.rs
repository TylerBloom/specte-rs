use std::{
    cell::RefCell,
    io,
    rc::Rc,
    sync::{Arc, Mutex},
};

use tracing::level_filters::LevelFilter;
use tracing_subscriber::{
    fmt::{
        format::{Compact, DefaultFields, Format},
        MakeWriter,
    },
    FmtSubscriber,
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
#[derive(Debug, Default)]
pub(crate) struct AppState {
    // TODO: Add command history (don't store the actual command, store the string that then get
    // parsed into commands.
    // TODO: We should limit the command and output history. We don't want it to grow forever.
    pub(crate) cli_history: Vec<String>,
    pub(crate) subscriber:
        Option<FmtSubscriber<DefaultFields, Format<Compact, ()>, LevelFilter, GameboySubscriber>>,
    buffer: Arc<Mutex<Vec<u8>>>,
}

/// This captures the different levels of verbosity, current, there is only two, but others will be
/// added to adjust the max filter level of the inner subscriber.
pub enum Verbosity {
    Quite,
    Verbose,
}

impl AppState {
    pub fn new() -> Self {
        Self::default()
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
