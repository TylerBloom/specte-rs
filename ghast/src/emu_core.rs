#[cfg(not(target_family = "wasm"))]
use std::env::home_dir;

use std::sync::Arc;
use std::time::Duration;

use futures::Stream;
use spirit::Gameboy;
use spirit::StartUpSequence;
use spirit::ppu::Pixel;

use futures::stream::StreamExt;
use instant::Instant;
use tokio::sync::mpsc;
use tokio_stream::wrappers::UnboundedReceiverStream;
use troupe::ActorKind;
use troupe::ActorState;
use troupe::Scheduler;
use troupe::compat::sleep_for;

use crate::keys::ButtonInteration;
use crate::keys::ControlSignal;
use crate::keys::Keystroke;
use crate::trove::Trove;
use crate::utils::screen_to_image_scaled;

/// This is the core of the emulator state. It is interfaced with via the `EmuHandle`. It is
/// intended that the core is ran in a seperate thread/task from the main core.
pub struct EmuCore {
    is_paused: bool,
    last_updated: Instant,
    frames: usize,
    emulator: Option<Emulator>,
    trove: Trove,
}

impl EmuCore {
    pub fn new(trove: Trove) -> Self {
        Self {
            is_paused: false,
            last_updated: Instant::now(),
            frames: 0,
            emulator: None,
            trove,
        }
    }
}

#[derive(Debug, Clone, derive_more::From)]
pub enum EmuMessage {
    NextFrame,
    Keystroke(Keystroke),
    LoadGame(Vec<u8>),
    AddGame,
    RfdReturn(Option<(String, Vec<u8>)>),
    TakeSnapShot,
    SaveState,
    FetchGameList,
}

#[derive(Debug, Clone, derive_more::From)]
pub enum EmuOutput {
    Frame(Frame),
    TroveGames(Vec<Arc<str>>),
}

impl ActorState for EmuCore {
    type ActorKind = SinkSendActor<EmuOutput>;
    type Message = EmuMessage;

    async fn start_up(&mut self, scheduler: &mut Scheduler<Self>) {
        scheduler.attach_stream(futures::stream::repeat(EmuMessage::NextFrame).then(|msg| {
            Box::pin(async move {
                sleep_for(Duration::from_secs(1) / 60).await;
                msg
            })
        }));
    }

    async fn process(&mut self, scheduler: &mut Scheduler<Self>, msg: Self::Message) {
        if self.emulator.is_some() {
            self.process_message(scheduler, msg).await
        } else {
            self.process_init_message(scheduler, msg).await
        }
    }
}

impl EmuCore {
    async fn process_init_message(&mut self, scheduler: &mut Scheduler<Self>, msg: EmuMessage) {
        match msg {
            EmuMessage::NextFrame => return,
            EmuMessage::Keystroke(_keystroke) => return,
            EmuMessage::TakeSnapShot => return,
            EmuMessage::SaveState => return,
            EmuMessage::AddGame => self.add_game(scheduler),
            EmuMessage::LoadGame(rom) => self.load_game(rom),
            EmuMessage::RfdReturn(msg) => self.rfd_return(msg),
            EmuMessage::FetchGameList => self.send_game_list(scheduler),
        }
    }

    async fn process_message(&mut self, scheduler: &mut Scheduler<Self>, msg: EmuMessage) {
        // Start up waits for the ROM, so unwrap won't panic
        let emu = self.emulator.as_mut().unwrap();
        match msg {
            EmuMessage::AddGame => self.add_game(scheduler),
            EmuMessage::RfdReturn(msg) => self.rfd_return(msg),
            EmuMessage::LoadGame(rom) => self.load_game(rom),
            EmuMessage::FetchGameList => self.send_game_list(scheduler),
            EmuMessage::TakeSnapShot => todo!(),
            EmuMessage::SaveState => todo!(),
            EmuMessage::NextFrame => {
                if !self.is_paused {
                    emu.next_frame();
                    self.frames += 1;
                    scheduler.send_message(EmuOutput::Frame((emu.just_pixels(), self.frames)));
                } else {
                    return;
                }
            }
            EmuMessage::Keystroke(Keystroke::Control(ControlSignal::Pause)) => {
                self.is_paused = !self.is_paused;
                return;
            }
            EmuMessage::Keystroke(Keystroke::Control(ControlSignal::NextFrame)) => {
                self.is_paused = true;
                emu.next_frame();
                self.frames += 1;
                scheduler.send_message(EmuOutput::Frame((emu.just_pixels(), self.frames)));
            }
            EmuMessage::Keystroke(Keystroke::Button(button)) => match button {
                ButtonInteration::ButtonPress(button) => {
                    let now = Instant::now();
                    step_duration(emu.gb_mut(), now - self.last_updated);
                    emu.gb_mut().button_press(button);
                    self.last_updated = now;
                    return;
                }
                ButtonInteration::ButtonRelease(button) => {
                    let now = Instant::now();
                    step_duration(emu.gb_mut(), now - self.last_updated);
                    emu.gb_mut().button_release(button);
                    self.last_updated = now;
                    return;
                }
            },
        }
        self.last_updated = Instant::now()
    }

    fn add_game(&mut self, scheduler: &mut Scheduler<Self>) {
        scheduler.await_message(async move {
            #[cfg(not(target_family = "wasm"))]
            let dialog = rfd::AsyncFileDialog::new().set_directory(home_dir().unwrap());
            #[cfg(target_family = "wasm")]
            let dialog = rfd::AsyncFileDialog::new();
            let digest = match dialog.pick_file().await {
                None => None,
                Some(handle) => Some((handle.file_name(), handle.read().await)),
            };
            EmuMessage::RfdReturn(digest)
        });
    }

    fn load_game(&mut self, rom: Vec<u8>) {
        self.frames = 0;
        self.is_paused = false;
        self.emulator = Some(Emulator::new(rom));
    }

    fn rfd_return(&mut self, msg: Option<(String, Vec<u8>)>) {
        match msg {
            Some((name, rom)) => self.trove.add_game(&name, &rom),
            None => return,
        }
    }

    fn send_game_list(&self, scheduler: &mut Scheduler<Self>) {
        scheduler.send_message(self.trove.list_games());
    }
}

pub type Frame = (Image, usize);

#[derive(Debug, Clone)]
pub struct Image {
    pub width: u32,
    pub height: u32,
    pub pixels: Vec<u8>,
}

impl Image {
    pub fn new(screen: &[Vec<Pixel>]) -> Self {
        const SCALE: usize = 4;
        let (width, height, pixels) = screen_to_image_scaled(screen, SCALE);
        Image {
            width,
            height,
            pixels,
        }
    }

    pub fn blank() -> Self {
        Self::new(&vec![vec![Pixel::WHITE; 160]; 144])
    }
}

pub struct Emulator {
    gb: EmulatorInner,
}

impl Emulator {
    pub fn new(cart: Vec<u8>) -> Self {
        let gb = Gameboy::load_cartridge(cart);
        Self {
            gb: EmulatorInner::Ready(gb.complete()),
            // gb: EmulatorInner::StartUp(Some(gb)),
        }
    }

    pub fn just_pixels(&self) -> Image {
        Image::new(&self.gb.gb().ppu.screen)
    }

    pub fn gb(&self) -> &Gameboy {
        self.gb.gb()
    }

    pub fn gb_mut(&mut self) -> &mut Gameboy {
        self.gb.gb_mut()
    }

    pub fn next_frame(&mut self) {
        self.gb.next_frame()
    }
}

enum EmulatorInner {
    #[allow(dead_code)]
    StartUp(Option<StartUpSequence>),
    Ready(Gameboy),
}

impl EmulatorInner {
    pub fn gb(&self) -> &Gameboy {
        match self {
            EmulatorInner::StartUp(seq) => seq.as_ref().unwrap(),
            EmulatorInner::Ready(gb) => gb,
        }
    }

    pub fn gb_mut(&mut self) -> &mut Gameboy {
        match self {
            EmulatorInner::StartUp(seq) => seq.as_mut().unwrap(),
            EmulatorInner::Ready(gb) => gb,
        }
    }

    pub fn next_frame(&mut self) {
        match self {
            EmulatorInner::StartUp(seq) => {
                seq.as_mut().unwrap().next_frame();
                if seq.as_ref().unwrap().is_complete() {
                    println!("Completed!!");
                    *self = EmulatorInner::Ready(seq.take().unwrap().complete())
                }
            }
            EmulatorInner::Ready(gb) => {
                if !gb.is_stopped() {
                    gb.next_frame()
                } else {
                    println!("Is stopped!!");
                }
            }
        }
    }
}

fn step_duration(gb: &mut Gameboy, dur: Duration) {
    // We need to know how many instructions to step through. For this, we calculate the number of
    // "dots" (clock cycles) that span the given duration.
    //
    // There are 70224 dots per frame. Calculate the percentage of a frame the duration is and find
    // the number of dots for the duration
    let mut dots = ((70224 * dur.as_micros()) / 17_000) as usize;
    while dots > 0 {
        dots = dots.saturating_sub(gb.step());
    }
}

/// An actor that has an MPSC channel as an input stream and an MPSC as output; however, since the
/// client holds the receiver, the output effectively becomes an SPSC.
pub struct SinkSendActor<T> {
    sender: mpsc::UnboundedSender<T>,
}

impl<A: ActorState, T: 'static + Send> ActorKind<A> for SinkSendActor<T> {
    type Client = SinkSendClient<A::Message, T>;
    type Config = ();

    fn construct(
        (): Self::Config,
    ) -> (Self, Self::Client, impl 'static + FnOnce(&mut Scheduler<A>)) {
        let (send_to_client, recv_at_client) = mpsc::unbounded_channel();
        let (send_to_actor, recv_from_client) = mpsc::unbounded_channel();

        let this = Self {
            sender: send_to_client,
        };

        let client = SinkSendClient {
            send: send_to_actor,
            recv: recv_at_client,
        };

        let init_fn = move |scheduler: &mut Scheduler<A>| {
            scheduler.attach_stream(UnboundedReceiverStream::new(recv_from_client).fuse());
        };
        (this, client, init_fn)
    }
}

impl<T> SinkSendActor<T> {
    pub fn send_message(&self, msg: impl Into<T>) {
        let _ = self.sender.send(msg.into());
    }
}

pub struct SinkSendClient<S, R> {
    send: mpsc::UnboundedSender<S>,
    recv: mpsc::UnboundedReceiver<R>,
}

impl<S, R> SinkSendClient<S, R> {
    pub fn sink_client(&self) -> SinkClient<S> {
        SinkClient {
            send: self.send.clone(),
        }
    }

    pub fn split(self) -> (SinkClient<S>, impl Stream<Item = R>) {
        let Self { send, recv } = self;
        let send = SinkClient { send };
        (send, UnboundedReceiverStream::new(recv))
    }
}

pub struct SinkClient<T> {
    send: mpsc::UnboundedSender<T>,
}

impl<T> SinkClient<T> {
    pub fn send(&self, msg: impl Into<T>) {
        let _ = self.send.send(msg.into());
    }
}
