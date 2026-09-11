use std::time::Duration;

use futures::stream::StreamExt;
use instant::Instant;
use spirit::Gameboy;
use spirit::StartUpSequence;
use spirit::ppu::Pixel;
use troupe::ActorState;
use troupe::Scheduler;
use troupe::compat::sleep_for;
use troupe::joint::JointActor;

use crate::keys::ButtonInteration;
use crate::keys::ControlSignal;
use crate::keys::Keystroke;
use crate::utils::screen_to_image_scaled;

/// This is the core of the emulator state. It is interfaced with via the `EmuHandle`. It is
/// intended that the core is ran in a seperate thread/task from the main core.
pub struct EmuCore {
    is_paused: bool,
    last_updated: Instant,
    frames: usize,
    emulator: Option<Emulator>,
}

impl EmuCore {
    pub fn new() -> Self {
        Self {
            is_paused: false,
            last_updated: Instant::now(),
            frames: 0,
            emulator: None,
        }
    }
}

impl Default for EmuCore {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, derive_more::From)]
pub enum EmuMessage {
    Start(Vec<u8>),
    Keystroke(Keystroke),
    NextFrame,
}

#[derive(Debug, Clone)]
pub enum EmuOutput {
    Frame(Frame),
    /// A serialized snapshot of the emulator's state.
    Snapshot(String, Vec<u8>),
    /// The saved ROM (+ RAM) state.
    SaveState(String, Vec<u8>),
}

impl ActorState for EmuCore {
    type ActorKind = JointActor<EmuOutput>;
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
        match self.emulator.as_mut() {
            None => match msg {
                EmuMessage::Start(rom) => self.emulator = Some(Emulator::new(rom)),
                EmuMessage::Keystroke(_) | EmuMessage::NextFrame => {}
            },
            Some(emu) => {
                match msg {
                    EmuMessage::NextFrame => {
                        if !self.is_paused {
                            emu.next_frame();
                            self.frames += 1;
                            scheduler.broadcast(EmuOutput::Frame((emu.just_pixels(), self.frames)));
                        } else {
                            return;
                        }
                    }
                    EmuMessage::Start(cart) => {
                        self.frames = 0;
                        self.is_paused = false;
                        *emu = Emulator::new(cart);
                    }
                    EmuMessage::Keystroke(Keystroke::Control(ControlSignal::Pause)) => {
                        self.is_paused = !self.is_paused;
                        return;
                    }
                    EmuMessage::Keystroke(Keystroke::Control(ControlSignal::NextFrame)) => {
                        self.is_paused = true;
                        emu.next_frame();
                        self.frames += 1;
                        scheduler.broadcast(EmuOutput::Frame((emu.just_pixels(), self.frames)));
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
        }
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
