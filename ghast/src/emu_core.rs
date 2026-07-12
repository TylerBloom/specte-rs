use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use iced::advanced::image::Handle;
use spirit::ButtonInput;
use spirit::Gameboy;
use spirit::StartUpSequence;
use spirit::ppu::Pixel;
use tokio::sync::mpsc::Receiver;
use tokio::sync::mpsc::Sender;
use tokio::sync::mpsc::UnboundedReceiver;
use tokio::sync::mpsc::UnboundedSender;
use tokio::sync::mpsc::channel;
use tokio::sync::mpsc::error::TryRecvError;
use tokio::sync::mpsc::unbounded_channel;
use tokio::time::Duration;
use tokio::time::Instant;
use tokio::time::interval;
use tokio_stream::Stream;
use tokio_stream::wrappers::ReceiverStream;

use crate::keys::ButtonInteration;
use crate::keys::ControlSignal;
use crate::keys::Keystroke;
use crate::utils::screen_to_image_scaled;

pub struct EmuHandle {
    send: EmuSend,
    recv: EmuRecv,
}

pub struct EmuSend(UnboundedSender<EmuMessage>);

pub struct EmuRecv(Receiver<(Handle, usize)>);

pub struct EmuStream(Pin<Box<ReceiverStream<(Handle, usize)>>>);

impl EmuHandle {
    // TODO: This will need a trove handle to pull in setting and configs
    /// This contructs the Emulator core and launches it into a seperate task, returning the handle
    /// in order to interface with it.
    pub fn contruct_and_launch() -> Self {
        let (msg_send, msg_recv) = unbounded_channel();
        let (frame_send, frame_recv) = channel(10);
        let core = EmuCore {
            recv: msg_recv,
            send: frame_send,
            frame_send: EmuSend(msg_send.clone()),
        };
        tokio::task::spawn(core.run());
        Self {
            recv: EmuRecv(frame_recv),
            send: EmuSend(msg_send),
        }
    }

    pub fn split(self) -> (EmuSend, EmuRecv) {
        let Self { send, recv } = self;
        (send, recv)
    }

    pub fn start_game(&self, game: Vec<u8>) {
        self.send.start_game(game)
    }

    pub async fn next_frame(&mut self) -> (Handle, usize) {
        self.recv.next_frame().await
    }
}

impl EmuSend {
    pub fn keystroke(&self, key: Keystroke) {
        self.0.send(EmuMessage::Keystroke(key)).unwrap()
    }

    pub fn pause(&self) {
        self.keystroke(Keystroke::Control(ControlSignal::Pause));
    }

    pub fn start_game(&self, game: Vec<u8>) {
        self.0.send(EmuMessage::Start(game)).unwrap()
    }

    fn next_frame(&self) {
        self.0.send(EmuMessage::NextFrame).unwrap()
    }
}

impl EmuRecv {
    pub async fn next_frame(&mut self) -> (Handle, usize) {
        self.0.recv().await.unwrap()
    }

    pub fn into_stream(self) -> EmuStream {
        EmuStream(Box::pin(ReceiverStream::new(self.0)))
    }
}

impl Stream for EmuStream {
    type Item = (Handle, usize);

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        Pin::get_mut(self).0.as_mut().poll_next(cx)
    }
}

/// This is the core of the emulator state. It is interfaced with via the `EmuHandle`. It is
/// intended that the core is ran in a seperate thread/task from the main core.
struct EmuCore {
    recv: UnboundedReceiver<EmuMessage>,
    frame_send: EmuSend,
    send: Sender<(Handle, usize)>,
}

enum EmuMessage {
    Start(Vec<u8>),
    Keystroke(Keystroke),
    NextFrame,
}

impl EmuCore {
    async fn run(mut self) {
        let mut emu = self.wait_for_cart().await;
        let Self {
            mut recv,
            send,
            frame_send,
        } = self;
        tokio::spawn(async move {
            let mut timer = interval(tokio::time::Duration::from_secs(1) / 60);
            timer.tick().await;
            loop {
                timer.tick().await;
                frame_send.next_frame();
            }
        });
        let mut is_paused = false;
        let mut frames = 0;

        let mut last_updated = Instant::now();
        loop {
            let Some(msg) = recv.recv().await else {
                // There is nothing to do while the handle has hung up. Data can neither be
                // sent or recv-ed.
                // TODO: This needs to be a cleaner shutdown to prevent data loss.
                panic!("Handle hung up")
            };
            match msg {
                EmuMessage::NextFrame => {
                    if !is_paused {
                        emu.next_frame();
                        frames += 1;
                        send.send((emu.just_pixels(), frames)).await.unwrap();
                    } else {
                        continue;
                    }
                }
                EmuMessage::Start(cart) => {
                    frames = 0;
                    is_paused = false;
                    emu = Emulator::new(cart);
                }
                EmuMessage::Keystroke(Keystroke::Control(ControlSignal::Pause)) => {
                    is_paused = !is_paused;
                    continue;
                }
                EmuMessage::Keystroke(Keystroke::Control(ControlSignal::NextFrame)) => {
                    is_paused = true;
                    emu.next_frame();
                    frames += 1;
                    send.send((emu.just_pixels(), frames)).await.unwrap();
                }
                EmuMessage::Keystroke(Keystroke::Button(button)) => match button {
                    ButtonInteration::ButtonPress(button) => {
                        last_updated = Instant::now();
                        step_duration(emu.gb_mut(), last_updated - last_updated);
                        emu.gb_mut().button_press(button);
                        continue
                    }
                    ButtonInteration::ButtonRelease(button) => {
                        last_updated = Instant::now();
                        step_duration(emu.gb_mut(), last_updated - last_updated);
                        emu.gb_mut().button_release(button);
                        continue
                    }
                },
            }
            last_updated = Instant::now()
        }
    }

    async fn wait_for_cart(&mut self) -> Emulator {
        let cart = loop {
            if let EmuMessage::Start(cart) = self.recv.recv().await.unwrap() {
                break cart;
            }
        };
        Emulator::new(cart)
    }
}

pub struct Emulator {
    gb: EmulatorInner,
}

#[allow(dead_code)]
enum EmulatorInner {
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

#[allow(clippy::ptr_arg)]
pub fn create_image(screen: &Vec<Vec<Pixel>>) -> Handle {
    const SCALE: usize = 4;
    let (width, height, image) = screen_to_image_scaled(screen, SCALE);
    assert_eq!(width * height * 4, image.len() as u32);
    Handle::from_rgba(width, height, image)
}

impl Emulator {
    pub fn new(cart: Vec<u8>) -> Self {
        let gb = Gameboy::load_cartridge(cart);
        Self {
            gb: EmulatorInner::Ready(gb.complete()),
            // gb: EmulatorInner::StartUp(Some(gb)),
        }
    }

    pub fn just_pixels(&self) -> Handle {
        create_image(&self.gb.gb().ppu.screen)
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
