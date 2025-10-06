use std::borrow::Cow;
use std::pin::Pin;
use std::task::Context;
use std::task::Poll;

use iced::advanced::image::Handle;
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
use tokio_stream::Stream;
use tokio_stream::wrappers::ReceiverStream;

use crate::utils::screen_to_image_scaled;

pub struct EmuHandle {
    send: EmuSend,
    recv: EmuRecv,
}

pub struct EmuSend(UnboundedSender<EmuMessage>);

pub struct EmuRecv(Receiver<Handle>);

pub struct EmuStream(Pin<Box<ReceiverStream<Handle>>>);

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

    pub fn pause(&self) {
        self.send.pause()
    }

    pub fn resume(&self) {
        self.send.resume()
    }

    pub fn start_game(&self, game: Vec<u8>) {
        self.send.start_game(game)
    }

    pub async fn next_frame(&mut self) -> Handle {
        self.recv.next_frame().await
    }
}

impl EmuSend {
    pub fn pause(&self) {
        self.0.send(EmuMessage::Pause).unwrap()
    }

    pub fn resume(&self) {
        self.0.send(EmuMessage::Resume).unwrap()
    }

    pub fn start_game(&self, game: Vec<u8>) {
        // self.0.send(EmuMessage::Start(game)).await.unwrap()
        self.0.send(EmuMessage::Start(game)).unwrap()
    }
}

impl EmuRecv {
    pub async fn next_frame(&mut self) -> Handle {
        self.0.recv().await.unwrap()
    }

    pub fn into_stream(self) -> EmuStream {
        EmuStream(Box::pin(ReceiverStream::new(self.0)))
    }
}

impl Stream for EmuStream {
    type Item = Handle;

    fn poll_next(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        Pin::get_mut(self).0.as_mut().poll_next(cx)
    }
}

/// This is the core of the emulator state. It is interfaced with via the `EmuHandle`. It is
/// intended that the core is ran in a seperate thread/task from the main core.
struct EmuCore {
    recv: UnboundedReceiver<EmuMessage>,
    send: Sender<Handle>,
}

enum EmuMessage {
    Start(Vec<u8>),
    Pause,
    Resume,
}

impl EmuCore {
    async fn run(mut self) {
        let mut emu = self.wait_for_cart().await;
        let Self { mut recv, send } = self;
        let mut is_paused = false;
        // 1/60 of a second is ~17 msec
        let mut timer = tokio::time::interval(tokio::time::Duration::from_millis(17));
        // Initially, this is a very basic cycle. We will always wait 17 ms, check for messages,
        // then, if not paused, calculate the next frame and send it off to the handle.
        loop {
            timer.tick().await;
            loop {
                match recv.try_recv() {
                    Ok(msg) => match msg {
                        EmuMessage::Start(cart) => {
                            is_paused = false;
                            emu = Emulator::new(cart);
                        }
                        EmuMessage::Pause => is_paused = true,
                        EmuMessage::Resume => is_paused = false,
                    },
                    Err(TryRecvError::Empty) => break,
                    // There is nothing to do while the handle has hung up. Data can neither be
                    // sent or recv-ed.
                    // TODO: This needs to be a cleaner shutdown to prevent data loss.
                    Err(TryRecvError::Disconnected) => panic!("Handle hung up"),
                }
            }
            if !is_paused {
                emu.next_frame();
                send.send(emu.just_pixels()).await.unwrap();
            }
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
        let gb = Gameboy::new(cart);
        Self {
            gb: EmulatorInner::StartUp(Some(gb)),
        }
    }

    pub fn just_pixels(&self) -> Handle {
        create_image(&self.gb.gb().ppu.screen)
    }

    pub fn gb(&self) -> &Gameboy {
        self.gb.gb()
    }

    pub fn next_frame(&mut self) {
        self.gb.next_frame()
    }
}
