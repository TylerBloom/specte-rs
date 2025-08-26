use std::pin::Pin;
use std::task::Context;
use std::task::Poll;
use std::time::Duration;

use iced::advanced::image::Handle;
use iced::futures::StreamExt;
use iced::widget::Image;
use tokio::sync::mpsc::Receiver;
use tokio::sync::mpsc::Sender;
use tokio::sync::mpsc::channel;
use tokio::sync::mpsc::error::TryRecvError;
use tokio_stream::Stream;
use tokio_stream::wrappers::ReceiverStream;

use crate::state::Emulator;

pub struct EmuHandle {
    send: EmuSend,
    recv: EmuRecv,
}

pub struct EmuSend(Sender<EmuMessage>);

pub struct EmuRecv(Receiver<Handle>);

pub struct EmuStream(Pin<Box<ReceiverStream<Handle>>>);

impl EmuHandle {
    // TODO: This will need a trove handle to pull in setting and configs
    /// This contructs the Emulator core and launches it into a seperate task, returning the handle
    /// in order to interface with it.
    pub fn contruct_and_launch() -> Self {
        let (msg_send, msg_recv) = channel(100);
        let (frame_send, frame_recv) = channel(100);
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

    pub async fn pause(&self) {
        self.send.pause().await
    }

    pub async fn resume(&self) {
        self.send.resume().await
    }

    pub fn start_game(&self, game: Vec<u8>) {
        self.send.start_game(game)
    }

    pub async fn next_frame(&mut self) -> Handle {
        self.recv.next_frame().await
    }
}

impl EmuSend {
    pub async fn pause(&self) {
        self.0.send(EmuMessage::Pause).await.unwrap()
    }

    pub async fn resume(&self) {
        self.0.send(EmuMessage::Resume).await.unwrap()
    }

    pub fn start_game(&self, game: Vec<u8>) {
        // self.0.send(EmuMessage::Start(game)).await.unwrap()
        self.0.try_send(EmuMessage::Start(game)).unwrap()
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
    recv: Receiver<EmuMessage>,
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
        let Self { mut recv, mut send } = self;
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
                        EmuMessage::Start(data) => {
                            is_paused = false;
                            todo!()
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
                emu.next_screen();
                send.send(emu.just_pixels()).await;
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
