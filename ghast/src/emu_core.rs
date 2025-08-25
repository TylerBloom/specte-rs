use std::time::Duration;

use iced::widget::Image;
use tokio::sync::mpsc::Receiver;
use tokio::sync::mpsc::Sender;
use tokio::sync::mpsc::channel;
use tokio::sync::mpsc::error::TryRecvError;

use crate::state::Emulator;

pub struct EmuHandle {
    send: Sender<EmuMessage>,
    recv: Receiver<Image>,
}

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
            recv: frame_recv,
            send: msg_send,
        }
    }

    pub async fn pause(&self) {
        self.send.send(EmuMessage::Pause).await.unwrap()
    }

    pub async fn resume(&self) {
        self.send.send(EmuMessage::Resume).await.unwrap()
    }

    pub async fn start_game(&self, game: Vec<u8>) {
        self.send.send(EmuMessage::Start(game)).await.unwrap()
    }
}

/// This is the core of the emulator state. It is interfaced with via the `EmuHandle`. It is
/// intended that the core is ran in a seperate thread/task from the main core.
struct EmuCore {
    recv: Receiver<EmuMessage>,
    send: Sender<Image>,
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
        let mut is_paused = true;
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
                send.send(emu.just_image()).await;
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
