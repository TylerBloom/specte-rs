use ghast::config::Config;
use ghast::emu_core::EmuHandle;
use ghast::state::UiState;
use tokio::sync::mpsc::unbounded_channel;
use xilem_web::App;
use xilem_web::document_body;

pub fn main() {
    let conf = Config::read();
    let (send, recv) = EmuHandle::contruct_and_launch().split();

    // Keyboard input isn't wired up for the WASM target yet, so this receiver currently never
    // gets anything sent to it.
    let (_key_proxy_send, key_proxy_recv) = unbounded_channel();

    let state = UiState::new(conf, send, recv, key_proxy_recv);

    App::new(document_body(), state, UiState::app_logic).run();
}
