use ghast::state::Emulator;

pub fn main() -> iced::Result {
    iced::application("Specters - Ghast GBC", Emulator::update, Emulator::view)
        .subscription(Emulator::subscription)
        .run()
}
