use iced::widget::Image;
use iced::{Element, widget::image::Handle};

pub struct GuiState {
    image: Handle,
}

#[derive(Debug)]
pub enum GuiMessage {
    Render(Handle),
}

impl GuiState {
    pub fn new() -> Self {
        Self {
            image: Handle::from_bytes(Vec::new()),
        }
    }
    pub fn update(&mut self, msg: GuiMessage) {
        match msg {
            GuiMessage::Render(handle) => self.image = handle,
        }
    }

    pub fn view(&self) -> Element<'_, GuiMessage> {
        Image::new(self.image.clone()).into()
    }
}

impl Default for GuiState {
    fn default() -> Self {
        Self::new()
    }
}
