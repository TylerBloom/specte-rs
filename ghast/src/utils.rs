use spirit::ppu::Pixel;

/// Given the emulator's screen, this function returns the data for creating an image.
pub fn screen_to_image(screen: &[Vec<Pixel>]) -> (u32, u32, Vec<u8>) {
    screen_to_image_scaled(screen, 1)
}

/// Returns an scaled up version of the emulator's screen
pub fn screen_to_image_scaled(screen: &[impl AsRef<[Pixel]>], scale: usize) -> (u32, u32, Vec<u8>) {
    let width = screen[0].as_ref().len();
    let height = screen.len();
    let mut digest = vec![0; width * scale * 4 * height * scale];
    for offset in 0..scale {
        for (i, row) in screen.iter().enumerate() {
            let index = (i * scale + offset) * (4 * width * scale);
            row.as_ref()
                .iter()
                .copied()
                .flat_map(|pixel| std::iter::repeat_n(pixel, scale))
                .flat_map(pixel_to_bytes)
                .enumerate()
                .for_each(|(j, b)| digest[index + j] = b);
        }
    }
    ((width * scale) as u32, (height * scale) as u32, digest)
}

/// Scales an image by a set factor, preserving the original ratio.
pub fn scale_up_image(image: &[u8], height: usize, width: usize, scale: usize) -> Vec<u8> {
    assert_eq!(image.len(), 4 * height * width);
    let true_width = 4 * width;
    (0..height)
        .map(|line| &image[(line * true_width)..((line + 1) * true_width)])
        .flat_map(|line| std::iter::repeat_n(line, scale))
        .flat_map(|line| {
            (0..true_width)
                .step_by(4)
                .map(|pixel| &line[pixel..pixel + 4])
        })
        .flat_map(|pixel| std::iter::repeat_n(pixel, scale))
        .flatten()
        .copied()
        .collect()
}

pub fn pixel_to_bytes(Pixel { r, g, b }: Pixel) -> [u8; 4] {
    [r * 8, g * 8, b * 8, 255]
}
