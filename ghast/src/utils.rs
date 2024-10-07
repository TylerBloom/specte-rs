use spirit::ppu::Pixel;

/// Given the emulator's screen, this function returns the data for creating an image.
pub fn screen_to_image(screen: &[Vec<Pixel>]) -> (u32, u32, Vec<u8>) {
    screen_to_image_scaled(screen, 1)
}

const SCREEN_WIDTH: usize = 160;
const SCREEN_HEIGHT: usize = 144;

/// Returns an scaled up version of the emulator's screen
pub fn screen_to_image_scaled(screen: &[Vec<Pixel>], scale: usize) -> (u32, u32, Vec<u8>) {
    let mut digest = vec![0; SCREEN_WIDTH * scale * 4 * SCREEN_HEIGHT * scale];
    for offset in 0..scale {
        for (i, row) in screen.iter().enumerate() {
            let index = ((i * scale + offset)  * (4 * SCREEN_WIDTH * scale));
            row.iter()
                .copied()
                .flat_map(|pixel| std::iter::repeat(pixel).take(scale))
                .flat_map(pixel_to_bytes)
                .enumerate()
                .for_each(|(j, b)| digest[index + j] = b);
        }
    }
    (
        (SCREEN_WIDTH * scale) as u32,
        (SCREEN_HEIGHT * scale) as u32,
        digest,
    )
}

fn fill_enlarge_pixel_area(x: usize, y: usize, pixel: Pixel, buffer: &mut [u8]) {
    let [r, g, b, a] = pixel_to_bytes(pixel);
}

/// Scales an image by a set factor, preserving the original ratio.
pub fn scale_up_image(image: &[u8], height: usize, width: usize, scale: usize) -> Vec<u8> {
    assert_eq!(image.len(), 4 * height * width);
    let true_width = 4 * width;
    (0..height)
        .map(|line| &image[(line * true_width)..((line + 1) * true_width)])
        .flat_map(|line| std::iter::repeat(line).take(scale))
        .flat_map(|line| {
            (0..true_width)
                .step_by(4)
                .map(|pixel| &line[pixel..pixel + 4])
        })
        .flat_map(|pixel| std::iter::repeat(pixel).take(scale))
        .flatten()
        .copied()
        .collect()
}

pub fn pixel_to_bytes(Pixel { r, g, b }: Pixel) -> [u8; 4] {
    [r * 8, g * 8, b * 8, 255]
}
