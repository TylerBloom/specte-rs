use spirit::ppu::Pixel;

/// Given the emulator's screen, this function returns the data for creating an image.
pub fn screen_to_image(screen: &[Vec<Pixel>]) -> (u32, u32, Vec<u8>) {
    screen_to_image_scaled(screen, 1)
}

/// Returns an scaled up version of the emulator's screen
pub fn screen_to_image_scaled(screen: &[impl AsRef<[Pixel]>], scale: usize) -> (u32, u32, Vec<u8>) {
    let width = screen[0].as_ref().len();
    let height = screen.len();
    let mut digest = vec![0; 4 * width * scale * height * scale];

    let line_width = 4 * width * scale;

    for (i, row) in screen.iter().enumerate() {
        let index = i * scale * line_width;
        let (_, after) = digest.split_at_mut(index);
        let (row_slice, after) = after.split_at_mut(line_width);

        for (j, pixel) in row.as_ref().iter().enumerate() {
            let index = j * 4 * scale;
            for x_offset in 0..scale {
                let index = index + (4 * x_offset);
                let [r, g, b, a] = pixel_to_bytes(*pixel);
                row_slice[index] = r;
                row_slice[index + 1] = g;
                row_slice[index + 2] = b;
                row_slice[index + 3] = a;
            }
        }

        // copy row_slice into subsequent the slices scale - 1 times
        for i in 1..scale {
            let next_row_slice = &mut after[((i - 1) * line_width)..(i * line_width)];
            next_row_slice.copy_from_slice(row_slice);
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
