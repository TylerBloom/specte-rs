

/// Scales an image by a set factor, preserving the original ratio.
fn scale_up_image(image: &[u8], height: usize, width: usize, scale: usize) -> Vec<u8> {
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
