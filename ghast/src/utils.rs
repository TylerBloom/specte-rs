use std::fmt::Debug;

use spirit::ppu::Pixel;
use tokio::sync::mpsc::UnboundedReceiver;
use xilem_core::MessageProxy;

/// Given the emulator's screen, this function returns the data for creating an image.
pub fn screen_to_image(screen: &[Vec<Pixel>]) -> (u32, u32, Vec<u8>) {
    screen_to_image_scaled(screen, 1)
}

/// Returns an scaled up version of the emulator's screen
pub fn screen_to_image_scaled(screen: &[impl AsRef<[Pixel]>], scale: usize) -> (u32, u32, Vec<u8>) {
    screen_to_image_scaled_asymmetric(screen, scale, scale)
}

/// Scales an image by a set factor, preserving the original ratio.
pub fn screen_to_image_scaled_asymmetric(
    screen: &[impl AsRef<[Pixel]>],
    h_scale: usize,
    w_scale: usize,
) -> (u32, u32, Vec<u8>) {
    let width = screen[0].as_ref().len();
    let height = screen.len();
    let mut digest = vec![0; 4 * width * w_scale * height * h_scale];

    let line_width = 4 * width * w_scale;

    for (i, row) in screen.iter().enumerate() {
        let index = i * h_scale * line_width;
        let (_, after) = digest.split_at_mut(index);

        // There are h_scale rows in the final image for each row in the original.
        // Populate the first upscaled row.
        let (row_slice, after) = after.split_at_mut(line_width);

        for (j, pixel) in row.as_ref().iter().enumerate() {
            let index = j * 4 * h_scale;
            for x_offset in 0..h_scale {
                let index = index + (4 * x_offset);
                let [r, g, b, a] = pixel_to_bytes(*pixel);
                row_slice[index] = r;
                row_slice[index + 1] = g;
                row_slice[index + 2] = b;
                row_slice[index + 3] = a;
            }
        }

        // Now, copy the populated row into the subsequent h_scale - 1 rows.
        // Internally, `copy_from_slice` uses `memcopy`, which is orders of magnitude faster than a
        // hand-rolled equiv (in debug mode).
        for i in 1..h_scale {
            let next_row_slice = &mut after[((i - 1) * line_width)..(i * line_width)];
            next_row_slice.copy_from_slice(row_slice);
        }
    }

    ((width * w_scale) as u32, (height * h_scale) as u32, digest)
}

pub fn pixel_to_bytes(Pixel { r, g, b }: Pixel) -> [u8; 4] {
    [r * 8, g * 8, b * 8, 255]
}

/// Used in combination with Xilem `worker`s. The `proxy` "wakes up" the
/// UI state via a message. The `recv` is being used to simply relay the message from another task
/// to the event loop's `proxy`.
pub async fn identity_proxy<T: 'static + Debug + Send>(
    proxy: MessageProxy<T>,
    mut recv: UnboundedReceiver<T>,
) {
    loop {
        let msg = recv.recv().await.unwrap();
        proxy.message(msg).unwrap();
    }
}
