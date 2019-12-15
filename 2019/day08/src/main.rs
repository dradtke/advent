use std::io::prelude::*;

type Layer = Vec<Vec<u8>>;
type Image = Vec<Layer>;

fn main() -> std::io::Result<()> {
    let mut input = String::new();
    std::io::stdin().lock().read_to_string(&mut input)?;

    let image = parse_image(25, 6, input.chars().flat_map(parse_char));
    let fewest_zeros = image.iter()
        .map(|layer| count_digits(layer, 0))
        .enumerate()
        .min_by_key(|(_, total)| *total)
        .unwrap().0;

    println!("part 1: {}", count_digits(&image[fewest_zeros], 1) * count_digits(&image[fewest_zeros], 2));

    let collapsed_image = collapse_image(25, 6, &image);
    println!("part 2:");
    for row in collapsed_image {
        println!("{}", row.iter().map(|pixel| draw_pixel(*pixel)).collect::<Vec<String>>().join(""));
    }

    Ok(())
}

fn parse_image<I: Iterator<Item=u8>>(width: usize, height: usize, data: I) -> Image {
    let pixels_per_layer = width*height;

    let mut image = Vec::new();
    for (i, pixel) in data.into_iter().enumerate() {
        let layer_index = i/pixels_per_layer;
        if layer_index == image.len() {
            let layer: Layer = (0..height).map(|_| new_row(width)).collect();
            image.push(layer);
        }
        let pixel_index = i%pixels_per_layer;
        let pixel_y = pixel_index/width;
        let pixel_x = pixel_index%width;
        image[layer_index][pixel_y][pixel_x] = pixel;
    }
    image
}

fn new_row(width: usize) -> Vec<u8> {
    let mut row = Vec::with_capacity(width);
    row.resize(width, 0);
    row
}

fn parse_char(c: char) -> Option<u8> {
    c.to_digit(10).map(|x| x as u8)
}

fn count_digits(layer: &Layer, digit: u8) -> usize {
    layer.iter().fold(0, |acc, row| acc + row.iter().filter(|pixel| **pixel == digit).count())
}

fn collapse_image(width: usize, height: usize, image: &Image) -> Layer {
    let mut collapsed_layer: Vec<Vec<Option<u8>>> = (0..height).map(|_| {
        let mut row = Vec::with_capacity(width);
        row.resize(width, None);
        row
    }).collect();

    for layer in image {
        for (y, row) in layer.iter().enumerate() {
            for (x, pixel) in row.iter().enumerate() {
                if collapsed_layer[y][x].is_some() || *pixel == 2 {
                    continue;
                }
                collapsed_layer[y][x] = Some(*pixel);
            }
        }
    }

    collapsed_layer.into_iter().map(|row| {
        row.into_iter().map(|pixel| pixel.unwrap()).collect()
    }).collect()
}

fn draw_pixel(pixel: u8) -> String {
    String::from(if pixel == 0 { "█" } else { " " })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_image() {
        let data = vec![1,2,3,4,5,6,7,8,9,0,1,2].into_iter();
        assert_eq!(parse_image(3, 2, data), vec![
            [[1,2,3],[4,5,6]],
            [[7,8,9],[0,1,2]],
        ]);
    }

    #[test]
    fn test_count_digits() {
        let layer = vec![vec![1,1,1,3],vec![2,2,3,3]];
        assert_eq!(count_digits(&layer, 1), 3);
        assert_eq!(count_digits(&layer, 2), 2);
        assert_eq!(count_digits(&layer, 3), 3);
    }

    #[test]
    fn test_parse_char() {
        assert_eq!(parse_char('0'), Some(0));
        assert_eq!(parse_char('1'), Some(1));
        assert_eq!(parse_char('2'), Some(2));
        assert_eq!(parse_char('3'), Some(3));
        assert_eq!(parse_char('4'), Some(4));
        assert_eq!(parse_char('5'), Some(5));
        assert_eq!(parse_char('6'), Some(6));
        assert_eq!(parse_char('7'), Some(7));
        assert_eq!(parse_char('8'), Some(8));
        assert_eq!(parse_char('9'), Some(9));
    }

    #[test]
    fn test_collapse_image() {
        let image = vec![
            vec![vec![0,2],vec![2,2]],
            vec![vec![1,1],vec![2,2]],
            vec![vec![2,2],vec![1,2]],
            vec![vec![0,0],vec![0,0]],
        ];
        assert_eq!(collapse_image(2, 2, &image), vec![vec![0,1],vec![1,0]]);
    }
}
