use std::num;

fn main() {
    // TODO: need to create the grid, parse input, and draw the wires
}

#[derive(Debug, PartialEq)]
enum Direction {
    Up,
    Down,
    Left,
    Right
}

impl Direction {
    fn from(c: char) -> Option<Direction> {
        match c {
            'U' => Some(Direction::Up),
            'D' => Some(Direction::Down),
            'L' => Some(Direction::Left),
            'R' => Some(Direction::Right),
            _ => None,
        }
    }
}

type Move = (Direction, u32);

#[derive(Debug, PartialEq)]
enum ParseError {
    NoDirectionChar,
    BadDirectionChar(char),
    BadNumber(num::ParseIntError),
}

impl From<num::ParseIntError> for ParseError {
    fn from(e: num::ParseIntError) -> ParseError {
        ParseError::BadNumber(e)
    }
}

fn parse_move<T: AsRef<str>>(value: T) -> Result<Move, ParseError> {
    let value = value.as_ref();
    let mut chars = value.chars();

    let direction_char = chars.next().ok_or(ParseError::NoDirectionChar)?;
    let direction = Direction::from(direction_char).ok_or(ParseError::BadDirectionChar(direction_char))?;

    Ok((direction, chars.collect::<String>().parse()?))
}

fn parse_moves<T: AsRef<str>>(values: T) -> Result<Vec<Move>, ParseError> {
    values.as_ref().split(',').map(parse_move).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_move() {
        assert_eq!(parse_move("U3"), Ok((Direction::Up, 3)));
        assert_eq!(parse_move("D1"), Ok((Direction::Down, 1)));
        assert_eq!(parse_move("R2"), Ok((Direction::Right, 2)));
        assert_eq!(parse_move("L4"), Ok((Direction::Left, 4)));
    }

    #[test]
    fn test_parse_moves() {
        let expected = vec![
            (Direction::Up, 3),
            (Direction::Down, 1),
            (Direction::Right, 2),
            (Direction::Left, 4),
        ];
        assert_eq!(parse_moves("U3,D1,R2,L4"), Ok(expected));
    }
}
