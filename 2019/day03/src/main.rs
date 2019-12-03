use std::collections::HashSet;
use std::io;
use std::io::prelude::*;
use std::num;

fn main() {
    let input: Vec<Vec<Move>> = io::stdin().lock().lines()
        .map(|line| line.expect("failed to read data"))
        .map(parse_moves)
        .map(|value| value.expect("failed to parse move"))
        .collect();

    let wires: Vec<HashSet<Position>> = input.into_iter()
        .map(run)
        .collect();

    let result = wires[0].intersection(&(wires[1]))
        .map(|pos| manhattan_distance(*pos))
        .min()
        .expect("failed to grab minimum value");

    println!("part 1: {}", result);
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
type Position = (i32, i32);

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

fn run(wire: Vec<Move>) -> HashSet<Position> {
    let mut x = 0;
    let mut y = 0;
    let mut result = HashSet::new();

    for (direction, distance) in wire {
        for _ in 0..distance {
            match direction {
                Direction::Up => y -= 1,
                Direction::Down => y += 1,
                Direction::Left => x -= 1,
                Direction::Right => x += 1,
            }
            result.insert((x, y));
        }
    }

    result
}

fn manhattan_distance(p: Position) -> i32 {
    p.0.abs() + p.1.abs()
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::Direction::*;

    #[test]
    fn test_parse_move() {
        assert_eq!(parse_move("U3"), Ok((Up, 3)));
        assert_eq!(parse_move("D1"), Ok((Down, 1)));
        assert_eq!(parse_move("R2"), Ok((Right, 2)));
        assert_eq!(parse_move("L4"), Ok((Left, 4)));
    }

    #[test]
    fn test_parse_moves() {
        let expected = vec![
            (Up, 3),
            (Down, 1),
            (Right, 2),
            (Left, 4),
        ];
        assert_eq!(parse_moves("U3,D1,R2,L4"), Ok(expected));
    }

    #[test]
    fn test_run() {
        let result = run(vec![
            (Right, 8),
            (Up, 5),
            (Left, 5),
            (Down, 3),
        ]);

        let expected: HashSet<Position> = [
            (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (8, 0),
            (8, -1), (8, -2), (8, -3), (8, -4), (8, -5),
            (7, -5), (6, -5), (5, -5), (4, -5), (3, -5),
            (3, -4), (3, -3), (3, -2),
        ].iter().cloned().collect();
        assert_eq!(result, expected);
    }

    #[test]
    fn test_manhattan_distance() {
        assert_eq!(manhattan_distance((3, 3)), 6);
    }
}
