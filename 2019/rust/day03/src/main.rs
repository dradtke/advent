use std::collections::HashSet;
use std::io;
use std::io::prelude::*;
use std::iter::FromIterator;
use std::num;

fn main() {
    let lines: Vec<String> = io::stdin().lock().lines()
        .map(|line| line.expect("failed to read data"))
        .collect();

    let wires = run(&lines);
    let intersections = find_intersections(&wires[0], &wires[1]);

    println!("part 1: {}", min_distance(&intersections));
    println!("part 2: {}", min_signal(&intersections, &wires[0], &wires[1]));
}

fn run<T: AsRef<str>>(input: &[T]) -> Vec<Vec<Position>> {
    input.into_iter()
        .map(AsRef::as_ref)
        .map(|line| parse_moves(line).expect("failed to parse moves"))
        .map(walk)
        .collect()
}

// TODO: take an IntoIterator?
fn find_intersections(v1: &Vec<Position>, v2: &Vec<Position>) -> Vec<Position> {
    let s1: HashSet<&Position> = HashSet::from_iter(v1);
    let s2: HashSet<&Position> = HashSet::from_iter(v2);
    s1.intersection(&s2).map(|x| **x).collect()
}

fn min_distance(intersections: &Vec<Position>) -> i32 {
    intersections
        .into_iter()
        .map(|pos| manhattan_distance(*pos))
        .min()
        .expect("failed to grab minimum distance")
}

fn min_signal(intersections: &Vec<Position>, wire1: &Vec<Position>, wire2: &Vec<Position>) -> usize {
    intersections.iter()
        .map(|intersection| {
            let delay1 = signal_delay(wire1, *intersection).expect("failed to get signal delay");
            let delay2 = signal_delay(wire2, *intersection).expect("failed to get signal delay");
            delay1 + delay2
        })
        .min()
        .expect("failed to grab minimum signal")
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
        use Direction::*;
        match c {
            'U' => Some(Up),
            'D' => Some(Down),
            'L' => Some(Left),
            'R' => Some(Right),
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

fn walk(moves: Vec<Move>) -> Vec<Position> {
    let mut x = 0;
    let mut y = 0;
    let mut wire = vec![];

    for (direction, distance) in moves {
        for _ in 0..distance {
            match direction {
                Direction::Up => y -= 1,
                Direction::Down => y += 1,
                Direction::Left => x -= 1,
                Direction::Right => x += 1,
            }
            wire.push((x, y));
        }
    }

    wire
}

fn manhattan_distance(p: Position) -> i32 {
    p.0.abs() + p.1.abs()
}

fn signal_delay(wire: &Vec<Position>, p: Position) -> Option<usize> {
    wire.iter()
        .enumerate()
        .find_map(|(i, pos)| if p == *pos { Some(i+1) } else { None })
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
    fn test_walk() {
        let wire = walk(vec![
            (Right, 8),
            (Up, 5),
            (Left, 5),
            (Down, 3),
        ]);

        let expected = vec![
            (1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (8, 0),
            (8, -1), (8, -2), (8, -3), (8, -4), (8, -5),
            (7, -5), (6, -5), (5, -5), (4, -5), (3, -5),
            (3, -4), (3, -3), (3, -2),
        ];
        assert_eq!(wire, expected);
    }

    #[test]
    fn test_find_intersections() {
        let wire1 = walk(vec![
            (Right, 8),
            (Up, 5),
            (Left, 5),
            (Down, 3),
        ]);
        let wire2 = walk(vec![
            (Up, 7),
            (Right, 6),
            (Down, 4),
            (Left, 4),
        ]);

        let mut result = find_intersections(&wire1, &wire2);
        result.sort_unstable();

        let expected = vec![(3, -3), (6, -5)];

        assert_eq!(result, expected);
    }

    #[test]
    fn test_manhattan_distance() {
        assert_eq!(manhattan_distance((3, 3)), 6);
    }

    #[test]
    fn test_signal_delay() {
        let wire = walk(vec![
            (Right, 8),
            (Up, 5),
            (Left, 5),
            (Down, 3),
        ]);
        assert_eq!(signal_delay(&wire, (6, -5)), Some(15));
        assert_eq!(signal_delay(&wire, (3, -3)), Some(20));
    }

    #[test]
    fn test_min_distance() {
        let results = run(&[
            "R75,D30,R83,U83,L12,D49,R71,U7,L72",
            "U62,R66,U55,R34,D71,R55,D58,R83"
        ]);
        let intersections = find_intersections(&results[0], &results[1]);
        assert_eq!(min_distance(&intersections), 159);

        let results = run(&[
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
        ]);
        let intersections = find_intersections(&results[0], &results[1]);
        assert_eq!(min_distance(&intersections), 135);
    }

    #[test]
    fn test_min_signal() {
        let results = run(&[
            "R75,D30,R83,U83,L12,D49,R71,U7,L72",
            "U62,R66,U55,R34,D71,R55,D58,R83"
        ]);
        let intersections = find_intersections(&results[0], &results[1]);
        assert_eq!(min_signal(&intersections, &results[0], &results[1]), 610);

        let results = run(&[
            "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51",
            "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7",
        ]);
        let intersections = find_intersections(&results[0], &results[1]);
        assert_eq!(min_signal(&intersections, &results[0], &results[1]), 410);
    }
}
