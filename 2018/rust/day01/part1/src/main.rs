use std::io;
use std::io::prelude::*;

type Operation = (i8, i32);

fn main() {
    let input: Vec<String> = io::stdin().lock().lines()
        .map(|line| line.expect("failed to read data"))
        .collect();

    println!("Result: {}", run(&input));
}

fn run<T: AsRef<str>>(values: &[T]) -> i32 {
    values.iter()
        .map(AsRef::as_ref)
        .map(parse)
        .map(|(sign, value)| (sign as i32) * value)
        .sum()
}

fn parse(line: &str) -> Operation {
    let mut chars = line.chars();
    let sign = match chars.next() {
        Some('+') => 1,
        Some('-') => -1,
        Some(x) => panic!("unexpected first character: {:?}", x),
        None => panic!("unexpected EOF"),
    };
    let n = chars.collect::<String>().parse::<i32>().expect("failed to parse number");
    (sign, n)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(parse("+3"), (1, 3));
        assert_eq!(parse("-2"), (-1, 2));
    }

    #[test]
    fn test_run() {
        assert_eq!(run(&["+1", "-1"]), 0);
        assert_eq!(run(&["+1", "-2", "+3", "+1"]), 3);
        assert_eq!(run(&["+1", "+1", "+1"]), 3);
        assert_eq!(run(&["+1", "+1", "-2"]), 0);
        assert_eq!(run(&["-1", "-2", "-3"]), -6);
    }
}
