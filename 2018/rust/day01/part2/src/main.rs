use std::collections::HashSet;
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
    let mut freq = 0;
    let mut seen = HashSet::new();

    seen.insert(freq);

    loop {
        for (sign, value) in values.iter().map(AsRef::as_ref).map(parse) {
            freq += (sign as i32) * value;
            if seen.contains(&freq) {
                return freq;
            }
            seen.insert(freq);
        }
    }
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
    fn test_run() {
        assert_eq!(run(&["+1", "-2", "+3", "+1", "+1", "-2"]), 2);
        assert_eq!(run(&["+1", "-1"]), 0);
        assert_eq!(run(&["+3", "+3", "+4", "-2", "-4"]), 10);
        assert_eq!(run(&["-6", "+3", "+8", "+5", "-6"]), 5);
        assert_eq!(run(&["+7", "+7", "-2", "-7", "-4"]), 14);
    }
}
