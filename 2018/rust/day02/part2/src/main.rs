use std::io;
use std::io::prelude::*;

fn main() {
    let input: Vec<String> = io::stdin().lock().lines()
        .map(|line| line.expect("failed to read data"))
        .collect();

    println!("Result: {}", run(&input));
}

fn run<S: ToString>(input: &Vec<S>) -> String {
    for (i, s1) in input.iter().enumerate() {
        for (j, s2) in input.iter().enumerate() {
            if i == j {
                continue
            }
            if diff(s1, s2) == 1 {
                return common(s1, s2);
            }
        }
    }
    panic!("expected boxes not found");
}

fn diff<S: ToString>(s1: &S, s2: &S) -> usize {
    s1.to_string().chars()
        .zip(s2.to_string().chars())
        .filter(|(a, b)| *a != *b).count()
}

fn common<S: ToString>(s1: &S, s2: &S) -> String {
    s1.to_string().chars()
        .zip(s2.to_string().chars())
        .filter(|(a, b)| *a == *b)
        .map(|pair| pair.0)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diff() {
        assert_eq!(diff(&"abcde", &"axcye"), 2);
        assert_eq!(diff(&"fghij", &"fguij"), 1);
    }

    #[test]
    fn test_common() {
        assert_eq!(common(&"fghij", &"fguij"), String::from("fgij"));
    }

    #[test]
    fn test_run() {
        let input = vec![
            "abcde",
            "fghij",
            "klmno",
            "pqrst",
            "fguij",
            "axcye",
            "wvxyz",
        ];
        assert_eq!(run(&input), String::from("fgij"));
    }
}
