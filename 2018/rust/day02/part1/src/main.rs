use std::collections::HashMap;
use std::io;
use std::io::prelude::*;

fn main() {
    let input: Vec<String> = io::stdin().lock().lines()
        .map(|line| line.expect("failed to read data"))
        .collect();

    println!("Result: {}", run(&input, 2) * run(&input, 3));
}

fn run<S: ToString>(input: &Vec<S>, n: i8) -> usize {
    input.iter()
        .map(count_chars)
        .map(|counts| has_n_of(counts, n))
        .filter(|x| *x)
        .count()
}

fn count_chars<S: ToString>(s: &S) -> HashMap<char, i8> {
    let mut counts = HashMap::new();
    for c in s.to_string().chars() {
        counts.entry(c).and_modify(|n| *n += 1).or_insert(1);
    }
    counts
}

fn has_n_of<K>(counts: HashMap<K, i8>, n: i8) -> bool {
    counts.values().any(|&x| x == n)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run() {
        let input = vec![
            "abcdef",
            "bababc",
            "abbcde",
            "abcccd",
            "aabcdd",
            "abcdee",
            "ababab",
        ];
        assert_eq!(run(&input, 2), 4);
        assert_eq!(run(&input, 3), 3);
    }
}
