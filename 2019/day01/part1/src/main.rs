use std::io;
use std::io::prelude::*;

fn main() {
    let result = io::stdin().lock().lines()
        .map(|line| line.unwrap().parse::<i32>().unwrap()) // not the most graceful error handling
        .map(fuel)
        .fold(0, |acc, x| acc + x);

    println!("Result: {}", result);
}

fn fuel(mass: i32) -> i32 {
    (mass/3) - 2
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuel() {
        assert_eq!(fuel(12), 2);
        assert_eq!(fuel(14), 2);
        assert_eq!(fuel(1969), 654);
        assert_eq!(fuel(100756), 33583);
    }
}
