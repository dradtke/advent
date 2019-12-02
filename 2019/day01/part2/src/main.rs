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
    let result = (mass/3) - 2;
    if result <= 0 {
        return 0;
    }
    result + fuel(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuel() {
        assert_eq!(fuel(14), 2);
        assert_eq!(fuel(1969), 966);
        assert_eq!(fuel(100756), 50346);
    }
}
