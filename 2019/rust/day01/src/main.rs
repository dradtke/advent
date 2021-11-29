use std::io;
use std::io::prelude::*;

fn main() {
    let input: Vec<i32> = io::stdin().lock().lines()
        .map(|line| line.expect("failed to read data"))
        .map(|line| line.parse().expect("failed to parse number"))
        .collect();

    println!("part 1: {}", input.iter().map(|mass| part1::fuel(*mass)).sum::<i32>());
    println!("part 2: {}", input.iter().map(|mass| part2::fuel(*mass)).sum::<i32>());
}

mod part1 {
    pub fn fuel(mass: i32) -> i32 {
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
}

mod part2 {
    pub fn fuel(mass: i32) -> i32 {
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
}

