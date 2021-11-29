fn main() {
    let result = (284639..=748759)
        .map(|num| num.to_string())
        .filter(|password| has_adjacent_chars(&password))
        .filter(|password| has_no_decreasing(&password))
        .count();

    println!("part 1: {}", result);

    let result = (284639..=748759)
        .map(|num| num.to_string())
        .filter(|password| has_two_adjacent_chars(&password))
        .filter(|password| has_no_decreasing(&password))
        .count();

    println!("part 2: {}", result);
}

fn has_adjacent_chars(s: &str) -> bool {
    let mut chars = s.chars();
    let mut last_char = match chars.next() {
        Some(c) => c,
        None => return false,
    };

    for current_char in chars {
        if current_char == last_char {
            return true;
        }
        last_char = current_char;
    }

    false
}

fn has_two_adjacent_chars(s: &str) -> bool {
    let chars: Vec<char> = s.chars().collect();
    for i in 0..chars.len()-1 {
        let j = i+1;
        let left_ok = i == 0 || chars[i-1] != chars[i];
        let right_ok = j == chars.len()-1 || chars[j+1] != chars[j];
        if chars[i] == chars[j] && left_ok && right_ok {
            return true;
        }
    }
    false
}

fn has_no_decreasing(s: &str) -> bool {
    let mut chars = s.chars();
    let mut last_char = match chars.next() {
        Some(c) => c,
        None => return false,
    };

    for current_char in chars {
        if current_char < last_char {
            return false;
        }
        last_char = current_char;
    }

    true
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_has_adjacent_chars() {
        assert!(!has_adjacent_chars(""));
        assert!(!has_adjacent_chars("123456"));
        assert!(has_adjacent_chars("111111"));
    }

    #[test]
    fn test_has_two_adjacent_chars() {
        assert!(has_two_adjacent_chars("112233"));
        assert!(!has_two_adjacent_chars("123444"));
        assert!(has_two_adjacent_chars("111122"));
    }

    #[test]
    fn test_has_no_decreasing() {
        assert!(has_no_decreasing("111111"));
        assert!(has_no_decreasing("123789"));
        assert!(!has_no_decreasing("223450"));
    }
}
