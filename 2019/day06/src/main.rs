use std::collections::HashMap;
use std::io::prelude::*;

fn main() -> std::io::Result<()> {
    let mut orbits = HashMap::new();
    for line in std::io::stdin().lock().lines() {
        mark_orbit(&mut orbits, line?);
    }

    println!("part 1: {}", count_orbits(&orbits));
    Ok(())
}

fn mark_orbit<S: ToString>(orbits: &mut HashMap<String, String>, line: S) {
    let line = line.to_string();
    let parts: Vec<&str> = line.split(")").collect();
    orbits.insert(parts[1].to_string(), parts[0].to_string());
}

fn count_orbits(orbits: &HashMap<String, String>) -> i32 {
    orbits.keys()
        .map(|key| orbiting(orbits, key))
        .map(|orbiting| orbiting.len() as i32)
        .sum()
}

fn orbiting<S: ToString>(orbits: &HashMap<String, String>, object: S) -> Vec<&str> {
    let object = object.to_string();
    match orbits.get(&object) {
        None => Vec::new(),
        Some(target) => {
            let mut result: Vec<&str> = vec![target];
            result.append(&mut orbiting(&orbits, &target));
            result
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn orbit(orbits: &mut HashMap<String, String>, orbiter: &str, orbitee: &str) {
        orbits.insert(String::from(orbiter), String::from(orbitee));
    }

    #[test]
    fn test_mark_orbit() {
        let mut orbits = HashMap::new();
        mark_orbit(&mut orbits, "A)B");
        mark_orbit(&mut orbits, "COM)A");
        assert_eq!(orbits.get("B").unwrap(), "A");
        assert_eq!(orbits.get("A").unwrap(), "COM");
    }

    #[test]
    fn test_orbiting() {
        let mut orbits = HashMap::new();
        orbit(&mut orbits, "A", "COM");
        orbit(&mut orbits, "B", "A");
        assert_eq!(orbiting(&orbits, "B"), vec!["A", "COM"]);
    }

    #[test]
    fn test_orbit_count() {
        let mut orbits = HashMap::new();
        orbit(&mut orbits, "B", "COM");
        orbit(&mut orbits, "C", "B");
        orbit(&mut orbits, "D", "C");
        orbit(&mut orbits, "E", "D");
        orbit(&mut orbits, "F", "E");
        orbit(&mut orbits, "G", "B");
        orbit(&mut orbits, "H", "G");
        orbit(&mut orbits, "I", "D");
        orbit(&mut orbits, "J", "E");
        orbit(&mut orbits, "K", "J");
        orbit(&mut orbits, "L", "K");
        assert_eq!(count_orbits(&orbits), 42);
    }
}
