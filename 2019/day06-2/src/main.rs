use std::collections::HashMap;
use std::cell::RefCell;
use std::io::prelude::*;
use std::rc::Rc;

fn main() -> std::io::Result<()> {
    let mut nodes: HashMap<String, NodeRef> = HashMap::new();
    for line in std::io::stdin().lock().lines() {
        let parts: Vec<String> = line?.split(")").map(|s| s.to_string()).collect();

        let orbitee = nodes.entry(parts[0].clone())
            .or_insert(Node::new_root(&parts[0]))
            .clone();

        let orbiter = nodes.entry(parts[1].clone())
            .or_insert(Node::new_root(&parts[1]))
            .clone();

        if orbiter.borrow().orbiting.is_none() {
            orbiter.borrow_mut().orbiting = Some(orbitee.clone());
        }

        orbitee.borrow_mut().orbiters.push(orbiter.clone());
    }
    let you = nodes.get("YOU").unwrap().clone();
    let san = nodes.get("SAN").unwrap().clone();
    let path = search(&vec![], you, san).unwrap();
    println!("number of orbital transfers: {}", orbital_transfers(&path));
    Ok(())
}

#[derive(Debug)]
struct Node {
    name: String,
    orbiting: Option<NodeRef>,
    orbiters: Vec<NodeRef>,
}

type NodeRef = Rc<RefCell<Node>>;

impl Node {
    fn new_root<S: ToString>(name: S) -> NodeRef {
        Rc::new(RefCell::new(Node{
            name: name.to_string(),
            orbiting: None,
            orbiters: Vec::new(),
        }))
    }

    fn new<S: ToString>(name: S, orbiting: NodeRef) -> NodeRef {
        Rc::new(RefCell::new(Node{
            name: name.to_string(),
            orbiting: Some(orbiting),
            orbiters: Vec::new(),
        }))
    }
}

fn add_orbiter<S: ToString>(orbitee: &NodeRef, name: S) -> NodeRef {
    let orbiter = Node::new(name, orbitee.clone());
    orbitee.borrow_mut().orbiters.push(orbiter.clone());
    orbiter
}

fn search(path: &Vec<NodeRef>, current: NodeRef, target: NodeRef) -> Option<Vec<NodeRef>> {
    let mut path = path.clone();
    path.push(current.clone());
    // print_path(&path);

    if current.borrow().name == target.borrow().name {
        return Some(path);
    }

    for orbiter in &(current.borrow()).orbiters {
        if !path_contains(&path, orbiter.clone()) {
            if let Some(result) = search(&path, orbiter.clone(), target.clone()) {
                return Some(result);
            }
        }
    }

    if let Some(orbiting) = &(current.borrow()).orbiting {
        if !path_contains(&path, orbiting.clone()) {
            if let Some(result) = search(&path, orbiting.clone(), target.clone()) {
                return Some(result);
            }
        }
    }

    None
}

fn path_contains(path: &Vec<NodeRef>, target: NodeRef) -> bool {
    path.iter().any(|node| node.borrow().name == target.borrow().name)
}

fn orbital_transfers(path: &Vec<NodeRef>) -> usize {
    path.len() - 3
}

fn path_names(path: &Vec<NodeRef>) -> Vec<String> {
    path.iter().map(|node| node.borrow().name.clone()).collect()
}

#[allow(dead_code)]
fn print_path(path: &Vec<NodeRef>) {
    println!("path: {}", path_names(path).join(" --> "));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_contains() {
        let com = Node::new_root("COM");
        let path = vec![com.clone()];
        assert!(path_contains(&path, com.clone()));
    }

    #[test]
    fn test_simple_search() {
        let com = Node::new_root("COM");
        let b = add_orbiter(&com, "B");
        let c = add_orbiter(&com, "C");

        // path from B to C will need to go through COM

        assert_eq!(
            search(&vec![], b.clone(), c.clone()).map(|path| path_names(&path)),
            Some(vec![String::from("B"), String::from("COM"), String::from("C")]),
        );
    }

    #[test]
    #[allow(unused_variables)]
    fn test_harder_search() {
        let com = Node::new_root("COM");
        let b = add_orbiter(&com, "B");
        let c = add_orbiter(&b, "C");
        let d = add_orbiter(&c, "D");
        let e = add_orbiter(&d, "E");
        let f = add_orbiter(&e, "F");
        let g = add_orbiter(&b, "G");
        let h = add_orbiter(&g, "H");
        let i = add_orbiter(&d, "I");
        let j = add_orbiter(&e, "J");
        let k = add_orbiter(&j, "K");
        let l = add_orbiter(&k, "L");

        let you = add_orbiter(&k, "YOU");
        let san = add_orbiter(&i, "SAN");

        let expected = vec!["YOU", "K", "J", "E", "D", "I", "SAN"];

        assert_eq!(
            search(&vec![], you.clone(), san.clone()).map(|path| path_names(&path)),
            Some(expected.iter().map(|s| String::from(*s)).collect()),
        );
    }
}
