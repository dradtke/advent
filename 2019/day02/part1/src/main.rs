use std::io;
use std::io::prelude::*;

fn main() {
    let iter = io::stdin().lock().split(b',');
    loop {
        match (iter.next()) {
            Some(Ok(vec![b'1'])) => {}, // add
            Some(Ok(vec![b'2'])) => {}, // multiply
            Some("99") => break, // exit
            Some(x) => panic!("unexpected opcode: {}", x),
            None => panic!("unexpected EOF"),
        }
    }
}

fn add(input1: i32, input2: i32, output: i32) {
}

fn multiply(input1: i32, input2: i32, output: i32) {
}
