use std::io;
use std::io::prelude::*;
use std::sync::mpsc::Sender;

const TEN: isize = 10;

type Program = Vec<isize>;

#[derive(Debug, PartialEq, Clone, Copy)]
enum Mode {
    Position,
    Immediate,
}

impl Mode {
    fn from(b: isize) -> Mode {
        match b {
            0 => Mode::Position,
            1 => Mode::Immediate,
            x => panic!("invalid node byte: {}", x),
        }
    }
}

fn main() {
    let program = read_program(io::stdin().lock());
    run(program);
}

fn read_program<R: BufRead>(input: R) -> Program {
    input.split(b',')
        .map(|result| result.unwrap())
        .map(|bytes| String::from_utf8(bytes).unwrap())
        .map(|string| string.trim().parse::<isize>().unwrap())
        .collect()
}

fn read_program_args(program: &Program, modes: isize, i: &mut usize, n: usize) -> Vec<isize> {
    let start = *i+1;
    let end = start+n;
    *i = end;
    program[start..end].to_vec()
        /*
        .enumerate()
        .map(|(i, param)| {
            let mode = Mode::from((modes / ten.pow(i as u32)) % ten);
            get_param_value(&program, mode, *param)
        })
        .collect()
        */
}

fn get_param_value(program: &Program, args: &Vec<isize>, modes: isize, index: usize) -> isize {
    let mode = Mode::from((modes / TEN.pow(index as u32)) % TEN);
    let param = args[index];
    match mode {
        Mode::Position => program[param as usize],
        Mode::Immediate => param,
    }
}

fn run(mut program: Program) -> Program {
    let mut i = 0;
    loop {
        let opcode = program[i] % 100; // last two digits
        let modes = program[i] / 100; // everything else
        // println!("processing: {} (opcode = {}, modes = {})", program[i], opcode, modes);
        match opcode {
            1 => {
                let args = read_program_args(&program, modes, &mut i, 3);
                let param1 = get_param_value(&program, &args, modes, 0);
                let param2 = get_param_value(&program, &args, modes, 1);
                // println!("adding {} + {} -> {} saved to {}", param1, param2, param1 + param2, args[2]);
                program[args[2] as usize] = param1 + param2;
            },
            2 => {
                let args = read_program_args(&program, modes, &mut i, 3);
                let param1 = get_param_value(&program, &args, modes, 0);
                let param2 = get_param_value(&program, &args, modes, 1);
                // println!("multing {} * {} -> {} saved to {}", param1, param2, param1 * param2, args[2]);
                program[args[2] as usize] = param1 * param2;
            },
            3 => {
                // input
                let args = read_program_args(&program, modes, &mut i, 1);
                let input = 1;
                // println!("inputting value to {}: {}", args[0], input);
                program[args[0] as usize] = input;
            },
            4 => {
                // output
                let args = read_program_args(&program, modes, &mut i, 1);
                let output = get_param_value(&program, &args, modes, 0);
                println!("output: {}", output);
            },
            99 => break,
            x => panic!("unexpected opcode: {}", x),
        }
    }
    program
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run() {
        assert_eq!(run(vec![1,9,10,3,2,3,11,0,99,30,40,50]), vec![3500,9,10,70,2,3,11,0,99,30,40,50]);
        assert_eq!(run(vec![1,0,0,0,99]), vec![2,0,0,0,99]);
        assert_eq!(run(vec![2,3,0,3,99]), vec![2,3,0,6,99]);
        assert_eq!(run(vec![2,4,4,5,99,0]), vec![2,4,4,5,99,9801]);
        assert_eq!(run(vec![1,1,1,4,99,5,6,0,99]), vec![30,1,1,4,2,5,6,0,99]);

        // new ones
        assert_eq!(run(vec![1002,4,3,4,33]), vec![1002,4,3,4,99]);
        assert_eq!(run(vec![1101,100,-1,4,0]), vec![1101,100,-1,4,99]);
    }
}
