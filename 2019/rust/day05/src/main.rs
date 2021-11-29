use std::convert::TryInto;
use std::io;
use std::io::prelude::*;

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
    println!("part 1:");
    run(program.clone(), 1);
    println!("");
    println!("part 2:");
    run(program.clone(), 5);
}

fn read_program<R: BufRead>(input: R) -> Program {
    input.split(b',')
        .map(|result| result.unwrap())
        .map(|bytes| String::from_utf8(bytes).unwrap())
        .map(|string| string.trim().parse::<isize>().unwrap())
        .collect()
}

fn read_program_args(program: &Program, i: &mut usize, n: usize) -> Vec<isize> {
    let start = *i+1;
    let end = start+n;
    *i = end;
    program[start..end].to_vec()
}

fn get_param_value(program: &Program, args: &Vec<isize>, modes: isize, index: usize) -> isize {
    let mode = Mode::from((modes / TEN.pow(index as u32)) % TEN);
    let param = args[index];
    match mode {
        Mode::Position => program[param as usize],
        Mode::Immediate => param,
    }
}

// TODO: figure out a better way to capture the output
fn run(mut program: Program, input: isize) -> Program {
    let mut i = 0;
    loop {
        let opcode = program[i] % 100; // last two digits
        let modes = program[i] / 100; // everything else
        println!("processing: {} (opcode = {}, modes = {})", program[i], opcode, modes);
        match opcode {
            1 => {
                let args = read_program_args(&program, &mut i, 3);
                let param1 = get_param_value(&program, &args, modes, 0);
                let param2 = get_param_value(&program, &args, modes, 1);
                // println!("adding {} + {} -> {} saved to {}", param1, param2, param1 + param2, args[2]);
                program[args[2] as usize] = param1 + param2;
            },
            2 => {
                let args = read_program_args(&program, &mut i, 3);
                let param1 = get_param_value(&program, &args, modes, 0);
                let param2 = get_param_value(&program, &args, modes, 1);
                // println!("multing {} * {} -> {} saved to {}", param1, param2, param1 * param2, args[2]);
                program[args[2] as usize] = param1 * param2;
            },
            3 => {
                // input
                let args = read_program_args(&program, &mut i, 1);
                // println!("inputting value to {}: {}", args[0], input);
                program[args[0] as usize] = input;
            },
            4 => {
                // output
                let args = read_program_args(&program, &mut i, 1);
                let output = get_param_value(&program, &args, modes, 0);
                println!("output: {}", output);
            },
            5 => {
                // jump-if-true
                let args = read_program_args(&program, &mut i, 2);
                let param1 = get_param_value(&program, &args, modes, 0);
                let param2 = get_param_value(&program, &args, modes, 1);
                if param1 != 0 {
                    i = param2.try_into().unwrap();
                }
            },
            6 => {
                // jump-if-false
                let args = read_program_args(&program, &mut i, 2);
                let param1 = get_param_value(&program, &args, modes, 0);
                let param2 = get_param_value(&program, &args, modes, 1);
                if param1 == 0 {
                    i = param2.try_into().unwrap();
                }
            },
            7 => {
                // less-than
                let args = read_program_args(&program, &mut i, 3);
                let param1 = get_param_value(&program, &args, modes, 0);
                let param2 = get_param_value(&program, &args, modes, 1);
                let dest = args[2];
                program[dest as usize] = if param1 < param2 { 1 } else { 0 };
            },
            8 => {
                // equals
                let args = read_program_args(&program, &mut i, 3);
                let param1 = get_param_value(&program, &args, modes, 0);
                let param2 = get_param_value(&program, &args, modes, 1);
                let dest = args[2];
                program[dest as usize] = if param1 == param2 { 1 } else { 0 };
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
}
