use std::collections::VecDeque;
use std::convert::TryInto;
use std::io;
use std::io::prelude::*;
use std::sync::mpsc::{channel, Sender, Receiver};

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

    {
        let mut possible_phase_settings = (0..=4).collect::<VecDeque<isize>>();
        let mut largest_output = 0;
        permute(&mut Vec::new(), &mut possible_phase_settings, &mut |phase_settings| {
            let output = run_amps(program.clone(), phase_settings.into_iter().map(|x| *x));
            if output > largest_output {
                largest_output = output;
            }
        });
        println!("part 1: {}", largest_output);
    }

    {
        let mut possible_phase_settings = (5..=9).collect::<VecDeque<isize>>();
        let mut largest_output = 0;
        permute(&mut Vec::new(), &mut possible_phase_settings, &mut |phase_settings| {
            let output = run_amps_feedback(program.clone(), phase_settings);
            if output > largest_output {
                largest_output = output;
            }
        });
        println!("part 2: {}", largest_output);
    }
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

fn run<I>(mut program: Program, mut inputs: I) -> (Program, Vec<isize>)
    where I: Iterator<Item=isize>
{
    let mut i = 0;
    let mut outputs = Vec::new();
    loop {
        let opcode = program[i] % 100; // last two digits
        let modes = program[i] / 100; // everything else
        // println!("processing: {} (opcode = {}, modes = {})", program[i], opcode, modes);
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
                program[args[0] as usize] = inputs.next().expect("ran out of inputs!");
            },
            4 => {
                // output
                let args = read_program_args(&program, &mut i, 1);
                let output = get_param_value(&program, &args, modes, 0);
                outputs.push(output);
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
    (program, outputs)
}

fn run_threaded(amp: usize, mut program: Program, inputs: Receiver<isize>, outputs: Sender<isize>) {
    // println!("[{}] Starting", amp);
    std::thread::spawn(move || {
        let mut i = 0;
        loop {
            let opcode = program[i] % 100; // last two digits
            let modes = program[i] / 100; // everything else
            // println!("processing: {} (opcode = {}, modes = {})", program[i], opcode, modes);
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
                    // println!("[{}] Waiting on input", amp);
                    let input = inputs.recv().expect("ran out of inputs!");
                    // println!("[{}] Got input: {}", amp, input);
                    program[args[0] as usize] = input;
                },
                4 => {
                    // output
                    let args = read_program_args(&program, &mut i, 1);
                    let output = get_param_value(&program, &args, modes, 0);
                    // println!("[{}] Producing output: {}", amp, output);
                    outputs.send(output).unwrap();
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
                99 => {
                    // println!("[{}] Halting", amp);
                    break;
                },
                x => panic!("unexpected opcode: {}", x),
            }
        }
    });
}

fn run_amps<I: Iterator<Item=isize>>(program: Program, phase_settings: I) -> isize {
    phase_settings.fold(0, |last_output, phase_setting| {
        let (_, outputs) = run(program.clone(), vec![phase_setting, last_output].into_iter());
        outputs[0]
    })
}

fn run_amps_feedback(program: Program, phase_settings: &[isize]) -> isize {
    let (mut input_sender, mut input_receiver) = channel();
    let first_sender = input_sender.clone();
    let (result_sender, result_receiver) = channel();

    for i in 0..phase_settings.len() {
        let (output_sender, output_receiver) = channel();

        // println!("[{}] Kicking off", i);
        run_threaded(i, program.clone(), input_receiver, output_sender);
        input_sender.send(phase_settings[i]).unwrap();

        let (new_input_sender, new_input_receiver) = channel();
        input_sender = new_input_sender;
        input_receiver = new_input_receiver;

        let output_dest = if i == phase_settings.len()-1 { first_sender.clone() } else { input_sender.clone() };
        connect(output_receiver, output_dest, result_sender.clone());
    }

    first_sender.send(0).unwrap();
    result_receiver.recv().unwrap()
}

fn connect(receiver: Receiver<isize>, sender: Sender<isize>, result: Sender<isize>) {
    std::thread::spawn(move || {
        loop {
            match receiver.recv() {
                Ok(value) => {
                    if sender.send(value).is_err() {
                        // presumably it's done, send the result.
                        result.send(value).unwrap();
                    }
                },
                Err(_) => break,
            }
        }
    });
}

// Shamelessly stolen from https://rosettacode.org/wiki/Permutations#Rust
fn permute<T, F: FnMut(&[T])>(used: &mut Vec<T>, unused: &mut VecDeque<T>, action: &mut F) {
    if unused.is_empty() {
        action(used);
    } else {
        for _ in 0..unused.len() {
            used.push(unused.pop_front().unwrap());
            permute(used, unused, action);
            unused.push_back(used.pop().unwrap());
        }
    }
}
 
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_run_amps() {
        let program = vec![3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0];
        assert_eq!(run_amps(program, vec![4, 3, 2, 1, 0].into_iter()), 43210);

        let program = vec![3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0];
        assert_eq!(run_amps(program, vec![0,1,2,3,4].into_iter()), 54321);

        let program = vec![3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0];
        assert_eq!(run_amps(program, vec![1,0,4,3,2].into_iter()), 65210);
    }

    #[test]
    fn test_run_amps_feedback() {
        let program = vec![3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5];
        assert_eq!(run_amps_feedback(program, &vec![9,8,7,6,5]), 139629729);

        let program = vec![3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10];
        assert_eq!(run_amps_feedback(program, &vec![9,7,8,5,6]), 18216);
    }
}
