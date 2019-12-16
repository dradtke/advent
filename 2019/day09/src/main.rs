use std::io::prelude::*;
use std::sync::mpsc::{channel, Sender, Receiver};

fn main() {
    let program_memory = read_program(std::io::stdin().lock());
    let program = Program::new(program_memory);

    let (input_sender, input_receiver) = channel();
    let (output_sender, output_receiver) = channel();
    run(program, input_receiver, output_sender);

    input_sender.send(1).unwrap();
    loop {
        match output_receiver.recv() {
            Ok(value) => println!("{}", value),
            Err(_) => break,
        }
    }
}

struct Program {
    memory: Vec<i64>,
    idx: usize,
    relative_base: i64,
}

impl Program {
    fn new(memory: Vec<i64>) -> Program {
        Program{
            memory: memory,
            idx: 0,
            relative_base: 0,
        }
    }

    fn input_arg(&mut self, modes: &mut i64) -> i64 {
        let arg = self.read(self.idx);
        self.idx += 1;
        match Mode::next(modes) {
            Mode::Position => self.read(arg as usize),
            Mode::Immediate => arg,
            Mode::Relative => self.read((self.relative_base + arg) as usize),
        }
    }

    fn output_arg(&mut self) -> usize {
        let arg = self.read(self.idx);
        self.idx += 1;
        arg as usize
    }

    fn step(&mut self) -> (i64, i64) {
        let opcode = self.read(self.idx) % 100; // last two digits
        let modes = self.read(self.idx) / 100; // everything else
        self.idx += 1;
        (opcode, modes)
    }

    fn ensure_memory_length(&mut self, dest: usize) {
        if self.memory.len() <= dest {
            self.memory.resize(dest+1, 0);
        }
    }

    fn read(&mut self, dest: usize) -> i64 {
        self.ensure_memory_length(dest);
        self.memory[dest]
    }

    fn write(&mut self, value: i64, dest: usize) {
        self.ensure_memory_length(dest);
        self.memory[dest] = value;
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum Mode {
    Position,
    Immediate,
    Relative,
}

impl Mode {
    fn from(b: i64) -> Mode {
        match b {
            0 => Mode::Position,
            1 => Mode::Immediate,
            2 => Mode::Relative,
            x => panic!("invalid node byte: {}", x),
        }
    }

    fn next(modes: &mut i64) -> Mode {
        let value = *modes % 10;
        if *modes > 0 {
            *modes /= 10;
        }
        Mode::from(value)
    }
}

fn read_program<R: BufRead>(input: R) -> Vec<i64> {
    input.split(b',')
        .map(|result| result.unwrap())
        .map(|bytes| String::from_utf8(bytes).unwrap())
        .map(|string| string.trim().parse::<i64>().unwrap())
        .collect()
}

fn run(mut program: Program, inputs: Receiver<i64>, outputs: Sender<i64>) {
    std::thread::spawn(move || {
        loop {
            let (opcode, mut modes) = program.step();
            // println!("processing opcode: {}", opcode);
            match opcode {
                1 => {
                    let arg1 = program.input_arg(&mut modes);
                    let arg2 = program.input_arg(&mut modes);
                    let dest = program.output_arg();
                    println!("[1] saving {} + {} to {}", arg1, arg2, dest);
                    program.write(arg1 + arg2, dest);
                },
                2 => {
                    let arg1 = program.input_arg(&mut modes);
                    let arg2 = program.input_arg(&mut modes);
                    let dest = program.output_arg();
                    println!("[2] saving {} * {} to {}", arg1, arg2, dest);
                    program.write(arg1 * arg2, dest);
                },
                3 => {
                    // input
                    println!("[3] waiting for input...");
                    let dest = program.output_arg();
                    let input = inputs.recv().expect("ran out of inputs!");
                    println!("[3] saving {} to {}", input, dest);
                    program.write(input, dest);
                },
                4 => {
                    // output
                    let arg = program.input_arg(&mut modes);
                    println!("[4] outputting {}", arg);
                    outputs.send(arg).unwrap();
                },
                5 => {
                    // jump-if-true
                    let arg1 = program.input_arg(&mut modes);
                    let arg2 = program.input_arg(&mut modes);
                    if arg1 != 0 {
                        println!("[5] jumping to {}", arg2);
                        program.idx = arg2 as usize;
                    }
                },
                6 => {
                    // jump-if-false
                    let arg1 = program.input_arg(&mut modes);
                    let arg2 = program.input_arg(&mut modes);
                    if arg1 == 0 {
                        println!("[6] jumping to {}", arg2);
                        program.idx = arg2 as usize;
                    }
                },
                7 => {
                    // less-than
                    let arg1 = program.input_arg(&mut modes);
                    let arg2 = program.input_arg(&mut modes);
                    let dest = program.output_arg();
                    program.write(if arg1 < arg2 { 1 } else { 0 }, dest);
                    println!("[7] saving {} to {}", program.memory[dest], dest);
                },
                8 => {
                    // equals
                    let arg1 = program.input_arg(&mut modes);
                    let arg2 = program.input_arg(&mut modes);
                    let dest = program.output_arg();
                    program.write(if arg1 == arg2 { 1 } else { 0 }, dest);
                    println!("[8] saving {} to {}", program.memory[dest], dest);
                },
                9 => {
                    // adjust relative base
                    println!("real arg: {}", program.memory[program.idx]);
                    let arg = program.input_arg(&mut modes);
                    program.relative_base += arg;
                    println!("[9] adjusting relative base by {} (new value: {})", arg, program.relative_base);
                },
                99 => {
                    break;
                },
                x => panic!("unexpected opcode: {}", x),
            }
        }
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_modes() {
        let mut program = Program::new(vec![1002,4,3,4]);
        let (opcode, mut modes) = program.step();
        assert_eq!(opcode, 2);
        assert_eq!(Mode::next(&mut modes), Mode::Position);
        assert_eq!(Mode::next(&mut modes), Mode::Immediate);

        let mut program = Program::new(vec![209,12]);
        let (opcode, mut modes) = program.step();
        assert_eq!(Mode::next(&mut modes), Mode::Relative);
    }

    #[test]
    fn test_read_program() {
        let memory = read_program("1,-2".as_bytes());
        assert_eq!(memory, vec![1, -2]);
    }

    #[test]
    fn test_outputs_same() {
        let memory = vec![109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99];

        let mut program = Program::new(memory.clone());
        let (_, input_receiver) = channel();
        let (output_sender, output_receiver) = channel();
        run(program, input_receiver, output_sender);

        let mut result = Vec::new();

        loop {
            match output_receiver.recv() {
                Ok(value) => result.push(value),
                Err(_) => break,
            }
        }

        assert_eq!(result, memory);
    }

    #[test]
    fn test_output_16_digits() {
        let mut program = Program::new(vec![1102,34915192,34915192,7,4,7,99,0]);
        let (_, input_receiver) = channel();
        let (output_sender, output_receiver) = channel();
        run(program, input_receiver, output_sender);

        let output = output_receiver.recv().unwrap();
        assert_eq!(output.to_string().len(), 16);
    }

    #[test]
    fn test_output_large_number() {
        let mut program = Program::new(vec![104,1125899906842624,99]);
        let (_, input_receiver) = channel();
        let (output_sender, output_receiver) = channel();
        run(program, input_receiver, output_sender);

        let output = output_receiver.recv().unwrap();
        assert_eq!(output, 1125899906842624);
    }
}
