use std::io;
use std::io::prelude::*;

fn main() {
    let mut program = read_program(io::stdin().lock());
    program[1] = 12;
    program[2] = 2;
    program = run(program);
    println!("Result: {}", program[0]);
}

type Program = Vec<usize>;

fn read_program<R: BufRead>(input: R) -> Program {
    input.split(b',')
        .map(|result| result.unwrap())
        .map(|bytes| String::from_utf8(bytes).unwrap())
        .map(|string| string.trim().parse::<usize>().unwrap())
        .collect()
}

fn run(mut program: Program) -> Program {
    let mut i = 0;
    loop {
        match program[i] {
            1 => {
                let input1 = program[i+1];
                let input2 = program[i+2];
                let output = program[i+3];
                program[output] = program[input1] + program[input2];
                i += 4;
            },
            2 => {
                let input1 = program[i+1];
                let input2 = program[i+2];
                let output = program[i+3];
                program[output] = program[input1] * program[input2];
                i += 4;
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
    fn test_read_program() {
        assert_eq!(read_program("1,2,3,4".as_bytes()), vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_read_program_with_newline() {
        assert_eq!(read_program("1,2,3,4\n".as_bytes()), vec![1, 2, 3, 4]);
    }

    #[test]
    fn test_run() {
        assert_eq!(run(vec![1,9,10,3,2,3,11,0,99,30,40,50]), vec![3500,9,10,70,2,3,11,0,99,30,40,50]);
        assert_eq!(run(vec![1,0,0,0,99]), vec![2,0,0,0,99]);
        assert_eq!(run(vec![2,3,0,3,99]), vec![2,3,0,6,99]);
        assert_eq!(run(vec![2,4,4,5,99,0]), vec![2,4,4,5,99,9801]);
        assert_eq!(run(vec![1,1,1,4,99,5,6,0,99]), vec![30,1,1,4,2,5,6,0,99]);
    }
}
