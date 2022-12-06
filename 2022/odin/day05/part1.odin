package main

import "core:bufio"
import "core:bytes"
import "core:container/queue"
import "core:fmt"
import "core:os"
import "core:strconv"
import "core:strings"
import "core:testing"

main :: proc() {
  if len(os.args) != 2 {
    panic(fmt.aprintf("usage: %s <input>", os.args[0]))
  }
  filename := os.args[1]
  input, success := os.read_entire_file(filename)
  if !success {
    panic("failed to read input file")
  }
  fmt.println(run(string(input)))
}

run :: proc(input: string) -> string {
  input_halves := strings.split(input, "\n\n")
  starting_stacks := strings.split_lines(input_halves[0])

  stack_positions := get_stack_positions(starting_stacks[len(starting_stacks)-1])
  defer delete(stack_positions)

  stacks := make(map[int]queue.Queue(rune))
  for n, _ in stack_positions {
    stacks[n] = queue.Queue(rune){}
  }
  for i := len(starting_stacks)-2; i >= 0; i -= 1 {
    line := starting_stacks[i]
    for n, index in stack_positions {
      if box := line[index]; box != ' ' {
        queue.push_back(&stacks[n], rune(box))
      }
    }
  }

  move_instruction_lines := strings.split_lines(input_halves[1])
  for line in move_instruction_lines {
    move_instruction := parse_move_instruction(line)
    for n := 0; n < move_instruction.Count; n += 1 {
      move_one(&stacks, move_instruction.From, move_instruction.To)
    }
  }

  result := bytes.Buffer{}
  for i := 1; i <= len(stacks); i += 1 {
    if _, err := bytes.buffer_write_rune(&result, queue.peek_back(&stacks[i])^); err != nil {
      panic("failed to write result rune")
    }
  }

  return bytes.buffer_to_string(&result)
}

get_stack_positions :: proc(line: string) -> map[int]int {
  indices := make(map[int]int)
  for character, index in line {
    if character != ' ' {
      n := int(character) - int('0')
      indices[n] = index
    }
  }
  return indices
}

MoveInstruction :: struct {
  Count, From, To: int
}

parse_move_instruction :: proc(line: string) -> MoveInstruction {
  instruction := MoveInstruction{}

  r := strings.Reader{}
  scanner := bufio.Scanner{
    r = strings.to_reader(&r, line),
    split = bufio.scan_words,
  }

  bufio.scanner_scan(&scanner) // "move"
  bufio.scanner_scan(&scanner)
  instruction.Count = strconv.atoi(bufio.scanner_text(&scanner))

  bufio.scanner_scan(&scanner) // "from"
  bufio.scanner_scan(&scanner)
  instruction.From = strconv.atoi(bufio.scanner_text(&scanner))

  bufio.scanner_scan(&scanner) // "to"
  bufio.scanner_scan(&scanner)
  instruction.To = strconv.atoi(bufio.scanner_text(&scanner))

  return instruction
}

move_one :: proc(stacks: ^map[int]queue.Queue(rune), from, to: int) {
  box := queue.pop_back(&stacks[from])
  if !queue.push_back(&stacks[to], box) {
    panic("cannot push box onto stack!")
  }
}

@(test)
test :: proc(t: ^testing.T) {
  result := run(`    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2`)
  testing.expect_value(t, result, "CMZ")
}
