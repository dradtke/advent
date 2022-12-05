package main

import "core:fmt"
import "core:os"
import "core:slice"
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
  priority_sum: int
  lines := strings.split_lines(input)
  for i := 0; i < len(lines)/3; i += 1 {
    m := make(map[int]map[rune]bool)
    defer delete(m)
    for g in 0..<3 {
      m[g] = make(map[rune]bool)
      line := lines[(i*3)+g]
      for char in line {
        (&m[g])[char] = true
      }
    }
    for char, _ in m[0] {
      if m[1][char] && m[2][char] {
        priority_sum += get_priority(char)
        break
      }
    }
  }
  return fmt.aprintf("%d", priority_sum)
}

get_priority :: proc(char: rune) -> int {
  if char >= 'a' && char <= 'z' {
    return (int(char) - int('a')) + 1
  } else if char >= 'A' && char <= 'Z' {
    return (int(char) - int('A')) + 27
  } else {
    panic("non-alphabetic char")
  }
}

@(test)
test :: proc(t: ^testing.T) {
  result := run(`vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw`)
  testing.expect_value(t, result, "70")
}
