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
  for line in strings.split_lines(input) {
    if len(line) == 0 {
      break
    }
    comp1 := line[:len(line)/2]
    comp2 := line[len(line)/2:]
    m := make(map[rune]bool)
    for char in comp1 {
      m[char] = true
    }
    for char in comp2 {
      if m[char] {
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
  testing.expect_value(t, result, "157")
}
