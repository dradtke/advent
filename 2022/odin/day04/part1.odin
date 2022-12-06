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
  result: int
  for line in strings.split_lines(input) {
    if len(line) == 0 {
      break
    }
    ranges := strings.split(line, ",")
    r1 := parse_range(ranges[0])
    r2 := parse_range(ranges[1])
    if contains(r1, r2) || contains(r2, r1) {
      result += 1
    }
  }
  return fmt.aprintf("%d", result)
}

Range :: struct {
  start: int,
  end: int,
}

parse_range :: proc(s: string) -> Range {
  parts := strings.split(s, "-")
  return Range{
    start = strconv.atoi(parts[0]),
    end = strconv.atoi(parts[1]),
  }
}

contains :: proc(container, containee: Range) -> bool {
  return container.start <= containee.start && container.end >= containee.end
}

@(test)
test :: proc(t: ^testing.T) {
  result := run(`2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8`)
  testing.expect_value(t, result, "2")
}
