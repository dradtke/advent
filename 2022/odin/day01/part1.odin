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
  calorie_counts: [dynamic]int
  for elf in strings.split(input, "\n\n") {
    count: int
    for item in strings.split(elf, "\n") {
      count += strconv.atoi(item)
    }
    append(&calorie_counts, count)
  }
  max := slice.max(calorie_counts[:])
  return fmt.aprintf("%d", max)
}

@(test)
test :: proc(t: ^testing.T) {
  result := run(`1000
2000
3000

4000

5000
6000

7000
8000
9000

10000`)
  testing.expect_value(t, result, "24000")
}
