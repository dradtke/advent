package main

import "core:fmt"
import "core:os"
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
	for i := 0; i < len(input) - 4; i += 1 {
		m := make(map[rune]int)
		for j := 0; j < 4; j += 1 {
			char := rune(input[i+j])
			m[char] += 1
		}
		if !has_duplicates(m) {
			result := i + 4
			return fmt.aprintf("%d", result)
		}
	}
	panic("not found")
}

has_duplicates :: proc(m: map[rune]int) -> bool {
	for _, value in m {
		if value > 1 {
			return true
		}
	}
	return false
}

@(test)
test_1 :: proc(t: ^testing.T) {
  result := run(`bvwbjplbgvbhsrlpgdmjqwftvncz`)
  testing.expect_value(t, result, "5")
}

@(test)
test_2 :: proc(t: ^testing.T) {
  result := run(`nppdvjthqldpwncqszvftbrmjlhg`)
  testing.expect_value(t, result, "6")
}

@(test)
test_3 :: proc(t: ^testing.T) {
  result := run(`nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg`)
  testing.expect_value(t, result, "10")
}

@(test)
test_4 :: proc(t: ^testing.T) {
  result := run(`zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw`)
  testing.expect_value(t, result, "11")
}
