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

Throw :: enum {
  Rock,
  Paper,
  Scissors,
}

parse_throw :: proc(s: string) -> Throw {
  switch s {
    case "A", "X":
      return .Rock
    case "B", "Y":
      return .Paper
    case "C", "Z":
      return .Scissors
    case:
      panic(fmt.aprintf("unknown throw: %s", s))
  }
}

GameResult :: enum {
  Win,
  Tie,
  Lose,
}

play :: proc(their_throw, my_throw: Throw) -> GameResult {
  if their_throw == my_throw {
    return .Tie
  }
  if (their_throw == .Rock && my_throw == .Paper) || (their_throw == .Paper && my_throw == .Scissors) || (their_throw == .Scissors && my_throw == .Rock) {
    return .Win
  }
  return .Lose
}

run :: proc(input: string) -> string {
  throw_scores := map[Throw]int{
    .Rock = 1,
    .Paper = 2,
    .Scissors = 3,
  }

  score: int
  for line in strings.split_lines(input) {
    if len(line) == 0 {
      break
    }
    parts := strings.split(line, " ")
    their_throw := parse_throw(parts[0])
    my_throw := parse_throw(parts[1])

    score += throw_scores[my_throw]
    switch play(their_throw, my_throw) {
      case .Lose:
        // add nothing
      case .Tie:
        score += 3
      case .Win:
        score += 6
    }
  }
  return fmt.aprintf("%d", score)
}

@(test)
test :: proc(t: ^testing.T) {
  result := run(`A Y
B X
C Z
`)
  testing.expect_value(t, result, "15")
}
