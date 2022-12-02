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
    case "A":
      return .Rock
    case "B":
      return .Paper
    case "C":
      return .Scissors
    case:
      panic(fmt.aprintf("unknown throw: %s", s))
  }
}

throw_wins_to :: proc(throw: Throw) -> Throw {
  switch throw {
    case .Rock:
      return .Scissors
    case .Paper:
      return .Rock
    case .Scissors:
      return .Paper
  }
  panic("unreachable")
}

throw_loses_to :: proc(throw: Throw) -> Throw {
  switch throw {
    case .Rock:
      return .Paper
    case .Paper:
      return .Scissors
    case .Scissors:
      return .Rock
  }
  panic("unreachable")
}

GameResult :: enum {
  Win,
  Tie,
  Lose,
}

parse_result :: proc(s: string) -> GameResult {
  switch s {
    case "X":
      return .Lose
    case "Y":
      return .Tie
    case "Z":
      return .Win
    case:
      panic(fmt.aprintf("unknown result: %s", s))
  }
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
    needed_result := parse_result(parts[1])
    my_throw: Throw

    switch needed_result {
      case .Tie:
        my_throw = their_throw
        score += 3
      case .Win:
        my_throw = throw_loses_to(their_throw)
        score += 6
      case .Lose:
        my_throw = throw_wins_to(their_throw)
    }

    score += throw_scores[my_throw]
  }

  return fmt.aprintf("%d", score)
}

@(test)
test :: proc(t: ^testing.T) {
  result := run(`A Y
B X
C Z
`)
  testing.expect_value(t, result, "12")
}
