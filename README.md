Consolidated Advent of Code submissions

## Running

### Erlang

To run `part1.erl`:

```sh
$ erlc part1.erl
$ cat input | erl -noshell -s part1 -s init stop
```

To run tests in `part1_tests.erl`:

```sh
$ erlc part1_tests.erl
$ erl -noshell -s eunit test -s init stop
```

### Rust

To run:

```sh
$ cat input | cargo run
```

To test:

```sh
$ cargo test
```
