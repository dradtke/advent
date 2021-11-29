Commands (erl):

```erl
c(part1).
c(part1_tests).
eunit:test(part1).
```

Commands (shell):

```sh
$ erlc part1.erl                                 # Compile it
$ erl -noshell -s part1 -s init stop < input     # Run it
$ erlc part1_tests.erl                           # Compile tests
$ erl -noshell -s eunit test part1 -s init stop  # Run tests
```
