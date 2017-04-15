For now, we have two categories of tests. If you are using `stack` you can run them with

```
$ stack test               # runs all tests
$ stack test :unit-tests   # runs unit test suite
$ stack test :rustc-tests  # runs rustc test suite
```

# `unit-tests` 

These are mostly regression/coverage style tests. They cover lexing, parsing, and pretty-printing.
The case for parsing is actually a bt more involved: on top of just checking that inputs parse
correctly, it also checks that:

  * resolving the parsed AST does nothing
  * pretty-printing the AST and then re-parsing does nothing
  * parsing substrings of the input corresponding to the span of sub-ASTs does nothing to those
    sub-ASTs

Whenever a bug is fixed or a feature introduced, new tests covering that case should be added to
`unit-tests` to prevent regressions.

# `rustc-tests`

These require `rustc` to be installed. The idea is to feed in as input a bunch of rust source files,
run them through the Rust compiler with `rustc -Z ast-json` to get a JSON of the AST, then parse the
same file on our side, and finally find the differences between the two.
