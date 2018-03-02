We have two categories of tests. If you are using `stack` you can run them with

```
$ stack test                                                      # runs all tests
$ stack test :unit-tests                                          # runs unit test suite
$ stack test :rustc-tests                                         # runs rustc test suite
$ stack test :rustc-tests --test-arguments "-t regex|src|dfa.rs"  # runs just one test file
$ stack test              --test-arguments "--hide-successes"     # show only failing tests
$ stack test              --test-arguments "--help"               # learn about more options!
```

## `unit-tests` 

These are mostly regression/coverage style tests. They cover lexing, parsing, and pretty printing.
The case for parsing is actually a bit more involved: on top of just checking that inputs parse
correctly, it also checks that:

  * resolving the parsed AST does nothing
  * pretty printing the AST and then re-parsing does nothing
  * parsing substrings of the input corresponding to the span of sub-ASTs does nothing to those
    sub-ASTs

Whenever a bug is fixed or a feature introduced, new tests covering that case should be added to
`unit-tests` to prevent regressions.

## `rustc-tests`

These require `rustup` to be installed. The idea is to feed in as input many rust source files, run
them through the Rust compiler with `rustc -Z ast-json -Z parse-only` (now only available on
nightly) to get a JSON of the AST, then parse the same file on our side, and finally find the
differences between the two. Specifically, these tests check that:

  * we parse the same thing as `rustc`
  * pretty printing the AST and then reparsing it doesn't change anything

Any test source files should be placed in the `sample-sources` directory at the project root.

This test suite also automatically downloads and sets `rustc` to nightly (only for this folder). In
the interest of reproducibility, we pin the specific nightly version (see the top of the `Main`
module). If the version of nightly being tested against is more than a month old, you will get
warnings when the tests are run.
