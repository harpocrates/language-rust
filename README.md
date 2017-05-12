# NOTE: `language-rust` is complete, but still undergoing testing! [![Build Status][4]][5]

`language-rust` aspires to efficiently and accurately parse and pretty-print the [Rust language][0].
The underlying AST structures are also intended to be as similar as possible to the AST `rustc` uses
itself. When `language-rust` and `rustc` have diverging AST, the divergence should be detailed in
the documentation.

## Building with Stack

With the [Stack][1] tool installed, you should only need to run

    stack init
    stack install

The second command is responsible for pulling in all of the dependencies (including executable
tools like [Alex][2], [Happy][3], and GHC itself) and then compiling everything.

## Evolution of Rust

As Rust evolves, so will `language-rust`. We will make a best effort to support unstable features
from nightly as they come out, but in general will only target compatibility with stable.

## Bugs

### Parser

Any difference between what is accepted by the `rustc` parser and the `language-rust` parser
indicates

  * a bug in `language-rust` (this is almost always the case)
  * a bug in `rustc`
  * that there is a newer version of `rustc` which made a breaking change to this syntax

If the AST/parser of `rustc` changes, the `rustc-tests` test suite should start failing - it
compares the JSON AST debug output of `rustc` to our parsed AST.

### Pretty-printer

For the pretty-printer, bugs are a bit tougher to list exhaustively. Suggestions for better layout
algorithms are most welcome! The [`fmt-rfcs`][6] repo is loosely used as the reference for "correct"
pretty-printing.

[0]: https://www.rust-lang.org/en-US/
[1]: https://docs.haskellstack.org/en/stable/README/
[2]: https://hackage.haskell.org/package/alex
[3]: https://hackage.haskell.org/package/happy
[4]: https://travis-ci.org/harpocrates/language-rust.svg?branch=master
[5]: https://travis-ci.org/harpocrates/language-rust
[6]: https://github.com/rust-lang-nursery/fmt-rfcs
