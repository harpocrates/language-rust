# NOTE: `language-rust` is not yet ready for general use!

`language-rust` aspires to efficiently and accurately parse and pretty-print the [Rust language][0].
The underlying AST structures are also intended to be as similar as possible to the AST `rustc` uses
itself. When `language-rust` and `rustc` have diverging AST, the divergence should be detailed in
the documentation.

## Evolution of Rust

As Rust evolves, so will `language-rust`. We will make a best effort to support unstable features
from nightly as they come out, but in general will only target compatibility with stable.

## Bugs

Any difference between what is accepted by the `rustc` parser and the `language-rust` parser
indicates

  * a bug in `language-rust` (this is almost always the case)
  * a bug in `rustc`
  * that there is a newer version of `rustc` which made a breaking change to this syntax

For the pretty-printer, bugs are a bit tougher to list exhaustively. As a function of the parser, we can
say that `parse . pretty == id`. Anything violating that is a bug. Suggestions for better layout
algorithms are most welcome!

[0]: https://www.rust-lang.org/en-US/
