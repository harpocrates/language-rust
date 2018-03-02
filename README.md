# Parser and pretty printer for Rust [![Build Status][4]][5] [![Windows build status][7]][8] [![Hackage Version][11]][12]

`language-rust` aspires to efficiently and accurately parse and pretty print the [Rust language][0].
The underlying AST structures are also intended to be as similar as possible to the [`libsyntax` AST
that `rustc`][10] itself uses.

A typical use looks like:

```haskell
>>> :set -XTypeApplications +t
>>> import Language.Rust.Syntax

>>> -- Sample use of the parser
>>> import Language.Rust.Parser
>>> let inp = inputStreamFromString "fn main () { println!(\"Hello world!\"); }"
inp :: InputStream
>>> let sourceFile = parse' @(SourceFile Span) inp
sourceFile :: SourceFile Span

>>> -- Sample use of the pretty printer
>>> import Language.Rust.Pretty
>>> pretty' sourceFile
fn main() {
  println!("Hello world!");
}
it :: Doc b
```

## Building

### Cabal

With Cabal and GHC, run

    cabal install happy --constraint 'happy >= 1.19.8'
    cabal install alex
    cabal configure
    cabal build

### Stack

With the [Stack][1] tool installed, run

    stack init
    stack build

The second command is responsible for pulling in all of the dependencies (including executable
tools like [Alex][2], [Happy][3], and GHC itself) and then compiling everything. If Stack complains
about the version of Happy installed, you can explicitly install a recent one with `stack install
happy-1.19.8`.

## Evolution of Rust

As Rust evolves, so will `language-rust`. A best effort will be made to support unstable features
from nightly as they come out, but only compatibility with stable is guaranteed. The last component
of the version number indicates the nightly Rust compiler version against which tests were run. For
example, `0.1.0.26` is tested against `rustc 1.26.0-nightly`.

## Bugs

Please report any bugs to the [github issue tracker][9].

### Parser

Any difference between what is accepted by the `rustc` parser and the `language-rust` parser
indicates one of

  * a bug in `language-rust` (this is almost always the case)
  * a bug in `rustc`
  * that there is a newer version of `rustc` which made a breaking change to this syntax

If the AST/parser of `rustc` changes, the `rustc-tests` test suite should start failing - it
compares the JSON AST debug output of `rustc` to our parsed AST.

### Pretty printer

For the pretty printer, bugs are a bit tougher to list exhaustively. Suggestions for better layout
algorithms are most welcome! The [`fmt-rfcs`][6] repo is loosely used as the reference for "correct"
pretty printing.

[0]: https://www.rust-lang.org/en-US/
[1]: https://docs.haskellstack.org/en/stable/README/
[2]: https://hackage.haskell.org/package/alex
[3]: https://hackage.haskell.org/package/happy
[4]: https://travis-ci.org/harpocrates/language-rust.svg?branch=master
[5]: https://travis-ci.org/harpocrates/language-rust
[6]: https://github.com/rust-lang-nursery/fmt-rfcs
[7]: https://ci.appveyor.com/api/projects/status/um8dxklqmubvn091/branch/master?svg=true
[8]: https://ci.appveyor.com/project/harpocrates/language-rust/branch/master
[9]: https://github.com/harpocrates/language-rust/issues
[10]: https://github.com/rust-lang/rust/blob/master/src/libsyntax/ast.rs
[11]: https://img.shields.io/hackage/v/language-rust.svg
[12]: https://hackage.haskell.org/package/language-rust
