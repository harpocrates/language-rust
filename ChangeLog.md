# Revision history for language-rust

## 0.3.0.39  -- 2019-11-01

* Bump Rust version
    - async blocks, async closures, async functions
    - try blocks
    - await
    - or-patterns
    - associated type constraints
    - macro definitions (fixes #31)
    - optional discriminator (::) in type paths
    - foreign macros

* Fix a couple old issues
    - parsing of expressions which conflict with statements is much more robust
    - parsing of expressions in the no-struct context
    - handle semicolons more like rustc

* Test harnesses have more options for skipping, removing, etc. tests

## 0.2.0.27  -- 2018-04-22

* Bump Rust version

## 0.1.1.26  -- 2018-03-02

* Bump test and benchmark dependencies

## 0.1.0.26  -- 2018-03-01

* Parser module (using Alex and Happy)
* Pretty printing module
* Resolving module for validating ASTs
* Unit testsuite
* Difference testsuite
* Allocation benchmarks
* Timing benchmarks
