extern crate foo;
extern crate foo_bar as foo;

use foo;
use foo::bar;
use foo::bar as FooBar;

static FOO: i32 = 42;
static mut FOO: i32 = 42;
static FOO: &'static str = "bar";

const FOO: i32 = 42;

mod foo { }
mod bar {

  extern { }
  extern "C" {
    fn foo<T>(x: int) -> int;
    static x: int;
    static mut x: *mut int;
    type C;
  }

  type Foo = Bar<u8>;

  enum Foo<A, B> { C(A), D(B) }

  struct Foo<A> { x: A }
  union Foo<A, B> { x: A, y: B }

  pub(super) struct Point { x: i32 }

  auto trait IsCool { }
  trait Foo { }
  trait Foo<T> {
    const ID1: i32;
    const ID2: i32 = 1;

    fn area1(self) -> f64;
    fn area2(mut self) -> f64 { 1f64 }
    fn area1(&self) -> f64;
    fn area2(&mut self) -> f64 { 1f64 }
    fn area1(&'lt self) -> f64;
    fn area2(&'lt mut self) -> f64 { 1f64 }
    fn area1(self: Foo<T>) -> f64;
    fn area2(mut self: Foo<T>) -> f64 { 1f64 }

    type N;
    type N: fmt::Display;
    type N: fmt::Display = i32;

    foo!{}
  }

  trait Foo = Bar + Baz;
  trait Foo = ?Sized;
  trait Foo<'a,N> = Bar<'a,N> + Baz + 'a;

  fn foo<T: ?Sized>(x: &T) { }
  struct Foo<T: Send + ?Sized + Sync> { field: Box<T> }
  trait Bar { type Baz: ?Sized; }

  struct Bleh<T, U> where T: Copy, U: Sized;

  impl<A> Foo<A> {
    const ID: i32 = 1;
    fn area(&self) -> f64 { 1f64 }
    type N = i32;
    foo!();
  }
  impl<A> Trait for Foo<A> { }
  impl<A> !Trait for Foo<A> { }

  macro_rules! foo { }
  foo!();

  pub macro PartialEq($item:item) { /* compiler built-in */ }
  pub macro PartialEq { 1 }

  enum Foo {
    Baz {
      foo: i32,
      bar: (),
    },
    Bar(i32, i32),
  }
}
