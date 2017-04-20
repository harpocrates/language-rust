
extern crate foo;
extern crate foo_bar as foo;

// use foo;
// use foo::bar;
// use foo::bar as FooBar;

static FOO: i32 = 42;
static mut FOO: i32 = 42;
static FOO: &'static str = "bar";

const FOO: i32 = 42;

mod foo { }
mod bar {

  extern { }
  extern "C" {
    fn foo(x: int) -> int;
    static x: int;
    static mut x: *mut int;
  }

  type Foo = Bar<u8>;
  
  enum Foo<A, B> { C(A), D(B) }
  
  struct Foo<A> { x: A }
  union Foo<A, B> { x: A, y: B }
  
  trait Foo { }
  trait Foo<T> {
    const ID1: i32;
    const ID2: i32 = 1;

    fn area1(&self) -> f64;
    fn area2(&self) -> f64 { 1f64 }

    type N;
    type N: fmt::Display;
    type N: fmt::Display = i32;

   // foo!{}
  }
  
  impl Trait for .. {}
  impl Trait<T> for .. {}
  impl<A> Foo<A> {
    const ID: i32 = 1;
    fn area(&self) -> f64 { 1f64 }
    type N = i32;
    // foo!()

  }
  impl<A> Trait for Foo<A> { }
  impl<A> !Trait for Foo<A> { }
  
 // macro_rules! foo { }
 // foo!();

}
