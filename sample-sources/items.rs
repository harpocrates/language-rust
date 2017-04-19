
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
  extern "C" { }

  type Foo = Bar<u8>;
  
  enum Foo<A, B> { C(A), D(B) }
  
  struct Foo<A> { x: A }
  union Foo<A, B> { x: A, y: B }
  
  trait Foo { }
  trait Foo<T> { }
  
  impl Trait for .. {}
  impl Trait<T> for .. {}
  impl<A> Foo<A> { }
  impl<A> Trait for Foo<A> { }
  
  macro_rules! foo { }
  foo!();

}
