fn main() {
  let x: [i32];
  let x: [i32; 128];
  let x: *mut i32;
  let x: *const i32;
  let x: &'a mut i32;
  let x: &mut i32;
  let x: &'a i32;
  let x: &i32;
  let x: fn() -> i32;
//  let x: fn(i32) -> i32;
//  let x: fn(i32,i32);
  let x: !;
  let x: (i32,);
  let x: (i32,!);
  let x: i32;
  let x: T; 
  let x: <Vec<T> as SomeTrait>::SomeType;
  let x: Bound1 + Bound2 + 'static;
  let x: (i32);
  let x: typeof(1i32);
  let x: _;
//  let x: HList![i32,(),u8];
}

fn foo() -> impl Bound1 + Bound2 + Bound3 { }


