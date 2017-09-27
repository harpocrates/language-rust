fn main() {
  let x = box "foo";
  let x = y <- 1;
  let x = [1,2,3];
  let x = foo(1,2,x);
  let x = x.foo::<Bar, Baz>(a, b, c, d);
  let x = (a, b, c ,d);
  let x = a + b;
  let x = a * b;
  let x = !x;
  let x = *x;
  let x = true as f64;
  let x = 1: f64;
  if true { } else { };
  if true { };
  if let y = true { };
  while true { }
  'l: while true { }
  while let y = true { continue; }
  'l: while let y = true { continue 'l; }
  for i in 1.. { }
  'l: for i in 1..10 { }
  loop { break; }
  'l: loop { break 'l 1; }
  match x { _ => () }
  let x = move |a,b,c| { a + b + c };
  let f = |_||x, y| x+y;
  let x = { 1 };
  let x = unsafe { 1 };
  a = 1;
  a += 1;
  let x = obj.foo;
  let x = foo.0;
  let x = foo[2];
  let x = &a;
  let x = &mut a;
  let x = return 1;
  let x = asm!("NOP");
  let x = println!("hi");
  let x = Foo { x: 1, y: 2 };
  let x = Foo { x: 1, ..base };
  let x = [1; 5];
  let x = 1 * (2 + 3);
  let x = foo()?;
  let x = do catch { 1 };
  return 0;
  return;

  match true {
    true => move | | { 1 },
    false => | | { 2}
  }
}

fn precedences() {
  x==&y||z|w.0<=x**y*&&z^=l^m<<=n;
  y|z..wm;
  1>!x%y-z*-w||u?==v;
}
