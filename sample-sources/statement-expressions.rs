fn main() {

  { 1 }[1];     // !!! Parses as { 1 }; [1];
  { 1 }(0);     // !!! Parses as { 1 }; (0);

  { 1 }.foo;    // Parses as a field access
  { 1 }.foo(0); // Parses as a method call
  { 1 }.0;      // Parses as a tup field access
  { 1 }?;       // Parses as a try


  { 1 }? + 1;  // SHOULD WORK
  { 1 }[0] + 1;  // SHOULD WORK
  { 1 }(0,1,2) + 1;  // SHOULD WORK
  { 1 }.foo(0,1,2) + 1;  // SHOULD WORK
  { 1 }.foo + 1;  // SHOULD WORK
  { 1 }.0 + 1;  // SHOULD WORK

 // { 1 } as i32 + 1;  // SHOULD NOT WORK
 // { 1 }  + 1;  // SHOULD NOT WORK

  { 1 }[1];
  { 1 }();
  { 1 }.bar;
  { 1 }.bar();
  { 1 }.0;


  if true { 1 } [1];
  if true { 1 } ();
  if true { 1 } .bar;
  if true { 1 } .bar();

  if true { 1 } else { 2 }[1];
  if true { 1 } else { 2 }();
  if true { 1 } else { 2 }.bar;
  if true { 1 } else { 2 }.bar();


  loop { 1 } [1];
  loop { 1 } ();
  loop { 1 } .bar;
  loop { 1 } .bar();

  match true {
   // true => { 1 } + 2, // SHOULD NOT WORK
    true => { 1 }? + 2, // SHOULD WORK
    false => 1,
    true => move | | { 1 },
    false => 1
  }
}
