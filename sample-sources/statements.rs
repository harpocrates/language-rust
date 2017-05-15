fn main() {
  // Local
  let x = 1;

  // Item
  fn foo() { return 1 }

  // Empty
  ;;;

  // NoSemi
  if true {
    foo()
  }
  let b = { let y = 3; y };

  // Semi
  2 + { 1 };

  // Mac
  println!("hi")
}
