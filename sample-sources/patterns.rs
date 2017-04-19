fn main() {
  match 0 {
    _ => 0,
    x => 0,
    &x => 0,
    mut x => 0,
    &mut x => 0,
    Person { age, name } => 0,
    Point { age: 0, .. } => 0,
    Point(x, y, z) => 0,
    Point(x, y, .., z) => 0,
    std::math::pi => 0,
    <i32 as T>::math::e => 0,
    (x, y, z) => 0,
    (x, y, .., z) => 0,
    box x => 0,
    &mut (a,b) => 0,
    0 => 0,
    0...1 => 0,
    [a, b, i.., y, z] => 0,
    [a, b, .., y, z] => 0,
    [a, b, c] => 0,
 //   LinkedList!(1,2,3) => 0,
  }
}

