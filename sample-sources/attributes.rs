fn main () {
  match x {
    #[arm_outer]
    0 => true,
  }
}

fn expressions() {
  #[box_outer]
  box 1;

//  #[inplace_outer]
//  x <- 1;

  #[vec_outer]
  [ #![vec_inner] 1, 2, 3];

  #[call_outer]
  foo(1, 2, 3);

  #[methodcall_outer]
  x.foo(1, 2, 3);

  #[tuple_outer]
  ( #![tuple_inner] 1, 2, 3);
  #[tuple_outer]
  ( #![tuple_inner] 1, );
  #[tuple_outer]
  ( #![tuple_inner] );

//  #[binary_outer]
//  1 + 2;

  #[unary_outer]
  -1;

  #[lit_outer]
  1;

//  #[cast_outer]
//  1 as f64;

//  #[typeascription_outer]
//  1: f32;

//  #[if_outer]
//  if (true) {
//    1;
//  }

//  #[iflet_outer]
//  if let x = true {
//    1;
//  }

  #[while_outer]
  while (true) {
    #![while_inner]
    1;
  }
  
  #[whilelet_outer]
  while let x = true {
    #![whilelet_inner]
    1;
  }

  #[for_outer]
  for i in 1..3 {
    #![for_inner]
    1;
  }

  #[loop_outer]
  loop {
    #![loop_inner]
    1;
  }

  #[match_outer]
  match x {
    #![match_inner]
    _ => 1
  }

  #[closure_outer]
  move |x| { 1 + 2 };

  #[blockexpr_outer]
  { #![blockexpr_inner] 1; 2 }
  #[blockexpr_outer]
  unsafe { #![blockexpr_inner] 1; 2 }

//  #[catch_outer]  UPDATE RUSTC
//  do catch { #![catch_inner] 1 }

//  #[assign_outer]
//  x = 1;

//  #[assignop_outer]
//  x += 1;

  #[fieldaccess_outer]
  x.foo;

  #[tupfield_outer]
  x.0;

  #[index_outer]
  x[0];

//  #[range_outer]
//  1..2;

  #[pathexpr_outer]
  math::PI;

}

