#!/bin/sh

// Every token
token!{
  [= < > & | ! ~ + - * / % ^]
  (>= >>= && || << >> == != <= <<= -= &= |= += *= /= ^= %=)
  {@ . .. ... , ; : :: -> <- => # $ ?}
  ( ) [ ] { } 1.0foo x _ 'lt #! $x
  /*! inner doc comment */  /// outer doc comment
}

// Check new way of tokenization
tokentrees!{
  $x
  $x:ty
  $($xs:expr),+
  $(1+2),+ 
  br##"hello "#
     world!"###suf
}

// literals
literals!{
  b'a' b'\n' b'a'suffix
  'a' '\n' 'a'suffix
  
  123 123i32
  0b1100_1101 0b1100_1101isize
  0o3170 0o3170i64
  0xAFAC 0xAFACu32
  
  123.1 123.1f32 
  123.f32          // Gets parsed as [integer token "123", ".", ident token "f32"]
  123.0f32
  0e+10            // Gets parsed as [integer token "0" with suffix "e", "+", integer token "10"]
  00e+10
  9e+10
  123e-9f32
 
  strings!{
    "hello \n world!"
    
    "hello \n world!"suffix
    
    r"hello 
     world!"
    
    r"hello 
     world!"suffix
    
    b"hello \n world!"
    
    b"hello \n world!"suffix
    
    br"hello 
     world!"
    
    br"hello 
     world!"suffix
    
    
    "hello \
          world!"
    
    b"hello \
          world!"
    
    br##"hello "#
     world!"##suf
  }
}

fn main() {
  print!("{}\n", 9e+1);
  print!("{}\n", 0.1e+1);
  print!("{}\n", 00e+1);
  print!("{}\n", 001e+1);
  print!("{}\n", 123.);
  print!("{}\n", 123.0f32);
  print!("{}\n", 123.0__9);
  print!("{}\n", 123e-4f32);

  // Not what they look like, see above
  print!("{}\n", 0e+1);
  print!("{}\n", 123.f32);
}

