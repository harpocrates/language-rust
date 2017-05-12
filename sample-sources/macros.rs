#!/bin/sh

// Every token
token!{
  [= < > & | ! ~ + - * / % ^]
  (>= >>= && || << >> == != <= <<= -= &= |= += *= /= ^= %=)
  {@ . .. ... , ; : :: -> <- => # $ ?}
  ( ) [ ] { } 1.0foo x _ 'lt /*! doc comment */ #! $x
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
  // 123.f32 123e-9f32 0e+10
 
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

