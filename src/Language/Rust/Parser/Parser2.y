{

{-# LANGUAGE PatternSynonyms #-}
module Language.Rust.Parser.Parser2 where

import Language.Rust.Data.InputStream
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
import Language.Rust.Parser.Lexer
import Language.Rust.Parser.ParseMonad
import Language.Rust.Syntax.AST

-- Based heavily on <https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y>
-- References to <https://doc.rust-lang.org/grammar.html>

-- %name item item
-- %name pattern pattern
-- %name typ typ
-- %name statement statement
-- %name expression expression
}

-- in order to document the parsers, we have to alias them
%name equal

%tokentype { Spanned Token }

%monad { P } { >>= } { return }
%error { parseError }
%lexer { lexRust } { Spanned Eof _ }

%expect 0

%token

  -- Expression-operator symbols. 
  '='        { Spanned Eq _ }
  '<'        { Spanned Lt _ }
  '<='       { Spanned Le _ }
  '=='       { Spanned EqEq _ }
  '!='       { Spanned Ne _ }
  '>='       { Spanned Ge _ }
  '>'        { Spanned Gt _ }
  '&&'       { Spanned AndAnd _ }
  '||'       { Spanned OrOr _ }
  '!'        { Spanned Exclamation _ }
  '~'        { Spanned Tilde _ }
  
  '+'        { Spanned (BinOp Plus) _ }
  '-'        { Spanned (BinOp Minus) _ }
  '*'        { Spanned (BinOp Star) _ }
  '/'        { Spanned (BinOp Slash) _ }
  '%'        { Spanned (BinOp Percent) _ }
  '^'        { Spanned (BinOp Caret) _ }
  '&'        { Spanned (BinOp And) _ }
  '|'        { Spanned (BinOp Or) _ }
  '<<'       { Spanned (BinOp Shl) _ }
  '>>'       { Spanned (BinOp Shr) _ }

  '+='       { Spanned (BinOpEq Plus) _ }
  '-='       { Spanned (BinOpEq Minus) _ }
  '*='       { Spanned (BinOpEq Star) _ }
  '/='       { Spanned (BinOpEq Slash) _ }
  '%='       { Spanned (BinOpEq Percent) _ }
  '^='       { Spanned (BinOpEq Caret) _ }
  '&='       { Spanned (BinOpEq And) _ }
  '|='       { Spanned (BinOpEq Or) _ }
  '<<='      { Spanned (BinOpEq Shl) _ }
  '>>='      { Spanned (BinOpEq Shr) _ }

  -- Structural symbols.
  '@'        { Spanned At _ }
  '...'      { Spanned DotDotDot _ }
  '..'       { Spanned DotDot _ }
  '.'        { Spanned Dot _ }
  ','        { Spanned Comma _ }
  ';'        { Spanned Semicolon _ }
  '::'       { Spanned ModSep _ }
  ':'        { Spanned Colon _ }
  '->'       { Spanned RArrow _ }
  '<-'       { Spanned LArrow _ }
  '=>'       { Spanned FatArrow _ }
  '#'        { Spanned Pound _ }
  '$'        { Spanned Dollar _ }
  '?'        { Spanned Question _ }

  '('        { Spanned (OpenDelim Paren) _ }
  '['        { Spanned (OpenDelim Bracket) _ }
  '{'        { Spanned (OpenDelim Brace) _ }
  ')'        { Spanned (CloseDelim Paren) _ }
  ']'        { Spanned (CloseDelim Bracket) _ }
  '}'        { Spanned (CloseDelim Brace) _ }

  -- Literals.
  byte       { Spanned (LiteralTok (ByteTok _) _) _ }
  char       { Spanned (LiteralTok (CharTok _) _) _ }
  int        { Spanned (LiteralTok (IntegerTok _) _) _ }
  float      { Spanned (LiteralTok (FloatTok _) _) _ }
  str        { Spanned (LiteralTok (StrTok _) _) _ }
  byteStr    { Spanned (LiteralTok (ByteStrTok _) _) _ }
  rawStr     { Spanned (LiteralTok (StrRawTok _ _) _) _ }
  rawByteStr { Spanned (LiteralTok (ByteStrRawTok _ _) _) _ }

  -- Strict keywords used in the language
  as         { Identifier "as" }
  box        { Identifier "box" } 
  break      { Identifier "break" } 
  const      { Identifier "const" } 
  continue   { Identifier "continue" }
  crate      { Identifier "crate" } 
  else       { Identifier "else" }
  enum       { Identifier "enum" }
  extern     { Identifier "extern" }
  false      { Identifier "false" } 
  fn         { Identifier "fn" }
  for        { Identifier "for" } 
  if         { Identifier "if" }
  impl       { Identifier "impl" }
  in         { Identifier "in" }
  let        { Identifier "let" } 
  loop       { Identifier "loop" }
  match      { Identifier "match" } 
  mod        { Identifier "mod" } 
  move       { Identifier "move" }
  mut        { Identifier "mut" } 
  pub        { Identifier "pub" } 
  ref        { Identifier "ref" } 
  return     { Identifier "return" }
  selftype   { Identifier "self" }
  selfvalue  { Identifier "Self" } 
  static     { Identifier "static" }
  struct     { Identifier "struct" }
  super      { Identifier "super" } 
  trait      { Identifier "trait" } 
  true       { Identifier "true" }
  type       { Identifier "type" }
  unsafe     { Identifier "unsafe" }
  use        { Identifier "use" } 
  where      { Identifier "where" } 
  while      { Identifier "while" } 
  
  -- Keywords reserved for future use
  abstract   { Identifier "abstract" }
  alignof    { Identifier "alignof" } 
  become     { Identifier "become" }
  do         { Identifier "do" }
  final      { Identifier "final" } 
  macro      { Identifier "macro" } 
  offsetof   { Identifier "offsetof" }
  override   { Identifier "override" }
  priv       { Identifier "priv" }
  proc       { Identifier "proc" }
  pure       { Identifier "pure" }
  sizeof     { Identifier "sizeof" }
  typeof     { Identifier "typeof" }
  unsized    { Identifier "unsized" } 
  virtual    { Identifier "virtual" } 
  yield      { Identifier "yield" } 

  -- Weak keywords, have special meaning only in specific contexts.
  default    { Identifier "default" } 
  staticLife { Identifier "'static" }
  union      { Identifier "union" } 

  -- Comments
  docComment { Spanned (DocComment _) _ }
  comment    { Spanned Comment _ }

  -- Types
  boolTyp    { Identifier "bool" }
  charTyp    { Identifier "char" }
  i8Typ      { Identifier "i8" }
  i16Typ     { Identifier "i16" }
  i32Typ     { Identifier "i32" }
  i64Typ     { Identifier "i64" }
  u8Typ      { Identifier "u8" }
  u16Typ     { Identifier "u16" }
  u32Typ     { Identifier "u32" }
  u64Typ     { Identifier "u64" }
  isizeTyp   { Identifier "isize" }
  usizeTyp   { Identifier "usize" }
  f32Typ     { Identifier "f32" }
  f64Typ     { Identifier "f64" }
  strTyp     { Identifier "str" }

  -- Identifiers.
  ident      { Identifier _ }
  '_'        { Spanned Underscore _ }

  -- Lifetimes.
  lifetime   { Spanned (LifetimeTok _) _ }

%%

equal : '='  { "dummy" }

{

}