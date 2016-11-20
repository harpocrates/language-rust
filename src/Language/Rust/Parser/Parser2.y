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
%name pat

%tokentype { TokenSpace Spanned }

%monad { P } { >>= } { return }
%error { parseError }
%lexer { lexRust } { Tok (Spanned Eof _) }

%expect 0

%token

  -- Expression-operator symbols. 
  '='        { Tok $$@(Spanned Equal _) }
  '<'        { Tok $$@(Spanned Less _) }
  '>'        { Tok $$@(Spanned Greater _) }
  '!'        { Tok $$@(Spanned Exclamation _) }
  '~'        { Tok $$@(Spanned Tilde _) }
  
  '+'        { Tok $$@(Spanned Plus _) }
  '-'        { Tok $$@(Spanned Minus _) }
  '*'        { Tok $$@(Spanned Star _) }
  '/'        { Tok $$@(Spanned Slash _) }
  '%'        { Tok $$@(Spanned Percent _) }
  '^'        { Tok $$@(Spanned Caret _) }
  '&'        { Tok $$@(Spanned Ampersand _) }
  '|'        { Tok $$@(Spanned Pipe _) }

  -- Structural symbols.
  '@'        { Tok $$@(Spanned At _) }
  '...'      { Tok $$@(Spanned DotDotDot _) }
  '..'       { Tok $$@(Spanned DotDot _) }
  '.'        { Tok $$@(Spanned Dot _) }
  ','        { Tok $$@(Spanned Comma _) }
  ';'        { Tok $$@(Spanned Semicolon _) }
  '::'       { Tok $$@(Spanned ModSep _) }
  ':'        { Tok $$@(Spanned Colon _) }
  '->'       { Tok $$@(Spanned RArrow _) }
  '<-'       { Tok $$@(Spanned LArrow _) }
  '=>'       { Tok $$@(Spanned FatArrow _) }
  '#'        { Tok $$@(Spanned Pound _) }
  '$'        { Tok $$@(Spanned Dollar _) }
  '?'        { Tok $$@(Spanned Question _) }

  '('        { Tok $$@(Spanned (OpenDelim Paren) _) }
  '['        { Tok $$@(Spanned (OpenDelim Bracket) _) }
  '{'        { Tok $$@(Spanned (OpenDelim Brace) _) }
  ')'        { Tok $$@(Spanned (CloseDelim Paren) _) }
  ']'        { Tok $$@(Spanned (CloseDelim Bracket) _) }
  '}'        { Tok $$@(Spanned (CloseDelim Brace) _) }

  -- Literals.
  byte       { Tok $$@(Spanned (LiteralTok ByteTok{} _) _) }
  char       { Tok $$@(Spanned (LiteralTok CharTok{} _) _) }
  int        { Tok $$@(Spanned (LiteralTok IntegerTok{} _) _) }
  float      { Tok $$@(Spanned (LiteralTok FloatTok{} _) _) }
  str        { Tok $$@(Spanned (LiteralTok StrTok{} _) _) }
  byteStr    { Tok $$@(Spanned (LiteralTok ByteStrTok{} _) _) }
  rawStr     { Tok $$@(Spanned (LiteralTok StrRawTok{} _) _) }
  rawByteStr { Tok $$@(Spanned (LiteralTok ByteStrRawTok{} _) _) }

  -- Strict keywords used in the language
  as         { Tok $$@(Identifier "as") }
  box        { Tok $$@(Identifier "box") } 
  break      { Tok $$@(Identifier "break") } 
  const      { Tok $$@(Identifier "const") } 
  continue   { Tok $$@(Identifier "continue") }
  crate      { Tok $$@(Identifier "crate") } 
  else       { Tok $$@(Identifier "else") }
  enum       { Tok $$@(Identifier "enum") }
  extern     { Tok $$@(Identifier "extern") }
  false      { Tok $$@(Identifier "false") } 
  fn         { Tok $$@(Identifier "fn") }
  for        { Tok $$@(Identifier "for") } 
  if         { Tok $$@(Identifier "if") }
  impl       { Tok $$@(Identifier "impl") }
  in         { Tok $$@(Identifier "in") }
  let        { Tok $$@(Identifier "let") } 
  loop       { Tok $$@(Identifier "loop") }
  match      { Tok $$@(Identifier "match") } 
  mod        { Tok $$@(Identifier "mod") } 
  move       { Tok $$@(Identifier "move") }
  mut        { Tok $$@(Identifier "mut") } 
  pub        { Tok $$@(Identifier "pub") } 
  ref        { Tok $$@(Identifier "ref") } 
  return     { Tok $$@(Identifier "return") }
  selftype   { Tok $$@(Identifier "self") }
  selfvalue  { Tok $$@(Identifier "Self") } 
  static     { Tok $$@(Identifier "static") }
  struct     { Tok $$@(Identifier "struct") }
  super      { Tok $$@(Identifier "super") } 
  trait      { Tok $$@(Identifier "trait") } 
  true       { Tok $$@(Identifier "true") }
  type       { Tok $$@(Identifier "type") }
  unsafe     { Tok $$@(Identifier "unsafe") }
  use        { Tok $$@(Identifier "use") } 
  where      { Tok $$@(Identifier "where") } 
  while      { Tok $$@(Identifier "while") } 
  
  -- Keywords reserved for future use
  abstract   { Tok $$@(Identifier "abstract") }
  alignof    { Tok $$@(Identifier "alignof") } 
  become     { Tok $$@(Identifier "become") }
  do         { Tok $$@(Identifier "do") }
  final      { Tok $$@(Identifier "final") } 
  macro      { Tok $$@(Identifier "macro") } 
  offsetof   { Tok $$@(Identifier "offsetof") }
  override   { Tok $$@(Identifier "override") }
  priv       { Tok $$@(Identifier "priv") }
  proc       { Tok $$@(Identifier "proc") }
  pure       { Tok $$@(Identifier "pure") }
  sizeof     { Tok $$@(Identifier "sizeof") }
  typeof     { Tok $$@(Identifier "typeof") }
  unsized    { Tok $$@(Identifier "unsized") } 
  virtual    { Tok $$@(Identifier "virtual") } 
  yield      { Tok $$@(Identifier "yield") } 

  -- Weak keywords, have special meaning only in specific contexts.
  default    { Tok $$@(Identifier "default") } 
  staticLife { Tok $$@(Identifier "'static") }
  union      { Tok $$@(Identifier "union") } 

  -- Comments
  -- docComment { Tok $$@(Spanned (DocComment _) _) }
  -- comment    { Tok $$@(Spanned Comment _) }

  -- Types
  boolTyp    { Tok $$@(Identifier "bool") }
  charTyp    { Tok $$@(Identifier "char") }
  i8Typ      { Tok $$@(Identifier "i8") }
  i16Typ     { Tok $$@(Identifier "i16") }
  i32Typ     { Tok $$@(Identifier "i32") }
  i64Typ     { Tok $$@(Identifier "i64") }
  u8Typ      { Tok $$@(Identifier "u8") }
  u16Typ     { Tok $$@(Identifier "u16") }
  u32Typ     { Tok $$@(Identifier "u32") }
  u64Typ     { Tok $$@(Identifier "u64") }
  isizeTyp   { Tok $$@(Identifier "isize") }
  usizeTyp   { Tok $$@(Identifier "usize") }
  f32Typ     { Tok $$@(Identifier "f32") }
  f64Typ     { Tok $$@(Identifier "f64") }
  strTyp     { Tok $$@(Identifier "str") }

  -- Identifiers.
  ident      { Tok $$@(Identifier _) }
  '_'        { Tok $$@(Spanned Underscore _) }

  -- Lifetimes.
  lifetime   { Tok $$@(Spanned (LifetimeTok _) _) }

-- fake-precedence symbol to cause '|' bars in lambda context to parse
-- at low precedence, permit things like |x| foo = bar, where '=' is
-- otherwise lower-precedence than '|'. Also used for proc() to cause
-- things like proc() a + b to parse as proc() { a + b }.
%left LAMBDA   -- precedence

%left self     -- precedence

-- MUT should be lower precedence than IDENT so that in the pat rule,
-- "& MUT pat" has higher precedence than "binding_mode ident [@ pat]"
%left mut      -- precedence

-- IDENT needs to be lower than '{' so that 'foo {' is shifted when
-- trying to decide if we've got a struct-construction expr (esp. in
-- contexts like 'if foo { .')
--
-- IDENT also needs to be lower precedence than '<' so that '<' in
-- 'foo:bar . <' is shifted (in a trait reference occurring in a
-- bounds list), parsing as foo:(bar<baz>) rather than (foo:bar)<baz>.
%left ident      -- precedence

-- A couple fake-precedence symbols to use in rules associated with +
-- and < in trailing type contexts. These come up when you have a type
-- in the RHS of operator-AS, such as "foo as bar<baz>". The "<" there
-- has to be shifted so the parser keeps trying to parse a type, even
-- though it might well consider reducing the type "bar" and then
-- going on to "<" as a subsequent binop. The "+" case is with
-- trailing type-bounds ("foo as bar:A+B"), for the same reason.
%left SHIFTPLUS

%left '::'         -- precedence
%left '<-' ':'      -- precedence

-- In where clauses, "for" should have greater precedence when used as
-- a higher ranked constraint than when used as the beginning of a
-- for_in_type (which is a ty)
%left FORTYPE             -- precedence
%left for                 -- precedence

-- Binops & unops, and their precedences
%left box                 -- precedence
%left BOXPLACE            -- precedence
%nonassoc '..'

-- RETURN needs to be lower-precedence than tokens that start
-- prefix_exprs
%left return              -- precedence

%right '=' '<<=' '>>=' '-=' '&=' '|=' '+=' '*=' '\=' '^=' '%='
%right '<-'
%left '||'
%left '&&'
%left '==' '!='
%left '<' '>' '<=' '>='
%left '|'
%left '^'
%left '&'
%left '<<' '>>'
%left '+' '-'
%left AS                 -- precedence
%left '*' '/' '%'
%left '!'                -- precedence

%left '{' '[' '(' '.'    -- precedence

%left RANGE              -- precedence

%%

-------------
-- Utility --
-------------

-- | Optional parser
opt(p)          : p                   { Just $1 }
                |                     { Nothing }

-- | One or more
some(p)         : rev_list1(p)        { reverse $1 }

rev_list1(p)    : p                   { [$1] }
                | rev_list1(p) p      { $2 : $1 }

-- | Zero or more 
many(p)         : rev_list(p)         { reverse $1 }

rev_list(p)     : {- Empty -}         { [] }
                | rev_list(p) p       { $2 : $1 }

-- | Zero or more occurrences of p, separated by sep
sep_by(p,sep)   : {- Empty -}         { [] }
                | sep_by1(p,sep)      { $1 }

-- | One or more occurences of p, seperated by sep
sep_by1(p,sep)  : p many(then(sep,p)) { $1 : $2 }

-- | Sequence two parsers, return the result of the second (*>)
then(a,b)       : a b                 { $2 }



pat : {- Empty -}        { () }



{
  
}
