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

comma_m :: { Bool }
comma_m
  : {- Empty -}    { False }
  | ','            { True }

pat :: { Pat () }
pat
  : '_'                                           { WildP () }
  | '&' pat                                       { RefP $2 Immutable () }
  | '&' mut pat                                   { RefP $3 Mutable () }
  | '&&' pat                                      { RefP (RefP $2 Immutable ()) Immutable () }
  | '&&' mut pat                                  { RefP (RefP $2 Mutable ()) Immutable () }
  | '(' pats_list_context ')'                     { TupleP (fst $2) (snd $2) () }
  | '[' pats_list_binding ']'                     { $2 }
  | lit                                           { LitP $1 () }
  | '-' lit                                       { LitP (Unary [] Neg $1 ()) () }
  | path_expr                                     { error "Unimplemented" } {- PathP (Maybe (QSelf a)) (Path a) a -}
  | lit_or_path '...' lit_or_path                 { RangeP $1 $3 () }
  | path '{' pat_fields comma_m '}'               { StructP $1 $3 False () }
  | path '{' pat_fields ',' '..' '}'              { StructP $1 $3 True () }
  | path '{' '..' '}'                             { StructP $1 [] True () }
  | path '(' pats_list_context ')'                { TupleStructP $1 (snd $3) (fst $3) () }
{-  | path '!' maybe_ident delimited_token_trees    { error "Unimplemented" } {- MacP (Mac a) a -} -}
  | binding_mode ident '@' pat                    { IdentP $1 $2 (Just $4) () }
  | binding_mode ident                            { IdentP $1 $2 Nothing () }
  | box pat                                       { BoxP $2 () }
{- | '<' ty_sum maybe_as_trait_ref '>' '::' ident { $$ = mk_node("PatQualifiedPath", 3, $2, $3, $6); }
| '<<' ty_sum maybe_as_trait_ref '>' '::' ident maybe_as_trait_ref '>' '::' ident
{
  $$ = mk_node("PatQualifiedPath", 3, mk_node("PatQualifiedPath", 3, $2, $3, $6), $7, $10);
} -}

pats_list_context :: { (Maybe Int, [Pat ()]) }
pats_list_context
  :                    '..'                              { (Nothing, []) }
  | comma_pats comma_m                                   { (Nothing, $1) }
  | comma_pats comma_m '..'                              { (Just (length $1), $1) }
  |                    '..' comma_m comma_pats comma_m   { (Just 0, $3) }
  | comma_pats comma_m '..' comma_m comma_pats comma_m   { (Just (length $1), $1 ++ $5) }

pats_list_binding :: { ([Pat a], Maybe (Pat a), [Pat a]) }
pats_list_binding
  :                    pat_m '..'                              { SliceP [] $1 [] () }
  | comma_pats comma_m                                         { SliceP $1 Nothing [] () }
  | comma_pats comma_m pat_m '..'                              { SliceP $1 $3 [] () }
  |                    pat_m '..' comma_m comma_pats comma_m   { SliceP [] $1 $4 () }
  | comma_pats comma_m pat_m '..' comma_m comma_pats comma_m   { SliceP $1 $3 $6 () }

pat_m :: { Maybe (Pat ()) }
pat_m
  : pat          { Just $1 }
  | {- Empty -}  { Nothing }

pats_or :: { [Pat ()] }
pats_or : pats_or_rev  { reverse $1 }

pats_or_rev :: { [Pat ()] }
pats_or_rev
  : pat              { [$1] }
  | pats_or '|' pat  { $3 : $1 }

binding_mode :: { BindingMode }
binding_mode
  : ref mut     { ByRef Mutable }
  | ref         { ByRef Immutable }
  | mut         { ByValue Mutable }
  | {- Empty -} { ByValue Immutable }

lit_or_path :: { Expr () }
lit_or_path
  : path_expr    { $1 }    {-  PathP (Maybe (QSelf a)) (Path a) a  ?? -}
  | lit          { $1 }
  | '-' lit      { Unary [] Neg $2 () }

pat_field :: { FieldPat () }
pat_field
  :     binding_mode ident        { FieldPat $2 (IdentP $1 $2 Nothing ()) True () }
  | box binding_mode ident        { FieldPat $3 (BoxP (IdentP $2 $3 Nothing ()) ()) True () }
  | binding_mode ident ':' pat    { FieldPat $2 (IdentP $1 $2 (Just $4) ()) True () }

pat_fields :: { [FieldPat ()] }
pat_fields : pat_fields_rev  { reverse $1 }

pat_fields_rev :: { [FieldPat ()] }
pat_fields_rev
  : pat_field                      { [$1] }
  | pat_fields_rev ',' pat_field   { $3 : $1 }

comma_pats :: { [Pat ()] }
comma_pats : comma_pats_rev  { reverse $1 }

comma_pats_rev :: { [Pat ()] }
comma_pats_rev
  : pat                      { [$1] }
  | comma_pats_rev ',' pat   { $3 : $1 }


lit :: { Expr () }
lit : {- Unimplemented -}         { error "Unimplemented" }

path :: { Path () }
path : {- Unimplemented -}        { error "Unimplemented" }

path_expr :: { Expr () }
path_expr : {- Unimplemented -}   { error "Unimplemented" }


maybe_ident :: { Maybe Ident }
maybe_ident
  : {- Empty -}            { Nothing }
  | ident                  { Just $1 }



{
  
}