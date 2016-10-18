{
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Rust.Parser.Parser where

import Language.Rust.Data.InputStream
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
import Language.Rust.Parser.Lexer
import Language.Rust.Syntax.AST

--- https://doc.rust-lang.org/grammar.html
}

%name parseExpr
%tokentype { Language.Rust.Syntax.Token.Token }
%error { parseError }

%token

  -- Expression-operator symbols. 
  '='        { Eq }
  '<'        { Lt }
  '<='       { Le }
  '=='       { EqEq }
  '!='       { Ne }
  '>='       { Ge }
  '>'        { Gt }
  '&&'       { AndAnd }
  '||'       { OrOr }
  '!'        { Not }
  '~'        { Tilde }
  
  '+'        { BinOp Plus }
  '-'        { BinOp Minus }
  '*'        { BinOp Star }
  '/'        { BinOp Slash }
  '%'        { BinOp Percent }
  '^'        { BinOp Caret }
  '&'        { BinOp And }
  '|'        { BinOp Or }
  '<<'       { BinOp Shl }
  '>>'       { BinOp Shr }

  '++'       { BinOpEq Plus }
  '-+'       { BinOpEq Minus }
  '*+'       { BinOpEq Star }
  '/+'       { BinOpEq Slash }
  '%+'       { BinOpEq Percent }
  '^+'       { BinOpEq Caret }
  '&+'       { BinOpEq And }
  '|+'       { BinOpEq Or }
  '<<+'      { BinOpEq Shl }
  '>>+'      { BinOpEq Shr }

  -- Structural symbols.
  '@'        { At }
  '...'      { DotDotDot }
  '..'       { DotDot }
  '.'        { Dot }
  ','        { Comma }
  ';'        { Semi }
  '..'       { ModSep }
  ':'        { Colon }
  '->'       { RArrow }
  '<-'       { LArrow }
  '=>'       { FatArrow }
  '#'        { Pound }
  '$'        { Dollar }
  '?'        { Question }

  '('        { OpenDelim Paren }
  '['        { OpenDelim Bracket }
  '{'        { OpenDelim Brace }
  ')'        { CloseDelim Paren }
  ']'        { CloseDelim Bracket }
  '}'        { CloseDelim Brace }

  -- Literals. TODO ByteStr and ByteStrRaw
  byte       { Literal (Byte $$) _ }
  char       { Literal (Char $$) _ }
  
  string     { Literal (Str_ $$) _ }
  bytestring { Literal (ByteStr $$) _ }

  int        { Literal (Integer $$) _ }
  float      { Literal (Float $$) _ }

  -- Strict keywords used in the language.
  as         { As }
  box        { Box }
  break      { Break }
  const      { Const }
  continue   { Continue }
  crate      { Crate }
  else       { Else }
  enum       { Enum }
  extern     { Extern }
  false      { False_ }
  fn         { Fn }
  for        { For }
  if         { If }
  impl       { Impl }
  in         { In }
  let        { Let }
  loop       { Loop }
  match      { Match }
  mod        { Mod }
  move       { Move }
  mut        { Mut }
  pub        { Pub }
  ref        { Ref }
  return     { Return }
  self       { SelfValue }
  Self       { SelfType }
  static     { Static }
  struct     { Struct }
  super      { Super }
  trait      { Trait }
  true       { True_ }
  type       { Type }
  unsafe     { Unsafe }
  use        { Use }
  where      { Where }
  while      { While }

  -- Keywords reserved for future use.
  abstract   { Abstract }
  alignof    { Alignof }
  become     { Become }
  do         { Do }
  final      { Final }
  macro      { Macro }
  offsetof   { Offsetof }
  override   { Override }
  priv       { Priv }
  proc       { Proc }
  pure       { Pure }
  sizeof     { Sizeof }
  typeof     { Typeof }
  unsized    { Unsized }
  virtual    { Virtual }
  yield      { Yield }

  -- Weak keywords, have special meaning only in specific contexts.
  default    { Default }
  "\'static" { StaticLifetime }
  union      { Union }

  -- Identifiers.
  ident      { IdentTok $$ }
  '_'        { Underscore }

  -- Lifetimes.
  lifetime   { Lifetime $$ }

  -- DocComment.
  doc        { DocComment $$ }

  -- Whitespace.
  whitespace { Whitespace }

  -- Comment.
  comment    { Comment }

%%


-- 6.3 Attributes (Done)

attribute :: { Attribute () }
attribute
    : '#' '[' meta_item ']'                 {  Attribute Outer $3 False () }
    | '#' '!' '[' meta_item ']'             {  Attribute Inner $3 False () }

meta_item :: { MetaItem () }
meta_item
    : ident '=' literal                     { NameValue $1 $3 () }
    | ident '(' meta_seq ')'                { List $1 (reverse $3) () }
    | ident                                 { Word $1 () }

meta_seq :: { [NestedMetaItem ()] }
meta_seq
    : {- Empty list -}                      { [] }
    | meta_seq ',' meta_item                { (MetaItem $3 ()) : $1 }
    | meta_seq ',' literal                  { (Literal $3 ()) : $1 }

attribute_list_rev :: { [Attribute ()] }
attribute_list_rev
    : {- Empty list -}                      { [] }
    | attribute_list_rev attribute          { $2 : $1 }

attribute_list :: { [Attribute ()] } 
attribute_list : attribute_list_rev         { reverse $1 }

-- 7.2.3 Tuple expressions (Done)

tuple_expr :: { Expr () }
tuple_expr
    : attribute_list '(' expr ',' ')'       { TupExpr $1 [$3] () }
    | attribute_list '(' expr_list ',' ')'  { TupExpr $1 $3 () }
    | attribute_list '(' expr_list ')'      { TupExpr $1 $3 () }

-- 7.2.4 Unit expressions (Done)

unit_expr :: { Expr () }
unit_expr : attribute_list '(' ')'          { TupExpr $1 [] }

-- 7.2.5 Structure expressions (Done)
-- TODO: ensure we really don't need the (expr_path) / (function-call-like struct) here )

struct_expr :: { Expr () }
struct_expr
    : attribute_list expr_path '{' struct_fields ',' '}'            { Struct $1 $2 (reverse $3) Nothing () }
    | attribute_list expr_path '{' struct_fields '}'                { Struct $1 $2 (reverse $3) Nothing () }
    | attribute_list expr_path '{' struct_fields ',' ".." expr '}'  { Struct $1 $2 (reverse $3) (Just $7) () }
    | attribute_list expr_path '{' struct_fields ".." expr '}'      { Struct $1 $2 (reverse $3) (Just $6) () }

struct_fields :: { [Field ()] }
struct_fields
    : {- Empty list -}                      { [] }
    | struct_fields ',' ident ':' expr      { (Field $3 $5 ()) : $1 }


-- 7.2.6 Block expressions (Done)

block_expr :: { Expr () }
block_expr
    : attribute_list '{'
        stmt_item_list
        expr
      '}'                                   { BlockExpr $1 (Block (reverse $3) DefaultBlock ()) () } 
    | attribute_list unsafe '{'
        stmt_item_list
        expr
      '}'                                   { BlockExpr $1 (Block (reverse $4) (UnsafeBlock False) ()) () }

stmt_item_list :: { [ Stmt () ] }
stmt_item_list
    : {- Empty list -}                      { [] }
    | stmt_item_list stmt                   { $2 : $1 }
    | stmt_item_list item                   { (ItemStmt $2 ()) : $1 }

-- 7.2.7 Method-call expressions (Done)

method_call_expr :: { Expr () }
method_call_expr
    : attribute_list expr '.' ident
      method_ty_params '('
        expr_list comma_m
      ')'                                   { MethodCall $1 $4 $5 ($2 : $7) () }

method_ty_params :: { [Ty ()] }
method_ty_params
    : {- None -}                            { [] }
    | '::' '<' ty_list '>'                  { $3 }

-- 7.2.8 Field expressions (Done)

field_expr :: { Expr () }
field_expr : attribute_list expr '.' ident  { FieldAccess $1 $2 $4 () }

-- 7.2.9 Array expressions (Done)
-- TOOD check if `mut` inside array is something allowed. If so, add it here.

array_expr :: { Expr () }
array_expr
    : attribute_list '[' expr_list comma_m ']' { Vec $1 $3 () }
    | attribute_list '[' expr ';' expr ']'     { Repeat $1 $3 $5 }

-- 7.2.10 Index expressions (Done)

idx_expr :: { Expr () }
idx_expr : attribute_list expr '[' expr ']' { Index $1 $2 $4 () }

-- 7.2.11 Range expressions (Done)

range_expr :: { Expr () }
range_expr
    : attribute_list maybe_expr '...' maybe_expr { Range $1 $2 $4 HalfOpen () }
    | attribute_list maybe_expr '..' maybe_expr  { Range $1 $2 $4 Closed () }

-- 7.2.12 Unary operator expressions (Done)

unop_expr :: { Expr () }
unop_expr : attribute_list unop expr        { Unary $1 $2 $3 () }

unop :: { UnOp }
unop
    : '*'                                   { Deref }
    | '!'                                   { Not } 
    | '-'                                   { Neg }

-- 7.2.13 Binary operator expressions (Done)
-- TODO precedence

-- Inlined: 7.2.13.1 Arithmetic operators
-- Inlined: 7.2.13.2 Bitwise operators
-- Inlined: 7.2.13.3 Lazy boolean operators
-- Inlined: 7.2.13.4 Comparison operators
-- Inlined: 7.2.13.5 Type cast expressions
-- Inlined: 7.2.13.6 Assignment expressions
-- Inlined: 7.2.13.7 Compound assignment expressions

binop_expr :: { Expr () }
binop_expr
    : attribute_list expr binop expr        { Binary $1 $3 $2 $4 () }
    | attribute_list value as type          { Cast $1 $2 $4 () }
    | attribute_list expr '=' expr          { Assign $1 $2 $4 () }
    | attribute_list expr assign_op expr    { AssignOp $1 $3 $2 $4 () }

binop :: { BinOp }
binop
    : '+'                                   { AddOp }
    | '-'                                   { SubOp }
    | '*'                                   { MulOp }
    | '/'                                   { DivOp }
    | '%'                                   { RemOp }
    | '&'                                   { AndOp }
    | '|'                                   { OrOp }
    | '^'                                   { BitXorOp }
    | '<<'                                  { BitAndOp }
    | '>>'                                  { BitOrOp } 
    | '&&'                                  { ShlOp }
    | '||'                                  { ShrOp }
    | '=='                                  { EqOp }
    | '!='                                  { LtOp }
    | '<'                                   { LeOp }
    | '>'                                   { NeOp }
    | '<='                                  { GeOp }
    | '>='                                  { GtOp }

assign_op :: { Binop }
assign_op
    : '+='                                  { AddOp }
    | '-='                                  { SubOp }
    | '*='                                  { MulOp }
    | '/='                                  { DivOp }
    | '%='                                  { RemOp }
    | '&='                                  { AndOp }
    | '|='                                  { OrOp }
    | '^='                                  { BitXorOp }
    | '<<='                                 { BitAndOp }
    | '>>='                                 { BitOrOp } 
    | '&&='                                 { ShlOp }
    | '||='                                 { ShrOp }

-- 7.2.14 Grouped expressions (Done)

paren_expr :: { Expr () }
paren_expr : attribute_list '(' expr ')'    { ParenExpr $1 $3 () }

-- 7.2.15 Call expressions (Done)

call_expr :: { Expr () }
call_expr
    : attribute_list expr '(' expr_list comma_m ')'   { Call $1 $2 $4 () }

7.2.16 Lambda expressions

lambda_expr
    : attribute_list '|' ident_list_rev '|' expr      { Closure $1 CaptureBy (FnDecl a) (Block a) () }
    | attribute_list '|' ident_list_rev ',' '|' expr  { Closure $1 CaptureBy (FnDecl a) (Block a) () }

ident_list_rev :: { [Expr ()] }
ident_list_rev
    : {- Empty list -}                       { [] }
    | ident_list_rev ',' ident               { $3 : $1 }


-- 7.2.17 While loops (Done)

while_expr :: { Expr () }
while_expr
    : attribute_list maybe_lifetime "while"
      no_struct_literal_expr '{' block '}'  { While $1 $4 $5 $2 () }

-- 7.2.18 Infinite loops (Done)

loop_expr :: { Expr () }
loop_expr
    : attribute_list maybe_lifetime "loop" '{'
        block
      '}'                                   { Loop $1 $5 $2 () }

-- 7.2.19 Break expressions (Done)
-- 7.2.20 Continue expressions (Done)

break_continue_expr :: { Expr () }
break_expr
    : attribute_list break                  { Break $1 Nothing () }
    | attribute_list break lifetime         { Break $1 (Just $3) () } 
    | attribute_list continue               { Continue $1 Nothing () }
    | attribute_list continue lifetime      { Continue $1 (Just $3) () }

-- 7.2.21 For expressions (Done)

for_expr :: { Expr () }
for_expr
    : attribute_list maybe_lifetime for
      pat in no_struct_literal_expr '{'
        block
      '}'                                   { ForLoop $1 $4 $6 $8 $2 () }

-- 7.2.22 If expressions (Done)
-- 7.2.24 If let expressions (Done)

if_expr :: { Expr () }
if_expr
    : attribute_list
      if no_struct_literal_expr '{'
        block
      '}' else_tail                         { If $1 $3 $5 $7 () }

if_let_expr :: { Expr () }
if_let_expr
    : attribute_list
      if let pat '=' expr '{'
        block
      '}' else_tail                         { IfLet $1 $4 $6 $8 $10 () }

else_tail :: { Maybe (Expr ()) }
else_tail : {- Nothing -}                   { Nothing }
          | else if_expr                    { Just $2 }
          | else if_let_expr                { Just $2 }
          | else '{' block '}'              { Just (BlockExpr [] $3 ()) }

-- 7.2.23 Match expressions (Done)

match_expr :: { Expr () }
match_expr
    : attribute_list match
      no_struct_literal_expr '{'
        match_arm_list_rev
      '}'                                   { Match $1 $2 (reverse $4) () } 

match_arm_list_rev :: { [Arm ()] }
match_arm_list_rev
    : {- Empty list -}                      { [] }
    | match_arm_list_rev match_arm          { $2 : $1 }

match_arm :: { Arm () }
match_arm
    : attribute_list pat_list_rev
      maybe_guard '=>' expr ','             { Arm $1 (reverse $2) $3 $5 }
    | attribute_list pat_list_rev
      maybe_guard '=>' '{' block '}'        { Arm $1 (reverse $2) $3 (BlockExpr [] $6 ()) () }

pat_list_rev :: { [Pat ()] }
pat_list_rev
    : pat                                   { [$1] }
    | pat_list_rev '|' pat                  { $3 : $1 }

maybe_guard :: { Maybe (Expr ()) }
maybe_guard
    : {- Nothing -}                         { Nothing }
    | if expr                               { Just $2 }

-- 7.2.25 While let loops (Done)

while_let_expr :: { Expr () }
while_let_expr
    : attribute_list maybe_lifetime whilelet
      pat '=' expr '{' block '}'            { WhileLet $1 $4 $6 $8 $2 () }

-- 7.2.26 Return expressions (Done)

return_expr :: { Expr () }
return_expr
    : attribute_list return                 { Ret $1 Nothing () }
    | attribute_list return expr            { Ret $1 (Just $3) () }


-- Util (Done)

expr_list_rev :: { [Expr ()] }
expr_list_rev
    : {- Empty list -}                       { [] }
    | expr_list_rev ',' expr                 { $3 : $1 }

expr_list :: { [Expr ()] } 
expr_list : expr_list_rev                    { reverse $1 }

maybe_expr :: { Maybe (Expr ()) }
maybe_expr
    : {- Nothing -}                          { Nothing }
    | expr                                   { Just $1 }

ty_list_rev :: { [Ty ()] }
ty_list_rev
    : {- Empty list -}                       { [] }
    | ty_list_rev ',' ty                     { $3 : $1 }

ty_list :: { [Ty ()] } 
ty_list : ty_list_rev                        { reverse $1 }

maybe_lifetime :: { Maybe Lifetime }
maybe_lifetime
    : {- Nothing -}                          { Nothing }
    | lifetime ':'                           { Just $1 }

comma_m :: { () }
comma_m
    : {- Nothing -}                         { () }
    | ','                                   { () }
