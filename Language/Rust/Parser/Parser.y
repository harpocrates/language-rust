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
-- https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y
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
  '::'       { ModSep }
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

  -- types
  bool_ty    { IdentTok (Ident (Name "bool") _ _) }
  char_ty    { IdentTok (Ident (Name "char") _ _) }
  i8_ty      { IdentTok (Ident (Name "i8") _ _) }
  i16_ty     { IdentTok (Ident (Name "i16") _ _) }
  i32_ty     { IdentTok (Ident (Name "i32") _ _) }
  i64_ty     { IdentTok (Ident (Name "i64") _ _) }
  u8_ty      { IdentTok (Ident (Name "u8") _ _) }
  u16_ty     { IdentTok (Ident (Name "u16") _ _) }
  u32_ty     { IdentTok (Ident (Name "u32") _ _) }
  u64_ty     { IdentTok (Ident (Name "u64") _ _) }
  isize_ty   { IdentTok (Ident (Name "isize") _ _) }
  usize_ty   { IdentTok (Ident (Name "usize") _ _) }
  f32_ty     { IdentTok (Ident (Name "f32") _ _) }
  f64_ty     { IdentTok (Ident (Name "f64") _ _) }
  str_ty     { IdentTok (Ident (Name "str") _ _) }

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


-- 3.6 Paths (Done)
-- TODO double check this

type_path :: { Path () }
type_path : global type_path_segments_rev   { Path $1 (reverse $2) () }

expr_path :: { Path () }
expr_path : global expr_path_segments_rev   { Path $1 (reverse $2) () }

mod_path :: { Path () }
mod_path : global mod_path_segments_rev     { Path $1 (reverse $2) () }

qualified_type_path :: { (QSelf (), Path ()) }
qualified_type_path : '<' ty_sum as_type_path '>' '::' type_path_segments_rev  { (QSelf $2 (length (segments $3)), appendSegmentsToPath $3 (reverse $6)) }

qualified_expr_path :: { (QSelf (), Path ()) }
qualified_expr_path : '<' ty_sum as_type_path '>' '::' expr_path_segments_rev  { (QSelf $2 (length (segments $3)), appendSegmentsToPath $3 (reverse $6)) }

qualified_mod_path :: { (QSelf (), Path ()) }
qualified_mod_path  : '<' ty_sum as_type_path '>' '::' mod_path_segments_rev   { (QSelf $2 (length (segments $3)), appendSegmentsToPath $3 (reverse $6)) }

as_type_path :: { Path () }
as_type_path
    : as type_path                     { $2 }
    | {- Nothing -}                    { Path False [] () }

-- e.g. `a::b<T,U>::c<V,W>` or `a::b<T,U>::c(V) -> W` or `a::b<T,U>::c(V)`
type_path_segments_rev :: { [(Ident a, Maybe (PathParameters a))] }
type_path_segments_rev
    : type_path_segments_rev '::' ident angle_bracketed      { ($3, Just $4) : $1 }
    | type_path_segments_rev '::' ident parenthesized        { ($3, Just $4) : $1 }
    | type_path_segments_rev '::' ident                      { ($3, Nothing) : $1 }
    | {- Empty -}                                            { [] }

-- e.g. `a::b::<T,U>::c`
expr_path_segments_rev :: { [(Ident a, Maybe (PathParameters a))] }
expr_path_segments_rev
    : expr_path_segments_rev '::' ident '::' angle_bracketed { ($3, Just $5) : $1 }
    | expr_path_segments_rev '::' ident                      { ($3, Nothing) : $1 }
    | {- Empty -}                                            { [] }

-- e.g. `a::b::c`
mod_path_segments_rev :: { [(Ident a, Maybe (PathParameters a))] }
mod_path_segments_rev
    : mod_path_segments_rev '::' ident                      { ($3, Nothing) : $1 }
    | {- Empty -}   

global :: { Boolean }
global : '::'          { True }
       | {- Empty -}   { False }


-- Aka: generic_values_after_lt
angle_bracketed :: { PathParameters () }
angle_bracketed
    : '<' lifetimes ',' ty_sums ',' bindings comma_m '>'  { AngleBracketed $2 $4 $6 () }
    | '<' lifetimes ',' ty_sums              comma_m '>'  { AngleBracketed $2 $4 [] () }
    | '<' lifetimes ','             bindings comma_m '>'  { AngleBracketed $2 [] $4 () }
    | '<'               ty_sums ',' bindings comma_m '>'  { AngleBracketed [] $2 $4 () }
    | '<'                           bindings comma_m '>'  { AngleBracketed [] [] $2 () }
    | '<'               ty_sums              comma_m '>'  { AngleBracketed [] $2 [] () }
    | '<' lifetimes                          comma_m '>'  { AngleBracketed $2 [] [] () }
    | '<'                                            '>'  { AngleBracketed [] [] [] () }

parenthesized :: { PathParameters () }
parenthesized
    : '(' ty_sums comma_m ')' '->' ty                     { Parenthesized $2 (Just $6) () }
    : '(' ty_sums comma_m ')'                             { Parenthesized $2 Nothing   () }        
    : '(' ')' '->' ty                                     { Parenthesized [] (Just $4) () }
    : '(' ')'                                             { Parenthesized [] Nothing   () }


lifetimes :: { [Lifetime ()] }
lifetimes : lifetimes_rev        { reverse $1 }
lifetimes_rev
    : lifetime                   { [$1] }
    | lifetimes_rev ',' lifetime { $2 : $1 }

ty_sums :: { [Ty ()] }
ty_sums : ty_sums_rev            { reverse $1 }
ty_sums_rev
    : ty_sum                     { [$1] }
    | ty_sums_rev ',' ty_sum     { $2 : $1 }

bindings :: { [(Ident a, Ty a)] }
bindings : bindings_rev          { reverse $1 }
bindings_rev
    : ident '=' ty                   { [($1, $3)] }
    | bindings_rev ',' ident '=' ty  { ($3, $5) : $1 }


ty_sum :: { Ty () }
ty_sum
    : ty                                    { $1 }
    : ty '+' ty_param_bounds            { ObjectSum $1 $3 () }

ty_param_bounds :: { [TyParamBound ()] }
ty_param_bounds : ty_param_bounds_rev       { reverse $1 }

ty_param_bounds_rev :: { [TyParamBound ()] }
ty_param_bounds_rev
    : ty_param_bound                        { [$1] }
    | ty_param_bounds_rev '+' ty_param_bound { $3 : $1 }

ty_param_bound :: { TyParamBound () }
ty_param_bound
    : trait_bound_modifier lifetime_defs_rev trait_ref   { TraitTyParamBound (PolyTraitRef (reverse $2) $3 ()) $1 }
    | lifetime                                           { RegionTyParamBound $1 }

trait_bound_modifier :: { TriatBoundModifier }
trait_bound_modifier
    : '?'                                   { Maybe }
    | {- Empty -}                           { None } 

trait_ref :: { TraitRef () }
trait_ref : <parse_trait_ref()>

lifetime_defs_rev :: { [LifetimeDef ()] }
lifetime_defs_rev
    : {- Empty -}                           { [] }
    | for '<' lifetime ':'  '>'

-- 6.1 Items

item :: { Item () }
item : attribute_list vis item_kind        { Item (snd $3) $1 (fst $3) $2 () }

item_kind :: { (Ident (), ItemKind ()) }
item_kind
    : use  _ ';'        { Use (ViewPath a) } -- TODO
    | extern crate 
    | extern 



    : vis mod_item
    | fn_item
    | type_item
    | struct_item
    | enum_item
    | const_item
    | static_item
    | trait_item
    | impl_item
    | extern_block_item

vis :: { Visibility () }
vis : pub '(' crate ')'                     { CrateV () }
    | pub '(' mod_path ')'                  { RestrictedV $3 () }
    | pub                                   { PublicV () }
    | {- Nothing -}                         { InheritedV () }

view_path :: { ViewPath () }
view_path
    : mod_path


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

-- 7 Statements and expressions

-- 7.1 Statements
-- 7.1.1.1 Item declarations
-- 7.1.1 Declaration statements
-- 7.1.1.2 Variable declarations
-- 7.1.2 Expression statements

stmt :: { Stmt () }
stmt
    : item                                     { ItemStmt $1 () }
    | attribute_list let pat type_m init ';'   { Local $3 $4 $5 $1 () } 
    | expr ';'                                 { Semi $1 }
    | ';'

init :: { Expr () }
init : '=' expr        { Just $2 }
     | {- Nothing -}   { Nothing }

-- 7.2 Expressions

expr :: { Expr () }
expr
    : literal                               { $1 }
    | expr_path                             { $1 }
    | tuple_expr                            { $1 }
    | struct_expr                           { $1 }
    | block_expr                            { $1 }
    | method_call_expr                      { $1 }
    | field_expr                            { $1 }
    | array_expr                            { $1 }
    | idx_expr                              { $1 }
    | range_expr                            { $1 }
    | unop_expr                             { $1 }
    | binop_expr                            { $1 }
    | paren_expr                            { $1 }
    | call_expr                             { $1 }
    | lambda_expr                           { $1 }
    | while_expr                            { $1 }
    | loop_expr                             { $1 }
    | break_continue_expr                   { $1 }
    | for_expr                              { $1 }
    | if_expr                               { $1 }
    | match_expr                            { $1 }
    | if_let_expr                           { $1 }
    | while_let_expr                        { $1 }
    | return_expr                           { $1 }

literal :: { Lit a }
literal
    : byte                                  { Byte (parseByte $1) () }
    | char                                  { Char (parseChar $1) () }
    | string                                { Str (parseString $1) Cooked () }
    | bytestring                            { ByteStr (parseByteStr $1) () }
    | int                                   { Int (parseInt $1) (parseIntType $1) () } 
    | float                                 { Float $1 (parseFloatTy $1) () }
    | true                                  { Bool True () }
    | false                                 { Bool False () }

-- 7.2.0.1 Lvalues, rvalues and temporaries
-- FIXME: grammar?

-- 7.2.0.2 Moved and copied types
-- FIXME: Do we want to capture this in the grammar as different productions?

-- 7.2.3 Tuple expressions (Done)
-- 7.2.4 Unit expressions (Done)

tuple_expr :: { Expr () }
tuple_expr
    : attribute_list '(' expr ','
      expr_list comma_m ')'                 { TupExpr $1 ($3 : $5) () }
    | attribute_list '(' ')'                { TupExpr $1 [] }

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
    | attribute_list value as ty            { Cast $1 $2 $4 () }
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

-- 7.2.16 Lambda expressions (Done)

lambda_expr
    : attribute_list capture_by
      '|' args_list_rev '|' '->' ty '{'
         block
      '}'                                    { Closure $1 $2 (FnDecl (reverse $4) (Just $7) False ()) $9 ()}
    : attribute_list capture_by
      '|' args_list_rev '|' expr             { Closure $1 $2 (FnDecl (reverse $4) Nothing False ())
                                                       (Block [NoSemi $6] DefaultBlock ()) () }

capture_by :: { CaptureBy }
capture_by
    : {- Nothing -}                          { Ref }
    | move                                   { Value }

arg :: { Arg () }
arg : pat ':' ty                             { Arg $3 $1 () }
    : pat                                    { Arg (Infer ()) $1 () }

args_list_rev :: { [Arg ()] }
args_list_rev
    : {- Nothing -}                          { [] }
    | args_list_rev ',' arg                  { $2 : $1 }

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
break_continue_expr
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
      maybe_guard '=>' '{' block '}'        { Arm $1 (reverse $2) $3 (BlockExpr [] $6 ()) () }     '} -- TODO delete < (fixes syntax highlighting)

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

-- 8.1 Types (Done)
-- 8.1.3 Tuple types (Done)
-- 8.1.4 Array, and Slice types (Done)
--- TODO macro

ty :: { Ty () }
ty  : '(' ty ')'                            { ParenTy $2 () }
    | '(' ty_sum ',' ')'                    { TupTy [$2] () }
    | '(' ty_sum_list comma_m ')'           { TupTy $2 () }
    | '!'                                   { Never () }
    | '*' mutability ty                     { Ptr $2 $3 () }
    | '[' ty_sum ']'                        { Slice $2 () }
    | '[' ty_sum ':' expr ']'               { Array $2 $4 () }
    | '&' lifetime_m mutability ty          { RPtr $2 $3 $4 () }
    | for '<' lifetime_defs '>' bare_fn_ty  { BareFn (addLifetimes $5 $3) () }
    | for '<' lifetime_defs '>' type_path
      parse_ty_param_bounds_m               { PolyTraitRefTy (TraitTyParamBound (PolyTraitRefTy $3 (TraitRef $5)) None : $6) () }
    | impl parse_ty_param_bounds            { ImplTrait $2 () }
    | bare_fn_ty                            { $1 }
    | qualified_type_path                   { PathTy (Just (fst $1)) (snd $1) () }
    | type_path                             { PathTy Nothing $1 () }
 {- | type_path '!' -}
    | typeof '(' expr ')'                   { Typeof $3 () }
    | '_'                                   { Infer () }

parse_ty_param_bounds_m :: { [TyParamBound ()] }
parse_ty_param_bounds_m
    : '+' parse_ty_param_bounds             { $2 }
    : {- Nothing -}                         { [] }

mutability :: { Mutability }
mutability
    : mut                                   { Mutable }
    | const                                 { Immutable }

lifetime_m :: { Maybe (Lifetime ()) }
lifetime_m
    : lifetime                              { Just $1 }
    | {- Nothing -}                         { Nothing }

lifetime_defs_rev :: { [LifetimeDef ()] }
lifetime_defs_rev
    : lifetime_defs_rev ',' attribute_list lifetime                   { LifetimeDef $1 $3 [] () }
    | lifetime_defs_rev ',' attribute_list lifetime ':' lifetime_sum  { LifetimeDef $1 $3 $5 () }
    | {- Nothing -}                                               { [] }

lifetime_defs :: { [LifetimeDef ()] }
lifetime_defs : lifetime_defs_rev           { reverse $1 }


-- 8.1.8 Function types

bare_fn_ty :: { Ty () }
bare_fn_ty
    : unsafe abi fn '(' arg_list_general_rev '...' ')' ret_ty   { BareFn $1 $2 _ (FnDecl (reverse $5) $8 True) () }
    | unsafe abi fn '(' arg_list_general_rev comma_m ')' ret_ty { BareFn $1 $2 _ (FnDecl (reverse $5) $8 False) () }
    | unsafe abi fn '(' ')' ret_ty                              { BareFn $1 $2 _ (FnDecl [] $6 False) () }


arg_list_general_rev :: { [Arg ()] }
arg_list_general_rev
    | pat_m ty_sum                           { [Arg $2 $1 ()] }
    : arg_list_general_rev ',' pat_m ty_sum  { (Arg $4 $3 ()) : $1 }

pat_m :: { Pat () }
pat_m
    : pat ':'                                { $1 }
    | {- Nothing -}                          { IdentP (ByValue Immutable) invalidIdent Nothing () }

unsafety :: { Unsafety () }
unsafety
    : unsafe                                 { Unsafe }
    | {- Nothing -}                          { Normal }

abi :: { Abi }
abi : extern string                          { parseAbi $2 }
    | extern                                 { C }
    | {- Nothing -}                          { Rust }

ret_ty :: Maybe (Ty a)
ret_ty
    : '->' ty                               { Just $2 }
    | {- None -}                            { Nothing }

-- Util (Done)

expr_list_rev :: { [Expr ()] }
expr_list_rev
    : expr                                   { [$1] }
    | expr_list_rev ',' expr                 { $3 : $1 }

expr_list :: { [Expr ()] } 
expr_list
    : expr_list_rev                          { reverse $1 }
    | {- Empty list -}                       { [] }

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

ty_sum_list_rev :: { [Ty ()] }
ty_sum_list_rev
    : {- Empty list -}                       { [] }
    | ty_sum_list_rev ',' ty_sum             { $3 : $1 }

ty_sum_list :: { [Ty ()] } 
ty_sum_list : ty_sum_list_rev                        { reverse $1 }


maybe_lifetime :: { Maybe Lifetime }
maybe_lifetime
    : {- Nothing -}                          { Nothing }
    | lifetime ':'                           { Just $1 }

comma_m :: { () }
comma_m
    : {- Nothing -}                         { () }
    | ','                                   { () }

type_m :: { Ty () }
type_m
    : {- Nothing -}                         { Infer () }
    : ':' ty                                { $2 }

lifetime_sum_rev :: { [Lifetime ()] }
lifetime_sum_rev
    : {- Empty list -}                      { [] }
    | lifetime_sum_rev '+' lifetime         { $3 : $1 }

lifetime_sum :: { [Lifetime ()] }
lifetime_sum : lifetime_sum_rev             { reverse $1 }
