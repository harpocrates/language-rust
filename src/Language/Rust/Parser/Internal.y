{
{-|
Module      : Language.Rust.Parser.Internal
Description : Rust parser
Copyright   : (c) Alec Theriault, 2017-2019
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

The parsers in this file are all re-exported to 'Language.Rust.Parser' via the
'Language.Rust.Parser.Parse' class. The parsers are based off of:

  * primarily the reference @rustc@ [implementation][0]
  * some documentation on [rust-lang][2]
  * drawing a couple ideas from a slightly outdated [ANTLR grammar][1]

To get information about transition states and such, run

>  happy --info=happyinfo.txt -o /dev/null src/Language/Rust/Parser/Internal.y

  [0]: https://github.com/rust-lang/rust/blob/master/src/libsyntax/parse/parser.rs
  [1]: https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y
  [2]: https://doc.rust-lang.org/grammar.html
-}
{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Language.Rust.Parser.Internal (
  -- * Parsers
  parseAttr,
  parseBlock,
  parseExpr,
  parseGenericParam,
  parseGenerics,
  parseImplItem,
  parseItem,
  parseLit,
  parsePat,
  parsePath,
  parseSourceFile,
  parseStmt,
  parseTokenStream,
  parseTraitItem,
  parseTt,
  parseTy,
  parseWhereClause,
) where

import Prelude hiding                  ( fail )

import Language.Rust.Syntax

import Language.Rust.Data.Ident        ( Ident(..), mkIdent )
import Language.Rust.Data.Position

import Language.Rust.Parser.Lexer      ( lexNonSpace, lexShebangLine )
import Language.Rust.Parser.ParseMonad ( pushToken, getPosition, P, parseError )
import Language.Rust.Parser.Literals   ( translateLit )
import Language.Rust.Parser.Reversed

import Data.Either                     ( partitionEithers )
import Data.Foldable                   ( toList )
import Data.List                       ( (\\), isSubsequenceOf, sort )
import Data.Semigroup                  ( (<>) )
import Control.Monad.Fail              ( fail )

import Text.Read                       ( readMaybe )

import Data.List.NonEmpty              ( NonEmpty(..), (<|) )
import qualified Data.List.NonEmpty as N
}

-- in order to document the parsers, we have to alias them
%name parseLit lit
%name parseAttr export_attribute
%name parseTy ty
%name parsePat or_pat
%name parsePath ty_path
%name parseStmt stmt
%name parseExpr expr
%name parseItem mod_item
%name parseSourceFileContents source_file
%name parseBlock export_block
%name parseImplItem  impl_item
%name parseTraitItem trait_item
%name parseTt token_tree
%name parseTokenStream token_stream
%name parseGenericParam generic_param
%name parseWhereClause where_clause
%name parseGenerics generics

%tokentype { Spanned Token }
%lexer { lexNonSpace >>= } { Spanned Eof _ }
%monad { P } { >>= } { return }

%errorhandlertype explist
%error { expParseError }

%expect 0

%token

  -- Expression-operator symbols.
  '='            { Spanned Equal _ }
  '<'            { Spanned Less _ }
  '>'            { Spanned Greater _ }
  '!'            { Spanned Exclamation _ }
  '~'            { Spanned Tilde _ }

  '+'            { Spanned Plus _ }
  '-'            { Spanned Minus _ }
  '*'            { Spanned Star _ }
  '/'            { Spanned Slash _ }
  '%'            { Spanned Percent _ }
  '^'            { Spanned Caret _ }
  '&'            { Spanned Ampersand _ }
  '|'            { Spanned Pipe _ }

  -- Structural symbols.
  '@'            { Spanned At _ }
  '...'          { Spanned DotDotDot _ }
  '..='          { Spanned DotDotEqual _ }
  '..'           { Spanned DotDot _ }
  '.'            { Spanned Dot _ }
  ','            { Spanned Comma _ }
  ';'            { Spanned Semicolon _ }
  '::'           { Spanned ModSep _ }
  ':'            { Spanned Colon _ }
  '->'           { Spanned RArrow _ }
  '<-'           { Spanned LArrow _ }
  '=>'           { Spanned FatArrow _ }
  '#'            { Spanned Pound _ }
  '$'            { Spanned Dollar _ }
  '?'            { Spanned Question _ }
  '#!'           { Spanned Shebang _ }

  '||'           { Spanned PipePipe _ }
  '&&'           { Spanned AmpersandAmpersand _ }
  '>='           { Spanned GreaterEqual _ }
  '>>='          { Spanned GreaterGreaterEqual _ }
  '<<'           { Spanned LessLess _ }
  '>>'           { Spanned GreaterGreater _ }

  '=='           { Spanned EqualEqual _ }
  '!='           { Spanned NotEqual _ }
  '<='           { Spanned LessEqual _ }
  '<<='          { Spanned LessLessEqual _ }
  '-='           { Spanned MinusEqual _ }
  '&='           { Spanned AmpersandEqual _ }
  '|='           { Spanned PipeEqual _ }
  '+='           { Spanned PlusEqual _ }
  '*='           { Spanned StarEqual _ }
  '/='           { Spanned SlashEqual _ }
  '^='           { Spanned CaretEqual _ }
  '%='           { Spanned PercentEqual _ }

  '('            { Spanned (OpenDelim Paren) _ }
  '['            { Spanned (OpenDelim Bracket) _ }
  '{'            { Spanned (OpenDelim Brace) _ }
  ')'            { Spanned (CloseDelim Paren) _ }
  ']'            { Spanned (CloseDelim Bracket) _ }
  '}'            { Spanned (CloseDelim Brace) _ }

  -- Literals.
  byte           { Spanned (LiteralTok ByteTok{} _) _ }
  char           { Spanned (LiteralTok CharTok{} _) _ }
  int            { Spanned (LiteralTok IntegerTok{} _) _ }
  float          { Spanned (LiteralTok FloatTok{} _) _ }
  str            { Spanned (LiteralTok StrTok{} _) _ }
  byteStr        { Spanned (LiteralTok ByteStrTok{} _) _ }
  rawStr         { Spanned (LiteralTok StrRawTok{} _) _ }
  rawByteStr     { Spanned (LiteralTok ByteStrRawTok{} _) _ }

  -- Strict keywords used in the language
  as             { Spanned (IdentTok "as") _ }
  async          { Spanned (IdentTok "async") _ }
  await          { Spanned (IdentTok "await") _ }
  box            { Spanned (IdentTok "box") _ }
  break          { Spanned (IdentTok "break") _ }
  const          { Spanned (IdentTok "const") _ }
  continue       { Spanned (IdentTok "continue") _ }
  crate          { Spanned (IdentTok "crate") _ }
  dyn            { Spanned (IdentTok "dyn") _ }
  else           { Spanned (IdentTok "else") _ }
  enum           { Spanned (IdentTok "enum") _ }
  extern         { Spanned (IdentTok "extern") _ }
  false          { Spanned (IdentTok "false") _ }
  fn             { Spanned (IdentTok "fn") _ }
  for            { Spanned (IdentTok "for") _ }
  if             { Spanned (IdentTok "if") _ }
  impl           { Spanned (IdentTok "impl") _ }
  in             { Spanned (IdentTok "in") _ }
  let            { Spanned (IdentTok "let") _ }
  loop           { Spanned (IdentTok "loop") _ }
  macro          { Spanned (IdentTok "macro") _ }
  match          { Spanned (IdentTok "match") _ }
  mod            { Spanned (IdentTok "mod") _ }
  move           { Spanned (IdentTok "move") _ }
  mut            { Spanned (IdentTok "mut") _ }
  pub            { Spanned (IdentTok "pub") _ }
  ref            { Spanned (IdentTok "ref") _ }
  return         { Spanned (IdentTok "return") _ }
  Self           { Spanned (IdentTok "Self") _ }
  self           { Spanned (IdentTok "self") _ }
  static         { Spanned (IdentTok "static") _ }
  struct         { Spanned (IdentTok "struct") _ }
  super          { Spanned (IdentTok "super") _ }
  trait          { Spanned (IdentTok "trait") _ }
  true           { Spanned (IdentTok "true") _ }
  try            { Spanned (IdentTok "try") _ }
  type           { Spanned (IdentTok "type") _ }
  unsafe         { Spanned (IdentTok "unsafe") _ }
  use            { Spanned (IdentTok "use") _ }
  where          { Spanned (IdentTok "where") _ }
  while          { Spanned (IdentTok "while") _ }
  yield          { Spanned (IdentTok "yield") _ }

  -- Keywords reserved for future use
  abstract       { Spanned (IdentTok "abstract") _ }
  become         { Spanned (IdentTok "become") _ }
  do             { Spanned (IdentTok "do") _ }
  final          { Spanned (IdentTok "final") _ }
  override       { Spanned (IdentTok "override") _ }
  priv           { Spanned (IdentTok "priv") _ }
  proc           { Spanned (IdentTok "proc") _ }
  typeof         { Spanned (IdentTok "typeof") _ }
  unsized        { Spanned (IdentTok "unsized") _ }
  virtual        { Spanned (IdentTok "virtual") _ }

  -- Weak keywords, have special meaning only in specific contexts.
  default        { Spanned (IdentTok "default") _ }
  union          { Spanned (IdentTok "union") _ }
  auto           { Spanned (IdentTok "auto") _ }
  macro_rules    { Spanned (IdentTok "macro_rules") _ }

  -- Comments
  outerDoc       { Spanned (Doc _ Outer _) _ }
  innerDoc       { Spanned (Doc _ Inner _) _ }

  -- Identifiers.
  '_'            { Spanned (IdentTok "_") _ }
  IDENT          { Spanned IdentTok{} _ }

  -- Lifetimes.
  LIFETIME       { Spanned (LifetimeTok _) _ }

  -- Interpolated
  ntItem         { Spanned (Interpolated (NtItem $$)) _ }
  ntBlock        { Spanned (Interpolated (NtBlock $$)) _ }
  ntStmt         { Spanned (Interpolated (NtStmt $$)) _ }
  ntPat          { Spanned (Interpolated (NtPat $$)) _ }
  ntExpr         { Spanned (Interpolated (NtExpr $$)) _ }
  ntTy           { Spanned (Interpolated (NtTy $$)) _ }
  ntIdent        { Spanned (Interpolated (NtIdent _)) _ }
  ntPath         { Spanned (Interpolated (NtPath $$)) _ }
  ntTT           { Spanned (Interpolated (NtTT $$)) _ }
  ntArm          { Spanned (Interpolated (NtArm $$)) _ }
  ntImplItem     { Spanned (Interpolated (NtImplItem $$)) _ }
  ntTraitItem    { Spanned (Interpolated (NtTraitItem $$)) _ }
  ntGenerics     { Spanned (Interpolated (NtGenerics $$)) _ }
  ntWhereClause  { Spanned (Interpolated (NtWhereClause $$)) _ }
  ntArg          { Spanned (Interpolated (NtArg $$)) _ }
  ntLit          { Spanned (Interpolated (NtLit $$)) _ }

-- 'SEG' needs to be lower than '::' for path segments
%nonassoc SEG

-- 'mut' needs to be lower precedence than 'IDENT' so that in 'pat', something like "&mut x"
-- associates the "mut" to a refence pattern and not to the identifier pattern "x".
--
-- 'DEF' is for the empty case of 'def', which needs to _not_ be taken when there is a 'default'
-- token available.
--
-- 'EQ' is for differentiating the 'where ty' from 'where ty = ty' case in where clause
-- predicates, since the former needs to _not_ be taken when there is a '=' token available.
--
-- '::' is so that the remainder of mod paths in attributes are not gobbled as just raw tokens
%nonassoc mut DEF EQ '::'

-- These are all identifiers of sorts ('union' and 'default' are "weak" keywords)
%nonassoc IDENT ntIdent default union self Self super auto crate macro_rules

-- These are all very low precedence unary operators
%nonassoc async box return yield break continue for IMPLTRAIT LAMBDA

-- 'static' needs to have higher precedence than 'LAMBDA' so that statements starting in static get
-- considered as static items, and not a static lambda
%nonassoc static move

-- These are the usual arithmetic precedences. 'UNARY' is introduced here for '*', '!', '-', '&'
%right '=' '>>=' '<<=' '-=' '+=' '*=' '/=' '^=' '|=' '&=' '%='
%right '<-'
%nonassoc SINGLERNG
%nonassoc INFIXRNG
%nonassoc POSTFIXRNG
%nonassoc PREFIXRNG
%nonassoc '..' '...' '..='
%left '||'
%left '&&'
%nonassoc '==' '!=' '<' '>' '<=' '>='
%left '|'
%left '^'
%left '&'
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%nonassoc ':' as
%nonassoc UNARY

-- These are all generated precedence tokens.
--
--  * 'FIELD' for field access expressions (which bind less tightly than '.' for method calls)
--  * 'VIS' for adjusting the precedence of 'pub' compared to other visbility modifiers (see 'vis')
--  * 'PATH' boosts the precedences of paths in types and expressions
--  * 'WHERE' is for non-empty where clauses
--
%nonassoc FIELD VIS PATH WHERE NOSEMI

-- These are postfix operators.
%nonassoc '?' '.'

-- Delimiters have the highest precedence. 'ntBlock' counts as a delimiter since it always starts
-- and ends with '{' and '}'
%nonassoc '{' ntBlock '[' '(' '!' ';'

%%

-- Unwraps the IdentTok into just an Ident
-- For questionable reasons of backwards compatibility, 'union', and 'default' can be used
-- as identifiers, even if they are also keywords. They are "contextual" keywords.
--
-- Union's RFC: https://github.com/rust-lang/rfcs/blob/master/text/1444-union.md
ident :: { Spanned Ident }
  : ntIdent                       { fmap (\(Interpolated (NtIdent i)) -> i) $1 }
  | union                         { toIdent $1 }
  | default                       { toIdent $1 }
  | auto                          { toIdent $1 }
  | macro_rules                   { toIdent $1 }
  | IDENT                         { toIdent $1 }

ident_or_self :: { Spanned Ident }
  : ident                         { $1 }
  | self                          { toIdent $1 }

-- This should precede any '>' token which could be absorbed in a '>>', '>=', or '>>=' token. Its
-- purpose is to check if the lookahead token starts with '>' but contains more that. If that is
-- the case, it pushes two tokens, the first of which is '>'. We exploit the %% feature of threaded
-- lexers to discard what would have been the troublesome '>>', '>=', or '>>=' token.
gt :: { () }
  : {- empty -}   {%% \(Spanned tok s) ->
      let s' = nudge 1 0 s; s'' = nudge 0 (-1) s in
      case tok of
        GreaterGreater      -> pushToken (Spanned Greater s')      *> pushToken (Spanned Greater s'')
        GreaterEqual        -> pushToken (Spanned Equal s')        *> pushToken (Spanned Greater s'')
        GreaterGreaterEqual -> pushToken (Spanned GreaterEqual s') *> pushToken (Spanned Greater s'')
        _                   -> pushToken (Spanned tok s)
    }

-- This should precede any '|' token which could be absorbed in a '||' token. This works in the same
-- way as 'gt'.
pipe :: { () }
  : {- empty -}   {%% \(Spanned tok s) ->
      let s' = nudge 1 0 s; s'' = nudge 0 (-1) s in
      case tok of
        PipePipe -> pushToken (Spanned Pipe s') *> pushToken (Spanned Pipe s'')
        _        -> pushToken (Spanned tok s)
    }

-------------
-- Utility --
-------------

-- | One or more occurences of 'p'
some(p) :: { Reversed NonEmpty p }
  : some(p) p             { let Reversed xs = $1 in Reversed ($2 <| xs) }
  | p                     { [$1] }

-- | Zero or more occurences of 'p'
many(p) :: { [ p ] }
  : some(p)               { toList $1 }
  | {- empty -}           { [] }

-- | One or more occurences of 'p', seperated by 'sep'
sep_by1(p,sep) :: { Reversed NonEmpty p }
  : sep_by1(p,sep) sep p  { let Reversed xs = $1 in Reversed ($3 <| xs) }
  | p                     { [$1] }

-- | Zero or more occurrences of 'p', separated by 'sep'
sep_by(p,sep) :: { [ p ] }
  : sep_by1(p,sep)        { toList $1 }
  | {- empty -}           { [] }

-- | One or more occurrences of 'p', seperated by 'sep', optionally ending in 'sep'
sep_by1T(p,sep) :: { Reversed NonEmpty p }
  : sep_by1(p,sep) sep    { $1 }
  | sep_by1(p,sep)        { $1 }

-- | Zero or more occurences of 'p', seperated by 'sep', optionally ending in 'sep' (only if there
-- is at least one 'p')
sep_byT(p,sep) :: { [ p ] }
  : sep_by1T(p,sep)       { toList $1 }
  | {- empty -}           { [] }


--------------------------
-- Whole file
--------------------------

-- shebang is dealt with at the top level, outside Happy/Alex
source_file :: { ([Attribute Span],[Item Span]) }
  : inner_attrs many(mod_item)   { (toList $1, $2) }
  |             many(mod_item)   { ([],        $1) }


--------------------------
-- Attributes
--------------------------

outer_attribute :: { Attribute Span }
  : '#' '[' mod_path token_stream ']'         { Attribute Outer $3 $4 ($1 # $>) }
  | outerDoc                                  { let Spanned (Doc str _ l) x = $1 in SugaredDoc Outer l str x }

inner_attribute :: { Attribute Span }
  : '#' '!' '[' mod_path token_stream ']'     { Attribute Inner $4 $5 ($1 # $>) }
  | '#!'    '[' mod_path token_stream ']'     { Attribute Inner $3 $4 ($1 # $>) }
  | innerDoc                                  { let Spanned (Doc str _ l) x = $1 in SugaredDoc Inner l str x }

-- TODO: for some precedence related reason, using 'some' here doesn't work
inner_attrs :: { Reversed NonEmpty (Attribute Span) }
  : inner_attrs inner_attribute               { let Reversed xs = $1 in Reversed ($2 <| xs) }
  | inner_attribute                           { [$1] }


--------------
-- Literals --
--------------

lit :: { Lit Span }
  : ntLit             { $1 }
  | byte              {% lit $1 }
  | char              {% lit $1 }
  | int               {% lit $1 }
  | float             {% lit $1 }
  | true              {% lit $1 }
  | false             {% lit $1 }
  | string            { $1 }

string :: { Lit Span }
  : str               {% lit $1 }
  | rawStr            {% lit $1 }
  | byteStr           {% lit $1 }
  | rawByteStr        {% lit $1 }


-----------
-- Paths --
-----------

-- parse_qualified_path(PathStyle::Type)
-- qual_path :: Spanned (NonEmpty (Ident, GenericArgs Span)) -> P (Spanned (QSelf Span, Path Span))
qual_path(segs) :: { Spanned (QSelf Span, Path Span) }
  : '<' qual_path_suf(segs)
    { let Spanned x _ = $2 in Spanned x ($1 # $2) }
  | lt_ty_qual_path as ty_path '>' '::' segs
    {
      let Path g segsTy x = $3 in
      let idx = length segsTy + if g then 1 else 0 in
      Spanned (QSelf (unspan $1) idx, Path g (segsTy <> toList $6) x) ($1 # $>)
    }

-- Basically a qualified path, but ignoring the very first '<' token
qual_path_suf(segs) :: { Spanned (QSelf Span, Path Span) }
  : ty '>' '::' segs
    { Spanned (QSelf $1 0, Path False (toList $4) (spanOf $4)) ($1 # $>) }
  | ty as ty_path '>' '::' segs
    {
      let Path g segsTy x = $3 in
      let idx = length segsTy + if g then 1 else 0 in
      Spanned (QSelf $1 idx, Path g (segsTy <> toList $6) x) ($1 # $>)
    }

-- Usually qual_path_suf is for... type paths! This consumes these but with a starting '<<' token.
-- The underlying type has the right 'Span' (it doesn't include the very first '<', while the
-- 'Spanned' wrapper does)
lt_ty_qual_path :: { Spanned (Ty Span) }
  : '<<' qual_path_suf(ty_path_segments)
    {
      let (qself,path) = unspan $2 in
      Spanned (PathTy (Just qself) path (nudge 1 0 ($1 # $2))) ($1 # $2)
    }

-- parse_generic_args() but with the '<' '>'
generic_args :: { GenericArgs Span }
  : '<'                 sep_byT(generic_arg_elem,',')  gt '>'
    { let (a, c) = partitionEithers $2 in AngleBracketed a c ($1 # $>) }
  | lt_ty_qual_path ',' sep_by1T(generic_arg_elem,',') gt '>'
    { let (a, c) = partitionEithers (toList $3)
      in AngleBracketed (TypeArg (unspan $1) : a) c ($1 # $>) }
  | lt_ty_qual_path                                    gt '>'
    { AngleBracketed [TypeArg (unspan $1)] [] ($1 # $>) }

generic_arg :: { GenericArg Span }
  : lifetime                                   { LifetimeArg $1 }
  | ty                                         { TypeArg $1 }
  | unannotated_block                          { ConstArg $1 }
  | '-' lit_expr                               { ConstArg (Unary [] Neg $2 ($1 # $2)) }
  |     lit_expr                               { ConstArg $1 }

generic_constraint :: { AssocTyConstraint Span }
  : ident '=' ty                               { EqualityConstraint (unspan $1) $3 ($1 # $3) }
  | ident ':' sep_by1T(ty_param_bound_mod,'+') { BoundConstraint (unspan $1) (toNonEmpty $3) ($1 # $>) }

generic_arg_elem :: { Either (GenericArg Span) (AssocTyConstraint Span) }
  : generic_arg                                { Left $1 }
  | generic_constraint                         { Right $1 }

-- Type related:
-- parse_path(PathStyle::Type)
ty_path :: { Path Span }
  : ntPath                                     { $1 }
  | ty_path_segments                 %prec SEG { Path False (toList $1) (spanOf $1) }
  | '::' ty_path_segments            %prec SEG { Path True (toList $2) ($1 # $2) }

ty_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(ty_path_segments)                { $1 }

-- parse_ty_path_segments()
ty_path_segments :: { Reversed NonEmpty (PathSegment Span) }
  : path_segment_ident ty_generic_args
    { [PathSegment (unspan $1) $2 ($1 # $2)] }
  | ty_path_segments '::' path_segment_ident ty_generic_args
    { $1 <> [PathSegment (unspan $3) $4 ($3 # $4)] }
  | ty_path_segments '::' ty_generic_args1
    {%
      case (unsnoc $1) of
        (rst, PathSegment i Nothing x) ->
          let seg = PathSegment i (Just $3) (x # $3)
          in pure $ snoc rst seg
        _ -> fail "invalid path segment in type path"
    }

ty_generic_args  :: { Maybe (GenericArgs Span) }
  : ty_generic_args1                           { Just $1 }
  | {- empty -}                    %prec IDENT { Nothing }

ty_generic_args1 :: { GenericArgs Span }
  : generic_args                               { $1 }
  | '(' sep_byT(ty,',') ')'                    { Parenthesized $2 Nothing ($1 # $>) }
  | '(' sep_byT(ty,',') ')' '->' ty_no_plus    { Parenthesized $2 (Just $>) ($1 # $>) }

-- Expression related:
-- parse_path(PathStyle::Expr)
expr_path :: { Path Span }
  : ntPath                                     { $1 }
  | expr_path_segments                         { Path False (toList $1) (spanOf $1) }
  | '::' expr_path_segments                    { Path True (toList $2) ($1 # $2) }

expr_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(expr_path_segments)              { $1 }

-- parse_expr_path_segments()
expr_path_segments :: { Reversed NonEmpty (PathSegment Span) }
  : gen_expr_path_segments(path_segment_ident) { $1 }

gen_expr_path_segments(first_seg) :: { Reversed NonEmpty (PathSegment Span) }
  : first_seg
    { [PathSegment (unspan $1) Nothing (spanOf $1)] }
  | gen_expr_path_segments(first_seg) '::' path_segment_ident
    { $1 <> [PathSegment (unspan $3) Nothing (spanOf $3)] }
  | gen_expr_path_segments(first_seg) '::' generic_args
    {%
      case (unsnoc $1) of
        (rst, PathSegment i Nothing x) ->
          let seg = PathSegment i (Just $3) (x # $3)
          in pure $ snoc rst seg
        _ -> fail "invalid path segment in expression path"
    }


-- Mod related:
-- parse_path(PathStyle::Mod)
--
-- TODO: This is O(n^2) in the segment length! I haven't been able to make the grammar work out in
--       order to refactor this nicely
mod_path :: { Path Span  }
  : ntPath                                     { $1 }
  | path_segment_ident
    { Path False [PathSegment (unspan $1) Nothing (spanOf $1)] (spanOf $1) }
  | '::' path_segment_ident
    { Path True  [PathSegment (unspan $2) Nothing (spanOf $2)] ($1 # $>) }
  | mod_path '::' path_segment_ident
    {
      let Path g segs _ = $1 in
      Path g (segs <> [PathSegment (unspan $3) Nothing (spanOf $3) ]) ($1 # $3)
    }

-- parse_path_segment_ident
path_segment_ident :: { Spanned Ident }
  : ident                                      { $1 }
  | crate                                      { Spanned "crate" (spanOf $1) }
  | self                                       { Spanned "self" (spanOf $1) }
  | Self                                       { Spanned "Self" (spanOf $1) }
  | super                                      { Spanned "super" (spanOf $1) }


-----------
-- Types --
-----------

lifetime :: { Lifetime Span }
  : LIFETIME                         { let Spanned (LifetimeTok (Ident l _ _)) s = $1 in Lifetime l s }

-- parse_trait_ref()
trait_ref :: { TraitRef Span }
  : ty_path                          { TraitRef $1 }

-- parse_ty()
-- See https://github.com/rust-lang/rfcs/blob/master/text/0438-precedence-of-plus.md
-- All types, including trait types with plus
ty :: { Ty Span }
  : ty_no_plus                       { $1 }
  | sum_ty                           { $1 }

-- parse_ty_no_plus()
ty_no_plus :: { Ty Span }
  : ntTy                             { $1 }
  | no_for_ty                        { $1 }
  | for_ty_no_plus                   { $1 }

-- All types not starting with a '(' or '<'
ty_prim :: { Ty Span }
  : no_for_ty_prim                   { $1 }
  | for_ty_no_plus                   { $1 }
  | sum_ty                           { $1 }

-- The types that contain top-level sums
sum_ty :: { Ty Span }
  : poly_trait_ref_mod_bound '+'                                                 { TraitObject [$1] ($1 # $2) }
  | poly_trait_ref_mod_bound '+' sep_by1T(ty_param_bound_mod,'+')                { TraitObject ($1 <| toNonEmpty $3) ($1 # $3) }
  | impl ty_param_bound_mod '+'                                  %prec IMPLTRAIT { ImplTrait   [$2] ($1 # $>) }
  | impl ty_param_bound_mod '+' sep_by1T(ty_param_bound_mod,'+') %prec IMPLTRAIT { ImplTrait   ($2 <| toNonEmpty $4) ($1 # $>) }
  | dyn  ty_param_bound_mod '+'                                  %prec IMPLTRAIT { TraitObject [$2] ($1 # $>) }
  | dyn  ty_param_bound_mod '+' sep_by1T(ty_param_bound_mod,'+') %prec IMPLTRAIT { TraitObject ($2 <| toNonEmpty $4) ($1 # $>) }
  | bare_fn_ty(ret_ty_plus)       {  $1 }
  | for_lts bare_fn_ty(ret_ty_plus)                {
      let BareFn safety abi _ retTy s = $2 in
      BareFn safety abi (unspan $1) retTy ($1 # s)
    }

-- All (non-sum) types not starting with a 'for'
no_for_ty :: { Ty Span }
  : no_for_ty_prim                   { $1 }
  | '(' ')'                          { TupTy [] ($1 # $2) }
  | '(' ty ')'                       { ParenTy $2 ($1 # $3) }
  | '(' ty ',' ')'                   { TupTy [$2] ($1 # $4) }
  | '(' ty ',' sep_by1T(ty,',') ')'  { TupTy ($2 : toList $4) ($1 # $5) }
  | ty_qual_path                     { PathTy (Just (fst (unspan $1))) (snd (unspan $1)) (spanOf $1) }

-- All (non-sum) types not starting with a 'for', '(', or '<'
no_for_ty_prim :: { Ty Span }
  : '_'                              { Infer (spanOf $1) }
  | '!'                              { Never (spanOf $1) }
  | '[' ty ']'                       { Slice $2 ($1 # $3) }
  | '*' ty_no_plus                   { Ptr Immutable $2 ($1 # $2) }
  | '*' const ty_no_plus             { Ptr Immutable $3 ($1 # $3) }
  | '*' mut   ty_no_plus             { Ptr Mutable $3 ($1 # $3) }
  | '&'               ty_no_plus     { Rptr Nothing   Immutable $2 ($1 # $>) }
  | '&'  lifetime     ty_no_plus     { Rptr (Just $2) Immutable $3 ($1 # $>) }
  | '&'           mut ty_no_plus     { Rptr Nothing   Mutable   $3 ($1 # $>) }
  | '&'  lifetime mut ty_no_plus     { Rptr (Just $2) Mutable   $4 ($1 # $>) }
  | '&&'              ty_no_plus     { Rptr Nothing Immutable (Rptr Nothing   Immutable $2 (nudge 1 0 ($1 # $>))) ($1 # $>) }
  | '&&' lifetime     ty_no_plus     { Rptr Nothing Immutable (Rptr (Just $2) Immutable $3 (nudge 1 0 ($1 # $>))) ($1 # $>) }
  | '&&'          mut ty_no_plus     { Rptr Nothing Immutable (Rptr Nothing   Mutable   $3 (nudge 1 0 ($1 # $>))) ($1 # $>) }
  | '&&' lifetime mut ty_no_plus     { Rptr Nothing Immutable (Rptr (Just $2) Mutable   $4 (nudge 1 0 ($1 # $>))) ($1 # $>) }
  | ty_path               %prec PATH { PathTy Nothing $1 ($1 # $>) }
  | ty_mac                           { MacTy $1 ($1 # $>) }
  | bare_fn_ty(ret_ty_no_plus)       {  $1 }
  | typeof '(' expr ')'              { Typeof $3 ($1 # $>) }
  | '[' ty ';' expr ']'              { Array $2 $4 ($1 # $>) }
  | '?' trait_ref                    { TraitObject [TraitBound (PolyTraitRef [] $2 (spanOf $2)) Maybe ($1 # $2)] ($1 # $2) }
  | '?' for_lts trait_ref            { TraitObject [TraitBound (PolyTraitRef (unspan $2) $3 ($2 # $3)) Maybe ($1 # $3)] ($1 # $3) }
  | impl ty_param_bound_mod %prec IMPLTRAIT { ImplTrait [$2] ($1 # $>) }
  | dyn  ty_param_bound_mod %prec IMPLTRAIT { TraitObject [$2] ($1 # $>) }

-- All (non-sum) types starting with a 'for'
for_ty_no_plus :: { Ty Span }
  : for_lts bare_fn_ty(ret_ty_no_plus)                {
      let BareFn safety abi _ retTy s = $2 in
      BareFn safety abi (unspan $1) retTy ($1 # s)
    }
  | for_lts trait_ref                                 {
      let poly = PolyTraitRef (unspan $1) $2 ($1 # $2) in
      TraitObject [TraitBound poly None ($1 # $2)] ($1 # $2)
    }

-- Bare function types
bare_fn_ty(ret) :: { Ty Span }
  : unsafe extern abi fn fn_decl(arg_general, ret) { BareFn Unsafe $3 [] $> ($1 # $>) }
  | unsafe fn fn_decl(arg_general, ret)            { BareFn Unsafe Rust [] $> ($1 # $>) }
  | extern abi fn fn_decl(arg_general, ret)        { BareFn Normal $2 [] $> ($1 # $>) }
  | fn fn_decl(arg_general, ret)                   { BareFn Normal Rust [] $> ($1 # $>) }

-- An optional lifetime followed by an optional mutability
lifetime_mut :: { (Maybe (Lifetime Span), Mutability) }
  : lifetime mut  { (Just $1, Mutable) }
  | lifetime      { (Just $1, Immutable) }
  |          mut  { (Nothing, Mutable) }
  | {- empty -}   { (Nothing, Immutable) }

-- The argument list and return type in a function
fn_decl(arg, ret) :: { FnDecl Span }
  : '(' sep_by1(arg,',') ',' '...' ')' ret  { FnDecl (toList $2) $> True ($1 # $5 # $6) }
  | '(' sep_byT(arg,',')           ')' ret  { FnDecl $2 $> False ($1 # $3 # $4) }

-- Like 'fn_decl', but also accepting a self argument
fn_decl_with_self :: { FnDecl Span }
  : '(' arg_self ',' sep_by1T(arg_named,',') ')' ret_ty  { FnDecl ($2 : toList $4) $> False ($1 # $5 # $6) }
  | '(' arg_self ','                         ')' ret_ty  { FnDecl [$2] $> False ($1 # $3 # $4) }
  | '(' arg_self                             ')' ret_ty  { FnDecl [$2] $> False ($1 # $3 # $4) }
  | fn_decl(arg_named, ret_ty)                           { $1 }


-- parse_ty_param_bounds(BoundParsingMode::Bare) == sep_by1(ty_param_bound,'+')
ty_param_bound :: { GenericBound Span }
  : lifetime                { OutlivesBound $1 (spanOf $1) }
  | poly_trait_ref          { TraitBound $1 None (spanOf $1) }
  | '(' poly_trait_ref ')'  { TraitBound $2 None ($1 # $3) }

poly_trait_ref_mod_bound :: { GenericBound Span }
  : lifetime                { OutlivesBound $1 (spanOf $1) }
  | poly_trait_ref          { TraitBound $1 None (spanOf $1) }
  | '?' poly_trait_ref      { TraitBound $2 Maybe ($1 # $2) }

-- parse_ty_param_bounds(BoundParsingMode::Modified) == sep_by1(ty_param_bound_mod,'+')
ty_param_bound_mod :: { GenericBound Span }
  : ty_param_bound          { $1 }
  | '?' poly_trait_ref      { TraitBound $2 Maybe ($1 # $2) }

-- Sort of like parse_opt_abi() -- currently doesn't handle raw string ABI
abi :: { Abi }
  : str             {% case unspan $1 of
                         LiteralTok (StrTok "cdecl") Nothing ->              pure Cdecl
                         LiteralTok (StrTok "stdcall") Nothing ->            pure Stdcall
                         LiteralTok (StrTok "fastcall") Nothing ->           pure Fastcall
                         LiteralTok (StrTok "thiscall") Nothing ->           pure Thiscall
                         LiteralTok (StrTok "vectorcall") Nothing ->         pure Vectorcall
                         LiteralTok (StrTok "aapcs") Nothing ->              pure Aapcs
                         LiteralTok (StrTok "win64") Nothing ->              pure Win64
                         LiteralTok (StrTok "sysv64") Nothing ->             pure SysV64
                         LiteralTok (StrTok "ptx-kernel") Nothing ->         pure PtxKernel
                         LiteralTok (StrTok "msp430-interrupt") Nothing ->   pure Msp430Interrupt
                         LiteralTok (StrTok "x86-interrupt") Nothing ->      pure X86Interrupt
                         LiteralTok (StrTok "amdgpu-kernel") Nothing ->      pure AmdGpuKernel
                         LiteralTok (StrTok "Rust") Nothing ->               pure Rust
                         LiteralTok (StrTok "C") Nothing ->                  pure C
                         LiteralTok (StrTok "system") Nothing ->             pure System
                         LiteralTok (StrTok "rust-intrinsic") Nothing ->     pure RustIntrinsic
                         LiteralTok (StrTok "rust-call") Nothing ->          pure RustCall
                         LiteralTok (StrTok "platform-intrinsic") Nothing -> pure PlatformIntrinsic
                         LiteralTok (StrTok "unadjusted") Nothing ->         pure Unadjusted
                         _ -> parseError $1 {- "invalid ABI" -}
                    }
  | {- empty -}     { C }

-- parse_ret_ty(allow_plus = true)
ret_ty :: { Maybe (Ty Span) }
  : '->' ty                                          { Just $2 }
  | {- empty -}                                      { Nothing }

-- parse_ret_ty(allow_plus = false)
ret_ty_no_plus :: { Maybe (Ty Span) }
  : '->' ty_no_plus                                  { Just $2 }
  | {- empty -}                                      { Nothing }

-- Must contain a sum (
ret_ty_plus :: { Maybe (Ty Span) }
  : '->' sum_ty                                      { Just $2 }

-- parse_poly_trait_ref()
poly_trait_ref :: { PolyTraitRef Span }
  :         trait_ref                                { PolyTraitRef [] $1 (spanOf $1) }
  | for_lts trait_ref                                { PolyTraitRef (unspan $1) $2 ($1 # $2) }

-- parse_for_lts()
-- Unlike the Rust libsyntax version, this _requires_ the 'for'
for_lts :: { Spanned [GenericParam Span] }
  : for '<' sep_byT(lifetime_def,',') '>'            { Spanned $3 ($1 # $>) }

-- Definition of a lifetime: attributes can come before the lifetime, and a list of bounding
-- lifetimes can come after the lifetime.
lifetime_def :: { GenericParam Span }
  : many(outer_attribute) lifetime ':' sep_byT(lifetime,'+')  { LifetimeParam $1 $2 [ OutlivesBound l (spanOf l) | l <- $4 ] ($1 # $2 # $>) }
  | many(outer_attribute) lifetime                            { LifetimeParam $1 $2 [] ($1 # $2 # $>) }


---------------
-- Arguments --
---------------

-- Argument (requires a name / pattern, ie. @parse_arg_general(true)@)
arg_named :: { Arg Span }
  : ntArg             { $1 }
  | pat ':' ty        { Arg (Just $1) $3 ($1 # $3) }

-- Argument (does not require a name / pattern, ie. @parse_arg_general(false)@)
--
-- Remark: not all patterns are accepted (as per <https://github.com/rust-lang/rust/issues/35203>)
-- The details for which patterns _should_ be accepted fall into @is_named_argument()@.
arg_general :: { Arg Span }
  : ntArg              { $1 }
  |                ty  { Arg Nothing $1 (spanOf $1) }
  |      '_'   ':' ty  { Arg (Just (WildP (spanOf $1))) $3 ($1 # $3) }
  |      ident ':' ty  { Arg (Just (IdentP (ByValue Immutable) (unspan $1) Nothing (spanOf $1))) $3 ($1 # $3) }
  | mut  ident ':' ty  { Arg (Just (IdentP (ByValue Mutable) (unspan $2) Nothing (spanOf $2))) $4 ($1 # $4) }
  | '&'  '_'   ':' ty  { Arg (Just (RefP (WildP (spanOf $2)) Immutable ($1 # $2))) $4 ($1 # $4) }
  | '&'  ident ':' ty  { Arg (Just (RefP (IdentP (ByValue Immutable) (unspan $2) Nothing (spanOf $2)) Immutable ($1 # $2))) $4 ($1 # $4) }
  | '&&' '_'   ':' ty  { Arg (Just (RefP (RefP (WildP (spanOf $2)) Immutable (nudge 1 0 ($1 # $2))) Immutable ($1 # $2))) $4 ($1 # $4) }
  | '&&' ident ':' ty  { Arg (Just (RefP (RefP (IdentP (ByValue Immutable) (unspan $2) Nothing (spanOf $2)) Immutable (nudge 1 0 ($1 # $2))) Immutable ($1 # $2))) $4 ($1 # $4) }

-- Self argument (only allowed in trait/impl function signatures)
arg_self :: { Arg Span }
  :                  self { SelfValue Immutable ($1 # $>) }
  |              mut self { SelfValue Mutable ($1 # $>) }
  | '&'              self { SelfRegion Nothing   Immutable ($1 # $>) }
  | '&' lifetime     self { SelfRegion (Just $2) Immutable ($1 # $>) }
  | '&'          mut self { SelfRegion Nothing   Mutable   ($1 # $>) }
  | '&' lifetime mut self { SelfRegion (Just $2) Mutable   ($1 # $>) }
  |     self ':' ty       { SelfExplicit $3 Immutable ($1 # $>) }
  | mut self ':' ty       { SelfExplicit $4 Mutable ($1 # $>) }

-- Lambda expression argument
lambda_arg :: { Arg Span }
  : ntArg                         { $1 }
  | pat ':' ty                    { Arg (Just $1) $3 ($1 # $3) }
  | pat                           { Arg (Just $1) (Infer mempty) (spanOf $1) }


--------------
-- Patterns --
--------------

top_pat :: { Pat Span }
  : '|' or_pat  { $> }
  |     or_pat  { $> }

-- There is a funky trick going on here around 'IdentP'. When there is a binding mode (ie a 'mut' or
-- 'ref') or an '@' pattern, everything is fine, but otherwise there is no difference between an
-- expression variable path and a pattern. To deal with this, we intercept expression paths with
-- only one segment, no path parameters, and not global and turn them into identifier patterns.
pat :: { Pat Span }
  : ntPat                           { $1 }
  | '_'                             { WildP (spanOf $1) }
  | '..'                            { RestP (spanOf $1) }
  | '&' mut pat                     { RefP $3 Mutable ($1 # $3) }
  | '&' pat                         { RefP $2 Immutable ($1 # $2) }
  | '&&' mut pat                    { RefP (RefP $3 Mutable (nudge 1 0 ($1 # $3))) Immutable ($1 # $3) }
  | '&&' pat                        { RefP (RefP $2 Immutable (nudge 1 0 ($1 # $2))) Immutable ($1 # $2) }
  |     lit_expr                    { LitP $1 (spanOf $1) }
  | '-' lit_expr                    { LitP (Unary [] Neg $2 ($1 # $2)) ($1 # $2) }
  | box pat                         { BoxP $2 ($1 # $2) }
  | binding_mode1 ident '@' pat     { IdentP (unspan $1) (unspan $2) (Just $4) ($1 # $>) }
  | binding_mode1 ident             { IdentP (unspan $1) (unspan $2) Nothing ($1 # $>) }
  |               ident '@' pat     { IdentP (ByValue Immutable) (unspan $1) (Just $3) ($1 # $>) }
  | expr_path                       {
       case $1 of
         Path False [PathSegment i Nothing _] _ -> IdentP (ByValue Immutable) i Nothing (spanOf $1)
         _                                      -> PathP Nothing $1 (spanOf $1)
    }
  | expr_qual_path                  { PathP (Just (fst (unspan $1))) (snd (unspan $1)) ($1 # $>) }
  | lit_or_path '...' lit_or_path   { RangeP $1 $3 Closed ($1 # $>) }
  | lit_or_path '..=' lit_or_path   { RangeP $1 $3 Closed ($1 # $>) }
  | lit_or_path '..'  lit_or_path   { RangeP $1 $3 HalfOpen ($1 # $>) }
  | expr_path '{' pat_fields '}'    { let (fs,b) = $3 in StructP $1 fs b ($1 # $>) }
  | expr_path '(' sep_byT(or_pat,',') ')' { TupleStructP $1 $3 ($1 # $>) }
  | expr_mac                        { MacP $1 (spanOf $1) }
  | '[' sep_byT(or_pat,',') ']'     { SliceP $2 ($1 # $>) }
  | '('                         ')' { TupleP [] ($1 # $>) }
  | '(' sep_by1(or_pat,',') ',' ')' { TupleP (toList $2) ($1 # $>) }
  | '(' sep_by1(or_pat,',')     ')' {
      case toList $2 of
        l @ [RestP _] -> TupleP l ($1 # $>)   -- (..) is a tuple pattern
        [p] -> ParenP p ($1 # $>)             -- (<pat>) is parenthesis around a pattern
        l -> TupleP l ($1 # $>)               -- (<pat1>, <pat2>) is a tuple pattern
    }

-- Or-pattern
or_pat :: { Pat Span }
  : sep_by1(pat,'|')     {
      case toNonEmpty $1 of
        [p] -> p
        ps -> OrP ps (spanOf ps)
    }

-- Endpoints of range patterns
lit_or_path :: { Expr Span }
  : expr_path         { PathExpr [] Nothing $1 (spanOf $1) }
  | expr_qual_path    { PathExpr [] (Just (fst (unspan $1))) (snd (unspan $1)) (spanOf $1) }
  | '-' lit_expr      { Unary [] Neg $2 ($1 # $2) }
  |     lit_expr      { $1 }

-- Used in patterns for expression patterns
pat_fields :: { ([FieldPat Span], Bool) }
  : '..'                             { ([], True) }
  | sep_byT(pat_field,',')           { ($1, False) }
  | sep_by1(pat_field,',') ',' '..'  { (toList $1, True) }

pat_field :: { FieldPat Span }
  : many(outer_attribute)     binding_mode ident
    { FieldPat Nothing (IdentP (unspan $2) (unspan $3) Nothing (spanOf $1)) $1 ($1 # $2 # $3) }
  | many(outer_attribute) box binding_mode ident
    { FieldPat Nothing (BoxP (IdentP (unspan $3) (unspan $4) Nothing ($3 # $4)) ($2 # $4)) $1 ($1 # $2 # $4) }
  | many(outer_attribute)     binding_mode ident ':' or_pat
    { FieldPat (Just (unspan $3)) $5 $1 ($1 # $2 # $3 # $5) }


-- Used prefixing IdentP patterns (not empty - that is a seperate pattern case)
binding_mode1 :: { Spanned BindingMode }
  : ref mut                          { Spanned (ByRef Mutable) ($1 # $2) }
  | ref                              { Spanned (ByRef Immutable) (spanOf $1) }
  |     mut                          { Spanned (ByValue Mutable) (spanOf $1) }

-- Used for patterns for fields (includes the empty case)
binding_mode :: { Spanned BindingMode }
  : binding_mode1                    { $1 }
  | {- empty -}                      { Spanned (ByValue Immutable) mempty }


-----------------
-- Expressions --
-----------------

-- Expressions are a pain to parse. The Rust language places "restrictions" preventing certain
-- specific expressions from being valid in a certain context. Elsewhere in the parser, it will turn
-- on or off these restrictions. Unfortunately, that doesn't work well at all in a grammar, so we
-- have to define production rules for every combination of restrications used. Parametrized
-- productions make this a bit easier by letting us factor out the core expressions used everywhere.

-- Generalized expressions, parametrized by
--
--   * 'lhs' - expressions allowed on the left extremity of the term
--   * 'rhs' - expressions allowed on the right extremity of the term
--   * 'rhs2' - expressions optionally allowed on the right extremity of terms
--
-- Precedences are handled by Happy (right at the end of the token section)
gen_expression(lhs,rhs,rhs2) :: { Expr Span }
  -- immediate expressions
  : ntExpr                           { $1 }
  | lit_expr                         { $1 }
  | '[' sep_byT(expr,',') ']'        { Vec [] $2 ($1 # $>) }
  | '[' inner_attrs sep_byT(expr,',') ']' { Vec (toList $2) $3 ($1 # $>) }
  | '[' expr ';' expr ']'            { Repeat [] $2 $4 ($1 # $>) }
  | expr_mac                         { MacExpr [] $1 (spanOf $1) }
  | expr_path            %prec PATH  { PathExpr [] Nothing $1 (spanOf $1) }
  | expr_qual_path                   { PathExpr [] (Just (fst (unspan $1))) (snd (unspan $1)) (spanOf $1) }
  -- unary expressions
  | '*'      rhs     %prec UNARY     { Unary [] Deref $2 ($1 # $>) }
  | '!'      rhs     %prec UNARY     { Unary [] Not $2 ($1 # $>) }
  | '-'      rhs     %prec UNARY     { Unary [] Neg $2 ($1 # $>) }
  | '&'      rhs     %prec UNARY     { AddrOf [] Immutable $2 ($1 # $>) }
  | '&'  mut rhs     %prec UNARY     { AddrOf [] Mutable $3 ($1 # $>) }
  | '&&'     rhs     %prec UNARY     { AddrOf [] Immutable (AddrOf [] Immutable $2 (nudge 1 0 ($1 # $2))) ($1 # $2) }
  | '&&' mut rhs     %prec UNARY     { AddrOf [] Immutable (AddrOf [] Mutable $3 (nudge 1 0 ($1 # $3))) ($1 # $3) }
  | box rhs          %prec UNARY     { Box [] $2 ($1 # $>) }
  -- left-recursive
  | left_gen_expression(lhs,rhs,rhs2) { $1 }
  -- range expressions
  |     '..'  rhs2  %prec PREFIXRNG  { Range [] Nothing (Just $2) HalfOpen ($1 # $2) }
  |     '...' rhs2  %prec PREFIXRNG  { Range [] Nothing (Just $2) Closed ($1 # $2) }
  |     '..=' rhs2  %prec PREFIXRNG  { Range [] Nothing (Just $2) Closed ($1 # $2) }
  |     '..'        %prec SINGLERNG  { Range [] Nothing Nothing HalfOpen (spanOf $1) }
  |     '..='       %prec SINGLERNG  { Range [] Nothing Nothing Closed (spanOf $1) }
  -- low precedence prefix expressions
  | return                           { Ret [] Nothing (spanOf $1) }
  | return rhs2                      { Ret [] (Just $2) ($1 # $2) }
  | yield                            { Yield [] Nothing (spanOf $1) }
  | yield rhs2                       { Yield [] (Just $2) ($1 # $2) }
  | continue                         { Continue [] Nothing (spanOf $1) }
  | continue label                   { Continue [] (Just $2) ($1 # $2) }
  | break                            { Break [] Nothing Nothing (spanOf $1) }
  | break       rhs2                 { Break [] Nothing (Just $2) ($1 # $2) }
  | break label                      { Break [] (Just $2) Nothing ($1 # $2) }
  | break label rhs2     %prec break { Break [] (Just $2) (Just $3) ($1 # $3) }
  -- lambda expressions
  | lambda_prefix rhs   %prec LAMBDA { $1 Nothing $> }


-- Variant of 'gen_expression' which only constructs expressions starting with another expression.
left_gen_expression(lhs,rhs,rhs2) :: { Expr Span }
  : postfix_blockexpr(lhs)           { $1 }
  | lhs '[' expr ']'                 { Index [] $1 $3 ($1 # $>) }
  | lhs '(' sep_byT(expr,',') ')'    { Call [] $1 $3 ($1 # $>) }
  -- unary expressions
  | lhs ':' ty_no_plus               { TypeAscription [] $1 $3 ($1 # $>) }
  | lhs as ty_no_plus                { Cast [] $1 $3 ($1 # $>) }
  -- binary expressions
  | lhs '*' rhs                      { Binary [] MulOp $1 $3 ($1 # $>) }
  | lhs '/' rhs                      { Binary [] DivOp $1 $3 ($1 # $>) }
  | lhs '%' rhs                      { Binary [] RemOp $1 $3 ($1 # $>) }
  | lhs '+' rhs                      { Binary [] AddOp $1 $3 ($1 # $>) }
  | lhs '-' rhs                      { Binary [] SubOp $1 $3 ($1 # $>) }
  | lhs '<<' rhs                     { Binary [] ShlOp $1 $3 ($1 # $>) }
  | lhs '>>' rhs                     { Binary [] ShrOp $1 $3 ($1 # $>) }
  | lhs '&' rhs                      { Binary [] BitAndOp $1 $3 ($1 # $>) }
  | lhs '^' rhs                      { Binary [] BitXorOp $1 $3 ($1 # $>) }
  | lhs '|' rhs                      { Binary [] BitOrOp $1 $3 ($1 # $>) }
  | lhs '==' rhs                     { Binary [] EqOp $1 $3 ($1 # $>) }
  | lhs '!=' rhs                     { Binary [] NeOp $1 $3 ($1 # $>) }
  | lhs '<'  rhs                     { Binary [] LtOp $1 $3 ($1 # $>) }
  | lhs '>'  rhs                     { Binary [] GtOp $1 $3 ($1 # $>) }
  | lhs '<=' rhs                     { Binary [] LeOp $1 $3 ($1 # $>) }
  | lhs '>=' rhs                     { Binary [] GeOp $1 $3 ($1 # $>) }
  | lhs '&&' rhs                     { Binary [] AndOp $1 $3 ($1 # $>) }
  | lhs '||' rhs                     { Binary [] OrOp $1 $3 ($1 # $>) }
  -- range expressions
  | lhs '..'        %prec POSTFIXRNG { Range [] (Just $1) Nothing HalfOpen ($1 # $>) }
  | lhs '...'       %prec POSTFIXRNG { Range [] (Just $1) Nothing Closed ($1 # $>) }
  | lhs '..='       %prec POSTFIXRNG { Range [] (Just $1) Nothing Closed ($1 # $>) }
  | lhs '..'  rhs2  %prec INFIXRNG   { Range [] (Just $1) (Just $3) HalfOpen ($1 # $>) }
  | lhs '...' rhs2  %prec INFIXRNG   { Range [] (Just $1) (Just $3) Closed ($1 # $>) }
  | lhs '..=' rhs2  %prec INFIXRNG   { Range [] (Just $1) (Just $3) Closed ($1 # $>) }
  -- assignment expressions
  | lhs '=' rhs                      { Assign [] $1 $3 ($1 # $>) }
  | lhs '>>=' rhs                    { AssignOp [] ShrOp $1 $3 ($1 # $>) }
  | lhs '<<=' rhs                    { AssignOp [] ShlOp $1 $3 ($1 # $>) }
  | lhs '-=' rhs                     { AssignOp [] SubOp $1 $3 ($1 # $>) }
  | lhs '+=' rhs                     { AssignOp [] AddOp $1 $3 ($1 # $>) }
  | lhs '*=' rhs                     { AssignOp [] MulOp $1 $3 ($1 # $>) }
  | lhs '/=' rhs                     { AssignOp [] DivOp $1 $3 ($1 # $>) }
  | lhs '^=' rhs                     { AssignOp [] BitXorOp $1 $3 ($1 # $>) }
  | lhs '|=' rhs                     { AssignOp [] BitOrOp $1 $3 ($1 # $>) }
  | lhs '&=' rhs                     { AssignOp [] BitAndOp $1 $3 ($1 # $>) }
  | lhs '%=' rhs                     { AssignOp [] RemOp $1 $3 ($1 # $>) }

-- Postfix expressions that can come after an expression block, in a 'stmt'
--
--  * `{ 1 }[0]` isn't here because it is treated as `{ 1 }; [0]`
--  * `{ 1 }(0)` isn't here because it is treated as `{ 1 }; (0)`
--
postfix_blockexpr(lhs) :: { Expr Span }
  : lhs '?'                          { Try [] $1 ($1 # $>) }
  | lhs '.' await                    { Await [] $1 ($1 # $>) }
  | lhs '.' ident       %prec FIELD  { FieldAccess [] $1 (unspan $3) ($1 # $>) }
  | lhs '.' ident '(' sep_byT(expr,',') ')'
    { MethodCall [] $1 (PathSegment (unspan $3) Nothing (spanOf $3)) $5 ($1 # $>) }
  | lhs '.' ident '::' '<' sep_byT(ty,',') '>' '(' sep_byT(expr,',') ')'
    { MethodCall [] $1 (PathSegment (unspan $3) (Just (AngleBracketed (map TypeArg $6) [] ($5 # $7))) ($3 # $7)) $9 ($1 # $>) }
  | lhs '.' int                      {%
      lit $3 >>= \l -> case l of
        Int Dec i Unsuffixed _ -> pure (TupField [] $1 (fromIntegral i) ($1 # $3))
        _ -> parseError $3
    }

-- Then, we instantiate this general production into the following families of rules:
--
--   ['expr']               Most general class of expressions, no restrictions
--
--   ['nostruct_expr']      Forbids struct literals (for use as scrutinee of loops, ifs, etc).
--
--   ['nostructblock_expr'] Forbids struct literals and block expressions, but not block-like things
--                          like 'if' expressions or 'loop' expressions (for use as an optional
--                          right hand side of a `nostruct_expr` expression).
--
--   ['nonblock_expr']      Forbids expressions starting with blocks like '{ 1 } + 2', but not
--                          struct expressions since their "block" is at the end of the expression.
--
--   ['blockpostfix_expr']  Allows expressions starting with blocks (things such as '{ 1 }? + 1')
--                          but only when the leading block is itself a postfix expression.
--
-- There is also a later instantiation revolving around 'match' expressions, but it has some
-- different types.

expr :: { Expr Span }
  : gen_expression(expr,expr,expr)                                            { $1 }
  | paren_expr                                                                { $1 }
  | struct_expr                                                               { $1 }
  | block_expr                                                                { $1 }
  | lambda_expr_with_ty                                                       { $1 }

nostruct_expr :: { Expr Span }
  : gen_expression(nostruct_expr,nostruct_expr,nostructblock_expr)            { $1 }
  | paren_expr                                                                { $1 }
  | block_expr                                                                { $1 }
  | lambda_expr_with_ty                                                       { $1 }

nostructblock_expr :: { Expr Span }
  : gen_expression(nostructblock_expr,nostruct_expr,nostructblock_expr)       { $1 }
  | paren_expr                                                                { $1 }
  | block_like_expr                                                           { $1 }
  | annotated_block                                                           { $1 }
  | lambda_expr_with_ty                                                       { $1 }

nonblock_expr :: { Expr Span }
  : gen_expression(nonblock_expr,expr,expr)                                   { $1 }
  | paren_expr                                                                { $1 }
  | struct_expr                                                               { $1 }
  | lambda_expr_with_ty                                                       { $1 }

blockpostfix_expr :: { Expr Span }
  : postfix_blockexpr(block_like_expr)                                        { $1 }
  | postfix_blockexpr(vis_safety_block)                                       { $1 }
  | left_gen_expression(blockpostfix_expr,expr,expr)                          { $1 }


-- Finally, what remains is the more mundane definitions of particular types of expressions.

-- labels on loops
label :: { Label Span }
  : LIFETIME                         { let Spanned (LifetimeTok (Ident l _ _)) s = $1 in Label l s }

-- Literal expressions (composed of just literals)
lit_expr :: { Expr Span }
  : lit                                                 { Lit [] $1 (spanOf $1) }

-- An expression ending in a '{ ... }' block. Useful since "There is a convenience rule that allows
-- one to omit the separating ';' after 'if', 'match', 'loop', 'for', 'while'"
block_expr :: { Expr Span }
  : block_like_expr                                     { $1 }
  | unannotated_block                                   { $1 }
  | annotated_block                                     { $1 }

-- simple expression that is a block (no prefix, just '{ ... }')
unannotated_block :: { Expr Span }
  : inner_attrs_block                                   { let (as,b) = $1 in BlockExpr as b (spanOf b) }

-- `unsafe` and `async` block expressions
annotated_block :: { Expr Span }
  : unsafe inner_attrs_block
    { let (as, Block ss r x) = $> in BlockExpr as (Block ss Unsafe ($1 # x)) ($1 # x) }
  | async      inner_attrs_block                        { let (as,b) = $> in Async as Ref b ($1 # b) }
  | async move inner_attrs_block                        { let (as,b) = $> in Async as Value b ($1 # b) }

-- Any expression ending in a '{ ... }' block except those that cause ambiguities with statements
-- (eg. a block itself).
block_like_expr :: { Expr Span }
  : if_expr                                                          { $1 }
  |           loop                                inner_attrs_block  { let (as,b) = $> in Loop as b Nothing ($1 # b) }
  | label ':' loop                                inner_attrs_block  { let (as,b) = $> in Loop as b (Just $1) ($1 # b) }
  |           for top_pat in nostruct_expr        inner_attrs_block  { let (as,b) = $> in ForLoop as $2 $4 b Nothing ($1 # b) }
  | label ':' for top_pat in nostruct_expr        inner_attrs_block  { let (as,b) = $> in ForLoop as $4 $6 b (Just $1) ($1 # b) }
  |           while                 nostruct_expr inner_attrs_block  { let (as,b) = $> in While as $2 b Nothing ($1 # b) }
  | label ':' while                 nostruct_expr inner_attrs_block  { let (as,b) = $> in While as $4 b (Just $1) ($1 # b) }
  |           while let top_pat '=' nostruct_expr inner_attrs_block  { let (as,b) = $> in WhileLet as $3 $5 b Nothing ($1 # b) }
  | label ':' while let top_pat '=' nostruct_expr inner_attrs_block  { let (as,b) = $> in WhileLet as $5 $7 b (Just $1) ($1 # b) }
  | match nostruct_expr '{'                  '}'                     { Match [] $2 [] ($1 # $>) }
  | match nostruct_expr '{' inner_attrs      '}'                     { Match (toList $4) $2 [] ($1 # $>) }
  | match nostruct_expr '{'             arms '}'                     { Match [] $2 $4 ($1 # $>) }
  | match nostruct_expr '{' inner_attrs arms '}'                     { Match (toList $4) $2 $5 ($1 # $>) }
  | expr_path '!' '{' token_stream '}'                               { MacExpr [] (Mac $1 $4 ($1 # $>)) ($1 # $>) }
  | try inner_attrs_block                                            { let (as,b) = $> in TryBlock as b ($1 # b) }
--  | label ':'                                     inner_attrs_block  { let (as,b) = $> in BlockExpr as b (spanOf b) }


-- 'if' expressions are a bit special since they can have an arbitrary number of 'else if' chains.
if_expr :: { Expr Span }
  : if                 nostruct_expr block else_expr    { If [] $2 $3 $4 ($1 # $3 # $>) }
  | if let top_pat '=' nostruct_expr block else_expr    { IfLet [] $3 $5 $6 $7 ($1 # $6 # $>) }

else_expr :: { Maybe (Expr Span) }
  : else block                                          { Just (BlockExpr [] $2 (spanOf $2)) }
  | else if_expr                                        { Just $2 }
  | {- empty -}                                         { Nothing }

-- Match arms usually have to be seperated by commas (with an optional comma at the end). This
-- condition is loosened (so that there is no seperator needed) if the arm ends in a safe block.
arms :: { [Arm Span] }
  : ntArm                                               { [$1] }
  | ntArm arms                                          { $1 : $2 }
  | many(outer_attribute) top_pat arm_guard '=>' expr_arms { let (e,as) = $> in (Arm $1 $2 $3 e ($1 # $2 # e) : as) }

arm_guard :: { Maybe (Expr Span) }
  : {- empty -}  { Nothing }
  | if expr      { Just $2 }

-- Possibly more match arms, with a comma if present
comma_arms :: { [Arm Span] }
  : {- empty -}  { [] }
  | ','          { [] }
  | ',' arms     { $2 }

-- An expression followed by match arms. If there is a comma needed, it is added
expr_arms :: { (Expr Span, [Arm Span]) }
  : nonblock_expr                           comma_arms  { ($1, $2) }
  | blockpostfix_expr                       comma_arms  { ($1, $2) }
  | vis_safety_block                        comma_arms  { ($1, $2) }
  | vis_safety_block                              arms  { ($1, $2) }
  | block_like_expr                         comma_arms  { ($1, $2) }
  | block_like_expr                               arms  { ($1, $2) }

-- As per https://github.com/rust-lang/rust/issues/15701 (as of March 10 2017), the only way to have
-- attributes on expressions should be with inner attributes on a paren expression.
paren_expr :: { Expr Span }
  : '(' ')'                                             { TupExpr [] [] ($1 # $>) }
  | '(' inner_attrs ')'                                 { TupExpr (toList $2) [] ($1 # $>) }
  | '('             expr ')'                            { ParenExpr [] $2 ($1 # $>) }
  | '(' inner_attrs expr ')'                            { ParenExpr (toList $2) $3 ($1 # $>) }
  | '('             expr ',' ')'                        { TupExpr [] [$2] ($1 # $>) }
  | '(' inner_attrs expr ',' ')'                        { TupExpr (toList $2) [$3] ($1 # $>) }
  | '('             expr ',' sep_by1T(expr,',') ')'     { TupExpr [] ($2 : toList $4) ($1 # $>) }
  | '(' inner_attrs expr ',' sep_by1T(expr,',') ')'     { TupExpr (toList $2) ($3 : toList $5) ($1 # $>) }

-- A lambda expression with a return type. This is seperate from the `gen_expression` production
-- because the RHS _has_ to be a block.
lambda_expr_with_ty :: { Expr Span }
  : lambda_prefix '->' ty block                         { $1 (Just $3) (BlockExpr [] $> (spanOf $>)) }

-- Given a return type and a body, make a lambda expression
lambda_prefix :: { Maybe (Ty Span) -> Expr Span -> Expr Span }
  : static async move lambda_args
    { \retTy body -> Closure [] Value IsAsync  Immovable (FnDecl (unspan $4) retTy False (spanOf $4)) body ($1 # body) }
  |        async move lambda_args
    { \retTy body -> Closure [] Value IsAsync  Movable   (FnDecl (unspan $3) retTy False (spanOf $3)) body ($1 # body) }
  | static       move lambda_args
    { \retTy body -> Closure [] Value NotAsync Immovable (FnDecl (unspan $3) retTy False (spanOf $3)) body ($1 # body) }
  |              move lambda_args
    { \retTy body -> Closure [] Value NotAsync Movable   (FnDecl (unspan $2) retTy False (spanOf $2)) body ($1 # body) }
  | static async      lambda_args
    { \retTy body -> Closure [] Ref   IsAsync  Immovable (FnDecl (unspan $3) retTy False (spanOf $3)) body ($1 # body) }
  |        async      lambda_args
    { \retTy body -> Closure [] Ref   IsAsync  Movable   (FnDecl (unspan $2) retTy False (spanOf $2)) body ($1 # body) }
  | static            lambda_args
    { \retTy body -> Closure [] Ref   NotAsync Immovable (FnDecl (unspan $2) retTy False (spanOf $2)) body ($1 # body) }
  |                   lambda_args
    { \retTy body -> Closure [] Ref   NotAsync Movable   (FnDecl (unspan $1) retTy False (spanOf $1)) body ($1 # body) }

-- Lambda expression arguments block
lambda_args :: { Spanned [Arg Span] }
  : '||'                                                { Spanned [] (spanOf $1) }
  | '|' sep_byT(lambda_arg,',') pipe '|'                { Spanned $2 ($1 # $4) }


-- Struct expression literal
struct_expr :: { Expr Span }
  : expr_path struct_suffix                             { $2 $1 }

struct_suffix :: { Path Span -> Expr Span }
  : '{'                                    '..' expr '}'  { \p -> Struct [] p [] (Just $3) (p # $>) }
  | '{' inner_attrs                        '..' expr '}'  { \p -> Struct (toList $2) p [] (Just $4) (p # $>) }
  | '{'             sep_by1(field,',') ',' '..' expr '}'  { \p -> Struct [] p (toList $2) (Just $5) (p # $>) }
  | '{' inner_attrs sep_by1(field,',') ',' '..' expr '}'  { \p -> Struct (toList $2) p (toList $3) (Just $6) (p # $>) }
  | '{'             sep_byT(field,',')               '}'  { \p -> Struct [] p $2 Nothing (p # $>) }
  | '{' inner_attrs sep_byT(field,',')               '}'  { \p -> Struct (toList $2) p $3 Nothing (p # $>) }

field :: { Field Span }
  : many(outer_attribute) ident ':' expr  { Field (unspan $2) (Just $4) $1 ($1 # $2 # $4) }
  | many(outer_attribute) ident           { Field (unspan $2) Nothing $1 ($1 # $2) }

-- an expression block that won't cause conflicts with stmts
vis_safety_block :: { Expr Span }
  : vis unannotated_block        {% noVis $1 $> }
  | vis annotated_block          {% noVis $1 $> }

-- Like 'nonblock_expr', but for expressions which explicitly conflict with
-- other statements:
--
--   * expressions starting with `union`, `default`, or `auto` (conflicts with
--     union items, default impls, and auto impls)
--
--   * expressions starting with `unsafe`, `static` (conflicts with unsafe
--     functions and static items)
--
-- This rule is designed to mesh with the `stmt` rule (hence the `vis`/`noVis`
-- and `safety`/`noSafety` dance). It _only_ generates the problematic cases
-- laid out above. Generating more cases would imply a conflict with the other
-- expression rules for statements.
conflict_nonblock_expr :: { Expr Span }
  : conflict_expr_path  { (PathExpr [] Nothing $> (spanOf $>)) }
  | conflict_expr_path struct_suffix                 { ($2 $1) }
  | vis lambda_expr_with_ty                                        {% noVis $1 $2 }
  | vis lambda_prefix expr                %prec '::'               {% noVis $1 ($2 Nothing $>) }
  | left_gen_expression(conflict_nonblock_expr, expr, expr) { $1 }
  | conflict_expr_path '!' '(' token_stream ')'                    { MacExpr [] (Mac $1 $4 ($1 # $>)) ($1 # $>) }
  | conflict_expr_path '!' '[' token_stream ']'                    { MacExpr [] (Mac $1 $4 ($1 # $>)) ($1 # $>) }

-- Like 'block_like_expr', but for expressions which explicitly conflict with
-- other statements:
--
--   * expressions starting with `union`, `default`, or `auto` (conflicts with
--     union items, default impls, and auto impls)
--
-- See comments/motivation on 'conflict_nonblock_expr'
conflict_block_like_expr :: { Expr Span }
  : conflict_expr_path '!' '{' token_stream '}'                    { MacExpr [] (Mac $1 $4 ($1 # $>)) ($1 # $>) }

-- Problematic "contextual" identifiers
conflict_ident :: { Spanned Ident }
  : vis union         {% noVis $1 (Spanned "union" (spanOf $>)) }
  | vis default       {% noVis $1 (Spanned "default" (spanOf $>)) }
  | vis safety auto   {% noSafety $2 =<< noVis $1 (Spanned "auto" (spanOf $>)) }

-- Expression path starting with a problematic identifier
conflict_expr_path :: { Path Span }
  : gen_expr_path_segments(conflict_ident)       { Path False (toList $1) (spanOf $1) }


----------------
-- Statements --
----------------

stmt :: { Stmt Span }
  : ntStmt                                                        { $1 }
  | many(outer_attribute) let top_pat ':' ty initializer      ';' { Local $3 (Just $5) $6 $1 ($1 # $2 # $>) }
  | many(outer_attribute) let top_pat        initializer      ';' { Local $3 Nothing $4 $1 ($1 # $2 # $>) }
  | many(outer_attribute)          nonblock_expr              ';' { toStmt ($1 `addAttrs` $2) True  False ($1 # $2 # $3) }
  | many(outer_attribute) conflict_nonblock_expr              ';' { toStmt ($1 `addAttrs` $2) True  False ($1 # $2 # $3) }
  | many(outer_attribute)          block_like_expr            ';' { toStmt ($1 `addAttrs` $2) True  True  ($1 # $2 # $3) }
  | many(outer_attribute) conflict_block_like_expr            ';' { toStmt ($1 `addAttrs` $2) False True  ($1 # $2 # $3) }
  | many(outer_attribute)          block_like_expr  %prec NOSEMI  { toStmt ($1 `addAttrs` $2) False True  ($1 # $2) }
  | many(outer_attribute) conflict_block_like_expr  %prec NOSEMI  { toStmt ($1 `addAttrs` $2) False True  ($1 # $2) }
  | many(outer_attribute) blockpostfix_expr                   ';' { toStmt ($1 `addAttrs` $2) True  True  ($1 # $2 # $3) }
  | many(outer_attribute) vis_safety_block                    ';' { toStmt ($1 `addAttrs` $2) True True ($1 # $2 # $>) }
  | many(outer_attribute) vis_safety_block          %prec NOSEMI  { toStmt ($1 `addAttrs` $2) False True ($1 # $2) }
  | item                                                      ';' { ItemStmt $1 ($1 # $2) }
  | item                                            %prec NOSEMI  { ItemStmt $1 (spanOf $1) }
  | many(outer_attribute) macro_rules '!' ident '[' token_stream ']' ';'
    { let s = $1 # $2 # $> in ItemStmt (MacroDef $1 InheritedV (unspan $4) $6 s) s }
  | many(outer_attribute) macro_rules '!' ident '(' token_stream ')' ';'
    { let s = $1 # $2 # $> in ItemStmt (MacroDef $1 InheritedV (unspan $4) $6 s) s }
  | many(outer_attribute) macro_rules '!' ident '{' token_stream '}' %prec NOSEMI
    { let s = $1 # $2 # $> in ItemStmt (MacroDef $1 InheritedV (unspan $4) $6 s) s }
  | many(outer_attribute) macro_rules '!' ident '{' token_stream '}' ';'
    { let s = $1 # $2 # $> in ItemStmt (MacroDef $1 InheritedV (unspan $4) $6 s) s }
  | many(outer_attribute) macro_rules '!'       '[' token_stream ']' ';'
    { MacStmt (Mac (macroRulesPath $2) $5 ($2 # $6)) SemicolonMac $1 ($1 # $2 # $>) }
  | many(outer_attribute) macro_rules '!'       '{' token_stream '}'
    { MacStmt (Mac (macroRulesPath $2) $5 ($2 # $6)) BracesMac    $1 ($1 # $2 # $>) }
  | many(outer_attribute) macro_rules '!'       '(' token_stream ')' ';'
    { MacStmt (Mac (macroRulesPath $2) $5 ($2 # $6)) SemicolonMac $1 ($1 # $2 # $>) }
  | ';'                                                      { StandaloneSemi (spanOf $1) }


-- List of statements where the last statement might be a no-semicolon statement.
stmts_possibly_no_semi :: { [Stmt Span] }
  : stmt stmts_possibly_no_semi                            { $1 : $2 }
  | stmt                                                   { [$1] }
  | many(outer_attribute)          nonblock_expr           { [toStmt ($1 `addAttrs` $2) False False ($1 # $2)] }
  | many(outer_attribute) conflict_nonblock_expr           { [toStmt ($1 `addAttrs` $2) False False ($1 # $2)] }
  | many(outer_attribute) blockpostfix_expr                { [toStmt ($1 `addAttrs` $2) False True  ($1 # $2)] }

initializer :: { Maybe (Expr Span) }
  : '=' many(outer_attribute) expr                         { Just ($2 `addAttrs` $3) }
  | {- empty -}                                            { Nothing }

block :: { Block Span }
  : ntBlock                                                { $1 }
  | '{' '}'                                                { Block [] Normal ($1 # $>) }
  | '{' stmts_possibly_no_semi '}'                         { Block $2 Normal ($1 # $>) }

inner_attrs_block :: { ([Attribute Span], Block Span) }
  : block                                                  { ([], $1) }
  | '{' inner_attrs '}'                                    { (toList $2, Block [] Normal ($1 # $>)) }
  | '{' inner_attrs stmts_possibly_no_semi '}'             { (toList $2, Block $3 Normal ($1 # $>)) }


-----------
-- Items --
-----------

item :: { Item Span }
  : many(outer_attribute) vis static     ident ':' ty '=' expr ';'
    { Static $1 (unspan $2) (unspan $4) $6 Immutable $8 ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis static mut ident ':' ty '=' expr ';'
    { Static $1 (unspan $2) (unspan $5) $7 Mutable $9 ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis const ident ':' ty '=' expr ';'
    { ConstItem $1 (unspan $2) (unspan $4) $6 $8 ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis const '_'   ':' ty '=' expr ';'
    { ConstItem $1 (unspan $2) "_" $6 $8 ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis type ident generics where_clause '=' ty ';'
    { TyAlias $1 (unspan $2) (unspan $4) $8 ($5 `withWhere` $6) ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis use use_tree ';'
    { Use $1 (unspan $2) $4 ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis safety extern crate ident_or_self ';'
    {% noSafety $3 (ExternCrate $1 (unspan $2) (unspan $6) Nothing ($1 # $2 # $4 # $>)) }
  | many(outer_attribute) vis safety extern crate ident_or_self as ident ';'
    {% noSafety $3 (ExternCrate $1 (unspan $2) (unspan $8) (Just (unspan $6)) ($1 # $2 # $4 # $>)) }
  | many(outer_attribute) vis safety extern crate ident_or_self as '_' ';'
    {% noSafety $3 (ExternCrate $1 (unspan $2) "_"         (Just (unspan $6)) ($1 # $2 # $4 # $>)) }
  | many(outer_attribute) vis const safety  fn ident generics fn_decl(arg_named, ret_ty) where_clause inner_attrs_block
    { Fn ($1 ++ fst $>) (unspan $2) (unspan $6) $8 (FnHeader (unspan $4) NotAsync Const Rust (spanOf $4))    ($7 `withWhere` $9) (snd $>) ($1 # $2 # $3 # snd $>) }
  | many(outer_attribute) vis async safety  fn ident generics fn_decl(arg_named, ret_ty) where_clause inner_attrs_block
    { Fn ($1 ++ fst $>) (unspan $2) (unspan $6) $8 (FnHeader (unspan $4) IsAsync NotConst Rust (spanOf $4))    ($7 `withWhere` $9) (snd $>) ($1 # $2 # $3 # snd $>) }
  | many(outer_attribute) vis safety extern abi fn ident generics fn_decl(arg_named, ret_ty) where_clause inner_attrs_block
    { Fn ($1 ++ fst $>) (unspan $2) (unspan $7) $9 (FnHeader (unspan $3) NotAsync NotConst $5 ($3 # $4))     ($8 `withWhere` $10) (snd $>) ($1 # $2 # $3 # $4 # snd $>) }
  | many(outer_attribute) vis safety            fn ident generics fn_decl(arg_named, ret_ty) where_clause inner_attrs_block
    { Fn ($1 ++ fst $>) (unspan $2) (unspan $5) $7 (FnHeader (unspan $3) NotAsync NotConst Rust (spanOf $3)) ($6 `withWhere` $8) (snd $>) ($1 # $2 # $3 # $4 # snd $>) }
  | many(outer_attribute) vis mod ident ';'
    { Mod $1 (unspan $2) (unspan $4) Nothing ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis mod ident '{'             many(mod_item) '}'
    { Mod $1 (unspan $2) (unspan $4) (Just $6) ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis mod ident '{' inner_attrs many(mod_item) '}'
    { Mod ($1 ++ toList $6) (unspan $2) (unspan $4) (Just $7) ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis safety extern abi '{'             many(foreign_item) '}'
    {% noSafety $3 (ForeignMod $1 (unspan $2) $5 $7 ($1 # $2 # $4 # $>)) }
  | many(outer_attribute) vis safety extern abi '{' inner_attrs many(foreign_item) '}'
    {% noSafety $3 (ForeignMod ($1 ++ toList $7) (unspan $2) $5 $8 ($1 # $2 # $4 # $>)) }
  | many(outer_attribute) vis struct ident generics struct_decl_args
    { StructItem $1 (unspan $2) (unspan $4) (snd $6) ($5 `withWhere` fst $6) ($1 # $2 # $3 # snd $>) }
  | many(outer_attribute) vis union ident generics struct_decl_args
    { Union $1 (unspan $2) (unspan $4) (snd $6) ($5 `withWhere` fst $6) ($1 # $2 # $3 # snd $>) }
  | many(outer_attribute) vis enum ident generics where_clause '{' sep_byT(enum_def,',') '}'
    { Enum $1 (unspan $2) (unspan $4) $8 ($5 `withWhere` $6) ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis safety trait ident generics ':' sep_byT(ty_param_bound,'+') where_clause '{' many(trait_item) '}'
    { Trait $1 (unspan $2) (unspan $5) False (unspan $3) ($6 `withWhere` $9) $8 $11 ($1 # $2 # $3 # $4 # $>) }
  | many(outer_attribute) vis safety trait ident generics where_clause '{' many(trait_item) '}'
    { Trait $1 (unspan $2) (unspan $5) False (unspan $3) ($6 `withWhere` $7) [] $9 ($1 # $2 # $3 # $4 # $>) }
  | many(outer_attribute) vis safety auto trait ident generics ':' sep_byT(ty_param_bound,'+') where_clause '{' many(trait_item) '}'
    { Trait $1 (unspan $2) (unspan $6) True (unspan $3) ($7 `withWhere` $10) $9 $12 ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) vis safety auto trait ident generics where_clause '{' many(trait_item) '}'
    { Trait $1 (unspan $2) (unspan $6) True (unspan $3) ($7 `withWhere` $8) [] $10 ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) vis safety trait ident generics '=' sep_byT(ty_param_bound_mod,'+') where_clause ';'
    {% noSafety $3 (TraitAlias $1 (unspan $2) (unspan $5) ($6 `withWhere` $9) $8 ($1 # $2 # $3 # $>)) }
  | many(outer_attribute) vis         safety impl generics ty_prim              where_clause '{' impl_items '}'
    { Impl ($1 ++ fst $9) (unspan $2) Final (unspan $3) Positive ($5 `withWhere` $7) Nothing $6 (snd $9) ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) vis default safety impl generics ty_prim              where_clause '{' impl_items '}'
    { Impl ($1 ++ fst $10) (unspan $2) Default (unspan $4) Positive ($6 `withWhere` $8) Nothing $7 (snd $10) ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) vis         safety impl generics '(' ty_no_plus ')'   where_clause '{' impl_items '}'
    { Impl ($1 ++ fst $11) (unspan $2) Final (unspan $3) Positive ($5 `withWhere` $9) Nothing (ParenTy $7 ($6 # $8)) (snd $11) ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) vis default safety impl generics '(' ty_no_plus ')'   where_clause '{' impl_items '}'
    { Impl ($1 ++ fst $12) (unspan $2) Default (unspan $4) Positive ($6 `withWhere` $10) Nothing (ParenTy $8 ($7 # $9)) (snd $12) ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) vis         safety impl generics '!' trait_ref for ty where_clause '{' impl_items '}'
    { Impl ($1 ++ fst $12) (unspan $2) Final (unspan $3) Negative ($5 `withWhere` $10) (Just $7) $9 (snd $12) ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) vis default safety impl generics '!' trait_ref for ty where_clause '{' impl_items '}'
    { Impl ($1 ++ fst $13) (unspan $2) Default (unspan $4) Negative ($6 `withWhere` $11) (Just $8) $10 (snd $13) ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) vis         safety impl generics     trait_ref for ty where_clause '{' impl_items '}'
    { Impl ($1 ++ fst $11) (unspan $2) Final (unspan $3) Positive ($5 `withWhere` $9) (Just $6) $8 (snd $11) ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) vis default safety impl generics     trait_ref for ty where_clause '{' impl_items '}'
    { Impl ($1 ++ fst $12) (unspan $2) Default (unspan $4) Positive ($6 `withWhere` $10) (Just $7) $9 (snd $12) ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) vis macro ident '(' token_stream ')' '{' token_stream '}'
    { MacroDef $1 (unspan $2) (unspan $4) (Stream [ Tree (Delimited ($5 # $7) Paren $6)
                                                  , Tree (Token mempty FatArrow)
                                                  , Tree (Delimited ($8 # $10) Brace $9) ]) ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis macro ident                      '{' token_stream '}'
    { MacroDef $1 (unspan $2) (unspan $4) $6 ($1 # $2 # $3 # $>) }

-- Most general type of item
mod_item :: { Item Span }
  : ntItem                                         { $1 }
  | item                                           { $1 }
  | many(outer_attribute) mod_mac                  { MacItem $1 $2 ($1 # $2) }
  | many(outer_attribute) conflict_mod_path '!' '[' token_stream ']' ';'  { MacItem $1 (Mac $2 $5 ($2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) conflict_mod_path '!' '{' token_stream '}'      { MacItem $1 (Mac $2 $5 ($2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) conflict_mod_path '!' '(' token_stream ')' ';'  { MacItem $1 (Mac $2 $5 ($2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) macro_rules '!' ident '[' token_stream ']' ';'
    { MacroDef $1 InheritedV (unspan $4) $6 ($1 # $2 # $>) }
  | many(outer_attribute) macro_rules '!' ident '(' token_stream ')' ';'
    { MacroDef $1 InheritedV (unspan $4) $6 ($1 # $2 # $>) }
  | many(outer_attribute) macro_rules '!' ident '{' token_stream '}'
    { MacroDef $1 InheritedV (unspan $4) $6 ($1 # $2 # $>) }
  | many(outer_attribute) macro_rules '!'       '[' token_stream ']' ';'
    { MacItem $1 (Mac (macroRulesPath $2) $5 ($2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) macro_rules '!'       '{' token_stream '}'
    { MacItem $1 (Mac (macroRulesPath $2) $5 ($2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) macro_rules '!'       '(' token_stream ')' ';'
    { MacItem $1 (Mac (macroRulesPath $2) $5 ($2 # $>)) ($1 # $2 # $>) }

-- Module path starting with a problematic identifier
conflict_mod_path :: { Path Span  }
  : conflict_ident
    { Path False [PathSegment (unspan $1) Nothing (spanOf $1)] (spanOf $1) }
  | conflict_mod_path '::' path_segment_ident
    {
      let Path g segs _ = $1 in
      Path g (segs <> [PathSegment (unspan $3) Nothing (spanOf $3) ]) ($1 # $3)
    }

foreign_item :: { ForeignItem Span }
  : many(outer_attribute) vis static     ident ':' ty ';'
    { ForeignStatic $1 (unspan $2) (unspan $4) $6 Immutable ($1 # $2 # $>) }
  | many(outer_attribute) vis static mut ident ':' ty ';'
    { ForeignStatic $1 (unspan $2) (unspan $5) $7 Mutable ($1 # $2 # $>) }
  | many(outer_attribute) vis fn ident generics fn_decl(arg_named, ret_ty) where_clause ';'
    { ForeignFn $1 (unspan $2) (unspan $4) $6 ($5 `withWhere` $7) ($1 # $2 # $>) }
  | many(outer_attribute) vis type ident ';'
    { ForeignTy $1 (unspan $2) (unspan $4) ($1 # $2 # $>) }
  | many(outer_attribute) mod_mac
    { ForeignMac $1 $2 ($1 # $>) }

-- parse_generics
-- Leaves the WhereClause empty
generics :: { Generics Span }
  : generics1                             { $1 }
  | {- empty -}                           { Generics [] (WhereClause [] mempty) mempty }

generics1 :: { Generics Span }
  : ntGenerics                            { $1 }
  | '<' sep_byT(generic_param,',') gt '>' { Generics $2 (WhereClause [] mempty) ($1 # $>) }

ty_param :: { GenericParam Span }
  : many(outer_attribute) ident                                             { TypeParam $1 (unspan $2) [] Nothing ($1 # $>) }
  | many(outer_attribute) ident ':' sep_byT(ty_param_bound_mod,'+')         { TypeParam $1 (unspan $2) $4 Nothing ($1 # $>) }
  | many(outer_attribute) ident                                     '=' ty  { TypeParam $1 (unspan $2) [] (Just $>) ($1 # $>) }
  | many(outer_attribute) ident ':' sep_byT(ty_param_bound_mod,'+') '=' ty  { TypeParam $1 (unspan $2) $4 (Just $>) ($1 # $>) }

const_param :: { GenericParam Span }
  : many(outer_attribute) const ident ':' ty                                 { ConstParam $1 (unspan $3) $5 ($1 # $2 # $>) }

generic_param :: { GenericParam Span }
  : ty_param     { $1 }
  | lifetime_def { $1 }
  | const_param  { $1 }

struct_decl_args :: { (WhereClause Span, VariantData Span) }
  : where_clause ';'                                         { ($1, UnitD ($1 # $>)) }
  | where_clause '{' sep_byT(struct_decl_field,',') '}'      { ($1, StructD $3 ($1 # $>)) }
  | '(' sep_byT(tuple_decl_field,',') ')' where_clause ';'   { ($4, TupleD $2 ($1 # $>)) }

struct_decl_field :: { StructField Span }
  : many(outer_attribute) vis ident ':' ty                   { StructField (Just (unspan $3)) (unspan $2) $5 $1 ($1 # $2 # $5) }

tuple_decl_field :: { StructField Span }
  : many(outer_attribute) vis ty                             { StructField Nothing (unspan $2) $3 $1 ($1 # $2 # $3) }

enum_def :: { Variant Span }
  : many(outer_attribute) ident '{' sep_byT(struct_decl_field,',') '}' initializer
    { Variant (unspan $2) $1 (StructD $4 ($3 # $5)) $> ($1 # $2 # $5 # $>) }
  | many(outer_attribute) ident '(' sep_byT(tuple_decl_field,',')  ')' initializer
    { Variant (unspan $2) $1 (TupleD $4 ($3 # $5)) $> ($1 # $2 # $5 # $>) }
  | many(outer_attribute) ident                                        initializer
    { Variant (unspan $2) $1 (UnitD mempty) $3 ($1 # $2 # $>) }


-- parse_where_clause
where_clause :: { WhereClause Span }
  : {- empty -}                                        { WhereClause [] mempty }
  | ntWhereClause                                      { $1 }
  | where sep_byT(where_predicate,',')     %prec WHERE { WhereClause $2 ($1 # $2) }

where_predicate :: { WherePredicate Span }
  : lifetime                                               { RegionPredicate $1 [] (spanOf $1) }
  | lifetime ':' sep_byT(lifetime,'+')                     { RegionPredicate $1 $3 ($1 # $3) }
  | no_for_ty                                     %prec EQ { BoundPredicate [] $1 [] (spanOf $1) }
  | no_for_ty '='  ty                                      { EqPredicate $1 $3 ($1 # $3) }
  | no_for_ty ':' sep_byT(ty_param_bound_mod,'+')          { BoundPredicate [] $1 $3 ($1 # $3) }
  | for_lts no_for_ty                                      { BoundPredicate (unspan $1) $2 [] ($1 # $2) }
  | for_lts no_for_ty ':' sep_byT(ty_param_bound_mod,'+')  { BoundPredicate (unspan $1) $2 $4 ($1 # $>) }

impl_items :: { ([Attribute Span], [ImplItem Span]) }
  :             many(impl_item)  { ([], $1) }
  | inner_attrs many(impl_item)  { (toList $1, $2) }

impl_item :: { ImplItem Span }
  : ntImplItem                                          { $1 }
  | many(outer_attribute) vis def type ident '=' ty ';'           { TypeI $1 (unspan $2) (unspan $3) (unspan $5) $7 ($1 # $2 # $3 # $4 # $>) }
  | many(outer_attribute) vis def const ident ':' ty '=' expr ';' { ConstI $1 (unspan $2) (unspan $3) (unspan $5) $7 $9 ($1 # $2 # $3 # $4 # $>) }
  | many(outer_attribute)     def mod_mac                         { MacroI $1 (unspan $2) $3 ($1 # $2 # $>) }
  | many(outer_attribute) vis def const safety fn ident generics fn_decl_with_self where_clause inner_attrs_block
    { let methodSig = MethodSig (FnHeader (unspan $5) NotAsync Const Rust (spanOf $5)) $9; generics = $8 `withWhere` $10
      in MethodI ($1 ++ fst $>) (unspan $2) (unspan $3) (unspan $7) generics methodSig (snd $>) ($1 # $2 # $3 # $4 # snd $>) }
  | many(outer_attribute) vis def async safety fn ident generics fn_decl_with_self where_clause inner_attrs_block
    { let methodSig = MethodSig (FnHeader (unspan $5) IsAsync NotConst Rust (spanOf $5)) $9; generics = $8 `withWhere` $10
      in MethodI ($1 ++ fst $>) (unspan $2) (unspan $3) (unspan $7) generics methodSig (snd $>) ($1 # $2 # $3 # $4 # snd $>) }
  | many(outer_attribute) vis def safety ext_abi fn ident generics fn_decl_with_self where_clause inner_attrs_block
    { let methodSig = MethodSig (FnHeader (unspan $4) NotAsync NotConst (unspan $5) ($4 # $5)) $9; generics = $8 `withWhere` $10
      in MethodI ($1 ++ fst $>) (unspan $2) (unspan $3) (unspan $7) generics methodSig (snd $>) ($1 # $2 # $3 # $4 # $5 # $6 # snd $>) }


trait_item :: { TraitItem Span }
  : ntTraitItem                                              { $1 }
  | many(outer_attribute) const ident ':' ty initializer ';' { ConstT $1 (unspan $3) $5 $6 ($1 # $2 # $>) }
  | many(outer_attribute) mod_mac                            { MacroT $1 $2 ($1 # $>) }
  | many(outer_attribute) type ident ';'                     { TypeT $1 (unspan $3) [] Nothing ($1 # $2 # $>) }
  | many(outer_attribute) type ident '=' ty ';'              { TypeT $1 (unspan $3) [] (Just $5) ($1 # $2 # $>) }
  | many(outer_attribute) type ident ':' sep_byT(ty_param_bound_mod,'+') ';'
    { TypeT $1 (unspan $3) $5 Nothing ($1 # $2 # $>) }
  | many(outer_attribute) type ident ':' sep_byT(ty_param_bound_mod,'+') '=' ty ';'
    { TypeT $1 (unspan $3) $5 (Just $7) ($1 # $2 # $>) }
  | many(outer_attribute) is_async safety ext_abi fn ident generics fn_decl_with_self where_clause ';'
    { let methodSig = MethodSig (FnHeader (unspan $3) (unspan $2) NotConst (unspan $4) ($2 # $3 # $4)) $8; generics = $7 `withWhere` $9
      in MethodT $1             (unspan $6) generics methodSig Nothing         ($1 # $2 # $3 # $4 # $5 # $>) }
  | many(outer_attribute) is_async safety ext_abi fn ident generics fn_decl_with_self where_clause inner_attrs_block
    { let methodSig = MethodSig (FnHeader (unspan $3) (unspan $2) NotConst (unspan $4) ($2 # $3 # $4)) $8; generics = $7 `withWhere` $9
      in MethodT ($1 ++ fst $>) (unspan $6) generics methodSig (Just (snd $>)) ($1 # $2 # $3 # $4 # $5 # snd $>) }

is_async :: { Spanned IsAsync }
  : {- empty -}             { pure NotAsync }
  | async                   { Spanned IsAsync (spanOf $1) }

safety :: { Spanned Unsafety }
  : {- empty -}             { pure Normal }
  | unsafe                  { Spanned Unsafe (spanOf $1) }

ext_abi :: { Spanned Abi }
  : {- empty -}             { pure Rust }
  | extern abi              { Spanned $2 (spanOf $1) }

vis :: { Spanned (Visibility Span) }
  : {- empty -}   %prec VIS { Spanned InheritedV mempty }
  | pub           %prec VIS { Spanned PublicV (spanOf $1) }
  | pub '(' crate ')'       { Spanned CrateV ($1 # $4) }
  | crate         %prec VIS { Spanned CrateV (spanOf $1) }
  | pub '(' in mod_path ')' { Spanned (RestrictedV $4) ($1 # $5) }
  | pub '(' super ')'
    { Spanned (RestrictedV (Path False [PathSegment "super" Nothing (spanOf $3)] (spanOf $3))) ($1 # $4) }
  | pub '(' self ')'
    { Spanned (RestrictedV (Path False [PathSegment "self" Nothing (spanOf $3)] (spanOf $3))) ($1 # $4) }

def :: { Spanned Defaultness }
  : {- empty -}  %prec DEF        { pure Final }
  | default              { Spanned Default (spanOf $1) }

use_tree :: { UseTree Span }
  : mod_path                                    { UseTreeSimple $1 Nothing (spanOf $1) }
  | mod_path as ident                           { UseTreeSimple $1 (Just (unspan $3)) ($1 # $3) }
  | mod_path as '_'                             { UseTreeSimple $1 (Just "_") ($1 # $3) }
  | mod_path '::' '*'                           { UseTreeGlob $1 ($1 # $3) }
  |          '::' '*'                           { UseTreeGlob (Path True [] (spanOf $1)) ($1 # $2) }
  |               '*'                           { UseTreeGlob (Path False [] mempty) (spanOf $1) }
  | mod_path '::' '{' sep_byT(use_tree,',') '}' { UseTreeNested $1 $4 ($1 # $>) }
  |          '::' '{' sep_byT(use_tree,',') '}' { UseTreeNested (Path True [] (spanOf $1)) $3 ($1 # $>) }
  |               '{' sep_byT(use_tree,',') '}' { UseTreeNested (Path False [] mempty) $2 ($1 # $>) }

-------------------
-- Macro related --
-------------------

expr_mac :: { Mac Span }
  : expr_path '!' '[' token_stream ']'     { Mac $1 $4 ($1 # $>) }
  | expr_path '!' '(' token_stream ')'     { Mac $1 $4 ($1 # $>) }

ty_mac :: { Mac Span }
  : ty_path '!' '[' token_stream ']'       { Mac $1 $4 ($1 # $>) }
  | ty_path '!' '{' token_stream '}'       { Mac $1 $4 ($1 # $>) }
  | ty_path '!' '(' token_stream ')'       { Mac $1 $4 ($1 # $>) }

mod_mac :: { Mac Span }
  : mod_path '!' '[' token_stream ']' ';'  { Mac $1 $4 ($1 # $>) }
  | mod_path '!' '{' token_stream '}'      { Mac $1 $4 ($1 # $>) }
  | mod_path '!' '(' token_stream ')' ';'  { Mac $1 $4 ($1 # $>) }

token_stream :: { TokenStream }
  : {- empty -}                                { Stream [] }
  | some(token_tree)                           {
      case $1 of
        [tt] -> Tree tt
        tts -> Stream [ Tree tt | tt <- toList tts ]
    }

token_tree :: { TokenTree }
  : ntTT                                       { $1 }
  -- # Delimited
  | '(' token_stream ')'                       { Delimited ($1 # $3) Paren $2 }
  | '{' token_stream '}'                       { Delimited ($1 # $3) Brace $2 }
  | '[' token_stream ']'                       { Delimited ($1 # $3) Bracket $2 }
  -- # Token
  | token                                      { let Spanned t s = $1 in Token s t }

token :: { Spanned Token }
  : '='        { $1 }
  | '<'        { $1 }
  | '>'        { $1 }
  | '!'        { $1 }
  | '~'        { $1 }
  | '-'        { $1 }
  | '/'        { $1 }
  | '+'        { $1 }
  | '*'        { $1 }
  | '%'        { $1 }
  | '^'        { $1 }
  | '&'        { $1 }
  | '|'        { $1 }
  | '<<='      { $1 }
  | '>>='      { $1 }
  | '-='       { $1 }
  | '&='       { $1 }
  | '|='       { $1 }
  | '+='       { $1 }
  | '*='       { $1 }
  | '/='       { $1 }
  | '^='       { $1 }
  | '%='       { $1 }
  | '||'       { $1 }
  | '&&'       { $1 }
  | '=='       { $1 }
  | '!='       { $1 }
  | '<='       { $1 }
  | '>='       { $1 }
  | '<<'       { $1 }
  | '>>'       { $1 }
  -- Structural symbols.
  | '@'        { $1 }
  | '...'      { $1 }
  | '..='      { $1 }
  | '..'       { $1 }
  | '.'        { $1 }
  | ','        { $1 }
  | ';'        { $1 }
  | '::'       { $1 }
  | ':'        { $1 }
  | '->'       { $1 }
  | '<-'       { $1 }
  | '=>'       { $1 }
  | '#'        { $1 }
  | '$'        { $1 }
  | '?'        { $1 }
  | '#!'       { $1 }
  -- Literals.
  | byte       { $1 }
  | char       { $1 }
  | int        { $1 }
  | float      { $1 }
  | str        { $1 }
  | byteStr    { $1 }
  | rawStr     { $1 }
  | rawByteStr { $1 }
  -- Strict keywords used in the language
  | as         { $1 }
  | async      { $1 }
  | await      { $1 }
  | box        { $1 }
  | break      { $1 }
  | const      { $1 }
  | continue   { $1 }
  | crate      { $1 }
  | dyn        { $1 }
  | else       { $1 }
  | enum       { $1 }
  | extern     { $1 }
  | false      { $1 }
  | fn         { $1 }
  | for        { $1 }
  | if         { $1 }
  | impl       { $1 }
  | in         { $1 }
  | let        { $1 }
  | loop       { $1 }
  | macro      { $1 }
  | match      { $1 }
  | mod        { $1 }
  | move       { $1 }
  | mut        { $1 }
  | pub        { $1 }
  | ref        { $1 }
  | return     { $1 }
  | Self       { $1 }
  | self       { $1 }
  | static     { $1 }
  | struct     { $1 }
  | super      { $1 }
  | trait      { $1 }
  | true       { $1 }
  | try        { $1 }
  | type       { $1 }
  | unsafe     { $1 }
  | use        { $1 }
  | where      { $1 }
  | while      { $1 }
  | yield      { $1 }
  -- Keywords reserved for future use
  | abstract   { $1 }
  | become     { $1 }
  | do         { $1 }
  | final      { $1 }
  | override   { $1 }
  | priv       { $1 }
  | proc       { $1 }
  | typeof     { $1 }
  | unsized    { $1 }
  | virtual    { $1 }
  -- Weak keywords, have special meaning only in specific contexts.
  | default    { $1 }
  | union      { $1 }
  | auto       { $1 }
  | macro_rules { $1 }
  -- Comments
  | outerDoc   { $1 }
  | innerDoc   { $1 }
  -- Identifiers.
  | IDENT      { $1 }
  | '_'        { $1 }
  -- Lifetimes.
  | LIFETIME   { $1 }


---------------------
-- Just for export --
---------------------

-- These rules aren't used anywhere in the grammar above, they just provide a more general parsers.

-- Any attribute
export_attribute :: { Attribute Span }
  : inner_attribute { $1 }
  | outer_attribute { $1 }

-- Complete blocks
export_block :: { Block Span }
  : ntBlock                                                { $1 }
  | safety '{' '}'                                         { Block [] (unspan $1) ($1 # $2 # $>) }
  | safety '{' stmts_possibly_no_semi '}'                  { Block $3 (unspan $1) ($1 # $2 # $>) }

{
-- | Parser for literals.
parseLit :: P (Lit Span)

-- | Parser for attributes.
parseAttr :: P (Attribute Span)

-- | Parser for types.
parseTy :: P (Ty Span)

-- | Parser for patterns.
parsePat :: P (Pat Span)

-- | Parser for paths.
parsePath :: P (Path Span)

-- | Parser for statements.
parseStmt :: P (Stmt Span)

-- | Parser for expressions.
parseExpr :: P (Expr Span)

-- | Parser for items.
parseItem :: P (Item Span)

-- | Parser for blocks.
parseBlock :: P (Block Span)

-- | Parser for @impl@ items.
parseImplItem :: P (ImplItem Span)

-- | Parser for @trait@ items.
parseTraitItem :: P (TraitItem Span)

-- | Parser for token trees.
parseTt :: P TokenTree

-- | Parser for token streams.
parseTokenStream :: P TokenStream

-- | Parser for generic parameters.
parseGenericParam :: P (GenericParam Span)

-- | Parser for a where clause.
parseWhereClause :: P (WhereClause Span)

-- | Parser for generics (although 'WhereClause' is always empty here).
parseGenerics :: P (Generics Span)

-- | Generate a nice looking error message based on expected tokens
expParseError :: (Spanned Token, [String]) -> P a
expParseError (Spanned t _, exps) = fail $ "Syntax error: unexpected `" ++ show t ++ "'" ++
    case go (sort exps) [] replacements of
      []       -> ""
      [s]      -> " (expected " ++ s ++ ")"
      [s2,s1]  -> " (expected " ++ s1 ++ " or " ++ s2 ++ ")"
      (s : ss) -> " (expected " ++ (reverse ss >>= (++ ", ")) ++ "or " ++ s ++ ")"
  where

  go []     msgs _ = msgs
  go (e:es) msgs rs | e `elem` ignore = go es msgs rs
  go (e:es) msgs [] = go es (e : msgs) []
  go es     msgs ((rep,msg):rs)
    | rep `isSubsequenceOf` es = go (es \\ rep) (msg : msgs) rs
    | otherwise = go es msgs rs

  ignore = words "ntItem ntBlock ntStmt ntPat ntExpr ntTy ntIdent ntPath ntTT" ++
           words "ntArm ntImplItem ntTraitItem ntGenerics ntWhereClause ntArg ntLit"

  replacements = map (\(ks,v) -> (sort ks,v)) $
    [ (expr,                              "an expression"   )

    , (lit,                               "a literal"       )
    , (boolLit,                           "a boolean"       )
    , (byteLit,                           "a byte"          )
    , (charLit,                           "a character"     )
    , (intLit,                            "an int"          )
    , (floatLit,                          "a float"         )
    , (strLit,                            "a string"        )
    , (byteStrLit,                        "a byte string"   )
    , (rawStrLit,                         "a raw string"    )
    , (rawByteStrLit,                     "a raw bytestring")

    , (doc,                               "a doc"           )
    , (outerDoc,                          "an outer doc"    )
    , (innerDoc,                          "an inner doc"    )

    , (identifier,                        "an identifier"   )
    , (lifetime,                          "a lifetime"      )
    ]

  expr :: [String]
  expr = lit ++ identifier ++ lifetime ++
         words "'<' '!' '-' '*' '&' '|' '...' '..=' '..' '::'" ++
         words "'||' '&&' '<<' '(' '[' '{' box break continue" ++
         words "for if loop match move return Self self      " ++
         words "static super unsafe while do default union   " ++
         words "auto yield try async"

  lit = boolLit ++ byteLit ++ charLit ++ intLit ++ floatLit ++ strLit ++
        byteStrLit ++ rawStrLit ++ rawByteStrLit
  boolLit       = words "true false"
  byteLit       = words "byte"
  charLit       = words "char"
  intLit        = words "int"
  floatLit      = words "float"
  strLit        = words "str"
  byteStrLit    = words "byteStr"
  rawStrLit     = words "rawStr"
  rawByteStrLit = words "rawByteStr"

  doc = outerDoc ++ innerDoc
  outerDoc = words "outerDoc"
  innerDoc = words "innerDoc"

  identifier = words "IDENT union default auto macro_rules"
  lifetime = words "LIFETIME"

-- | Convert an 'IdentTok' into an 'Ident'
toIdent :: Spanned Token -> Spanned Ident
toIdent (Spanned (IdentTok i) s) = Spanned i s

-- | Try to convert an expression to a statement given information about whether there is a trailing
-- semicolon
toStmt :: Expr Span -> Bool -> Bool -> Span -> Stmt Span
toStmt (MacExpr a m s) hasSemi isBlock | hasSemi = MacStmt m SemicolonMac a
                                       | isBlock = MacStmt m BracesMac a
toStmt e hasSemi _ = (if hasSemi then Semi else NoSemi) e

-- | A path containing only the `macro_rules` segment
macroRulesPath :: Spanned a -> Path Span
macroRulesPath (Spanned _ s) = Path False [PathSegment "macro_rules" Nothing s] s

-- | Return the second argument, as long as the visibility is 'InheritedV'
noVis :: Spanned (Visibility Span) -> a -> P a
noVis (Spanned InheritedV _) x = pure x
noVis _ _ = fail "visibility is not allowed here"

-- | Fill in the where clause in a generic
withWhere :: Generics a -> WhereClause a -> Generics a
withWhere (Generics ps _ x) w = Generics ps w x

-- | Return the second argument, as long as the safety is 'Normal'
noSafety :: Spanned Unsafety -> a -> P a
noSafety (Spanned Normal _) x = pure x
noSafety _ _ = fail "safety is not allowed here"

-- | Add attributes to an expression
addAttrs :: [Attribute Span] -> Expr Span -> Expr Span
addAttrs as (Box as' e s)            = Box (as ++ as') e s
addAttrs as (Vec as' e s)            = Vec (as ++ as') e s
addAttrs as (Call as' f es s)        = Call (as ++ as') f es s
addAttrs as (MethodCall as' i p a s) = MethodCall (as ++ as') i p a s
addAttrs as (TupExpr as' e s)        = TupExpr (as ++ as') e s
addAttrs as (Binary as' b e1 e2 s)   = Binary (as ++ as') b e1 e2 s
addAttrs as (Unary as' u e s)        = Unary (as ++ as') u e s
addAttrs as (Lit as' l s)            = Lit (as ++ as') l s
addAttrs as (Cast as' e t s)         = Cast (as ++ as') e t s
addAttrs as (TypeAscription as' e t s) = TypeAscription (as ++ as') e t s
addAttrs as (If as' e1 b b2 s)       = If (as ++ as') e1 b b2 s
addAttrs as (IfLet as' p e b em s)   = IfLet (as ++ as') p e b em s
addAttrs as (While as' e b l s)      = While (as ++ as') e b l s
addAttrs as (WhileLet as' p e b l s) = WhileLet (as ++ as') p e b l s
addAttrs as (ForLoop as' p e b l s)  = ForLoop (as ++ as') p e b l s
addAttrs as (Loop as' b l s)         = Loop (as ++ as') b l s
addAttrs as (Match as' e a s)        = Match (as ++ as') e a s
addAttrs as (Closure as' c a m f e s) = Closure (as ++ as') c a m f e s
addAttrs as (BlockExpr as' b s)      = BlockExpr (as ++ as') b s
addAttrs as (TryBlock as' b s)       = TryBlock (as ++ as') b s
addAttrs as (Async as' c b s)        = Async (as ++ as') c b s
addAttrs as (Await as' e s)          = Await (as ++ as') e s
addAttrs as (Assign as' e1 e2 s)     = Assign (as ++ as') e1 e2 s
addAttrs as (AssignOp as' b e1 e2 s) = AssignOp (as ++ as') b e1 e2 s
addAttrs as (FieldAccess as' e i s)  = FieldAccess (as ++ as') e i s
addAttrs as (TupField as' e i s)     = TupField (as ++ as') e i s
addAttrs as (Index as' e1 e2 s)      = Index (as ++ as') e1 e2 s
addAttrs as (Range as' e1 e2 r s)    = Range (as ++ as') e1 e2 r s
addAttrs as (PathExpr as' q p s)     = PathExpr (as ++ as') q p s
addAttrs as (AddrOf as' m e s)       = AddrOf (as ++ as') m e s
addAttrs as (Break as' l e s)        = Break (as ++ as') l e s
addAttrs as (Continue as' l s)       = Continue (as ++ as') l s
addAttrs as (Ret as' e s)            = Ret (as ++ as') e s
addAttrs as (MacExpr as' m s)        = MacExpr (as ++ as') m s
addAttrs as (Struct as' p f e a)     = Struct (as ++ as') p f e a
addAttrs as (Repeat as' e1 e2 s)     = Repeat (as ++ as') e1 e2 s
addAttrs as (ParenExpr as' e s)      = ParenExpr (as ++ as') e s
addAttrs as (Try as' e s)            = Try (as ++ as') e s
addAttrs as (Yield as' e s)          = Yield (as ++ as') e s


-- | Given a 'LitTok' token that is expected to result in a valid literal, construct the associated
-- literal. Note that this should _never_ fail on a token produced by the lexer.
lit :: Spanned Token -> P (Lit Span)
lit (Spanned (IdentTok (Ident "true" False _)) s) = pure (Bool True Unsuffixed s)
lit (Spanned (IdentTok (Ident "false" False _)) s) = pure (Bool False Unsuffixed s)
lit (Spanned (LiteralTok litTok suffix_m) s) = do
  suffix <- case suffix_m of
              Nothing      -> pure Unsuffixed
              Just "_"     -> pure Unsuffixed -- See https://github.com/rust-lang/rust/issues/42326
              Just "isize" -> pure Is
              Just "usize" -> pure Us
              Just "i8"    -> pure I8
              Just "u8"    -> pure U8
              Just "i16"   -> pure I16
              Just "u16"   -> pure U16
              Just "i32"   -> pure I32
              Just "u32"   -> pure U32
              Just "i64"   -> pure I64
              Just "u64"   -> pure U64
              Just "i128"  -> pure I128
              Just "u128"  -> pure U128
              Just "f32"   -> pure F32
              Just "f64"   -> pure F64
              _ -> fail "invalid literal suffix"
  pure (translateLit litTok suffix s)

isTraitBound TraitBound{} = True
isTraitBound _ = False

-- | Parse a source file
parseSourceFile :: P (SourceFile Span)
parseSourceFile = do
  sh <- lexShebangLine
  (as,items) <- parseSourceFileContents
  pure (SourceFile sh as items)

-- | Nudge the span endpoints of a 'Span' value
nudge :: Int -> Int -> Span -> Span
nudge leftSide rightSide (Span l r) = Span l' r'
  where l' = incPos l leftSide
        r' = incPos r rightSide


-- Functions related to `NonEmpty` that really should already exist...

-- | Append an element to a list to get a nonempty list (flipped version of '(:|)')
(|:) :: [a] -> a -> NonEmpty a
[] |: y = y :| []
(x:xs) |: y = x :| (xs ++ [y])

-- | Append an element to a nonempty list to get anothg nonempty list (flipped version of '(<|)')
(|>) :: NonEmpty a -> a -> NonEmpty a
(x:|xs) |> y = x :| (xs ++ [y])

}
