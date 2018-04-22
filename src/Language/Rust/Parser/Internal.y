{
{-|
Module      : Language.Rust.Parser.Internal
Description : Rust parser
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

The parsers in this file are all re-exported to 'Language.Rust.Parser' via the 'Parse' class. The
parsers are based off of:

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
{-# LANGUAGE PartialTypeSignatures #-}

module Language.Rust.Parser.Internal (
  -- * Parsers
  parseAttr,
  parseBlock,
  parseExpr,
  parseGenerics,
  parseImplItem,
  parseItem,
  parseLifetimeDef,
  parseLit,
  parsePat,
  parseSourceFile,
  parseStmt,
  parseTokenStream,
  parseTraitItem,
  parseTt,
  parseTy,
  parseTyParam,
  parseWhereClause,
) where

import Language.Rust.Syntax

import Language.Rust.Data.Ident        ( Ident(..), mkIdent )
import Language.Rust.Data.Position

import Language.Rust.Parser.Lexer      ( lexNonSpace, lexShebangLine )
import Language.Rust.Parser.ParseMonad ( pushToken, getPosition, P, parseError )
import Language.Rust.Parser.Literals   ( translateLit )
import Language.Rust.Parser.Reversed

import Data.Foldable                   ( toList )
import Data.List                       ( (\\), isSubsequenceOf, sort )
import Data.Semigroup                  ( (<>) )

import Text.Read                       ( readMaybe )

import Data.List.NonEmpty              ( NonEmpty(..), (<|) )
import qualified Data.List.NonEmpty as N
}

-- in order to document the parsers, we have to alias them
%name parseLit lit
%name parseAttr export_attribute
%name parseTy ty
%name parsePat pat
%name parseStmt stmt
%name parseExpr expr
%name parseItem mod_item
%name parseSourceFileContents source_file
%name parseBlock export_block
%name parseImplItem  impl_item
%name parseTraitItem trait_item
%name parseTt token_tree
%name parseTokenStream token_stream
%name parseTyParam ty_param
%name parseLifetimeDef lifetime_def
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
  box            { Spanned (IdentTok "box") _ }
  break          { Spanned (IdentTok "break") _ }
  const          { Spanned (IdentTok "const") _ }
  continue       { Spanned (IdentTok "continue") _ }
  crate          { Spanned (IdentTok "crate") _ }
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
  type           { Spanned (IdentTok "type") _ }
  unsafe         { Spanned (IdentTok "unsafe") _ }
  use            { Spanned (IdentTok "use") _ }
  where          { Spanned (IdentTok "where") _ }
  while          { Spanned (IdentTok "while") _ }
  do             { Spanned (IdentTok "do") _ }

  -- Keywords reserved for future use
  abstract       { Spanned (IdentTok "abstract") _ }
  alignof        { Spanned (IdentTok "alignof") _ }
  become         { Spanned (IdentTok "become") _ }
  final          { Spanned (IdentTok "final") _ }
  macro          { Spanned (IdentTok "macro") _ }
  offsetof       { Spanned (IdentTok "offsetof") _ }
  override       { Spanned (IdentTok "override") _ }
  priv           { Spanned (IdentTok "priv") _ }
  proc           { Spanned (IdentTok "proc") _ }
  pure           { Spanned (IdentTok "pure") _ }
  sizeof         { Spanned (IdentTok "sizeof") _ }
  typeof         { Spanned (IdentTok "typeof") _ }
  unsized        { Spanned (IdentTok "unsized") _ }
  virtual        { Spanned (IdentTok "virtual") _ }

  -- Weak keywords, have special meaning only in specific contexts.
  default        { Spanned (IdentTok "default") _ }
  union          { Spanned (IdentTok "union") _ }
  catch          { Spanned (IdentTok "catch") _ }
  auto           { Spanned (IdentTok "auto") _ }
  yield          { Spanned (IdentTok "yield") _ }
  dyn            { Spanned (IdentTok "dyn") _ }

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
%nonassoc IDENT ntIdent default union catch self Self super auto dyn crate

-- These are all very low precedence unary operators
%nonassoc box return yield break continue for IMPLTRAIT LAMBDA

-- 'static' needs to have higher precedenc than 'LAMBDA' so that statements starting in static get
-- considered as static items, and not a static lambda
%nonassoc static

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
%left '==' '!=' '<' '>' '<=' '>='
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
-- For questionable reasons of backwards compatibility, 'union', 'default', and 'catch' can be used
-- as identifiers, even if they are also keywords. They are "contextual" keywords.
--
-- Union's RFC: https://github.com/rust-lang/rfcs/blob/master/text/1444-union.md
ident :: { Spanned Ident }
  : ntIdent                       { fmap (\(Interpolated (NtIdent i)) -> i) $1 }
  | union                         { toIdent $1 }
  | default                       { toIdent $1 }
  | catch                         { toIdent $1 }
  | auto                          { toIdent $1 }
  | dyn                           { toIdent $1 }
  | IDENT                         { toIdent $1 }

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
some(p) :: { Reversed NonEmpty _ }
  : some(p) p             { let Reversed xs = $1 in Reversed ($2 <| xs) }
  | p                     { [$1] }

-- | Zero or more occurences of 'p'
many(p) :: { [ _ ] }
  : some(p)               { toList $1 }
  | {- empty -}           { [] }

-- | One or more occurences of 'p', seperated by 'sep'
sep_by1(p,sep) :: { Reversed NonEmpty _ }
  : sep_by1(p,sep) sep p  { let Reversed xs = $1 in Reversed ($3 <| xs) }
  | p                     { [$1] }

-- | Zero or more occurrences of 'p', separated by 'sep'
sep_by(p,sep) :: { [ _ ] }
  : sep_by1(p,sep)        { toList $1 }
  | {- empty -}           { [] }

-- | One or more occurrences of 'p', seperated by 'sep', optionally ending in 'sep'
sep_by1T(p,sep) :: { Reversed NonEmpty _ }
  : sep_by1(p,sep) sep    { $1 }
  | sep_by1(p,sep)        { $1 }

-- | Zero or more occurences of 'p', seperated by 'sep', optionally ending in 'sep' (only if there
-- is at least one 'p')
sep_byT(p,sep) :: { [ _ ] }
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
  | byte              { lit $1 }
  | char              { lit $1 }
  | int               { lit $1 }
  | float             { lit $1 }
  | true              { lit $1 }
  | false             { lit $1 }
  | string            { $1 }

string :: { Lit Span }
  : str               { lit $1 }
  | rawStr            { lit $1 }
  | byteStr           { lit $1 }
  | rawByteStr        { lit $1 }


-----------
-- Paths --
-----------

-- parse_qualified_path(PathStyle::Type)
-- qual_path :: Spanned (NonEmpty (Ident, PathParameters Span)) -> P (Spanned (QSelf Span, Path Span))
qual_path(segs) :: { Spanned (QSelf Span, Path Span) }
  : '<' qual_path_suf(segs)                    { let Spanned x _ = $2 in Spanned x ($1 # $2) }
  | lt_ty_qual_path as ty_path '>' '::' segs   {
      let Path g segsTy x = $3 in
      Spanned (QSelf (unspan $1) (length segsTy), Path g (segsTy <> toList $6) x) ($1 # $>)
    }

-- Basically a qualified path, but ignoring the very first '<' token
qual_path_suf(segs) :: { Spanned (QSelf Span, Path Span) }
  : ty '>' '::' segs                { Spanned (QSelf $1 0, Path False (toList $4) (spanOf $4)) ($1 # $>) }
  | ty as ty_path '>' '::' segs     {
      let Path g segsTy x = $3 in
      Spanned (QSelf $1 (length segsTy), Path g (segsTy <> toList $6) x) ($1 # $>) 
    }

-- Usually qual_path_suf is for... type paths! This consumes these but with a starting '<<' token.
-- The underlying type has the right 'Span' (it doesn't include the very first '<', while the
-- 'Spanned' wrapper does) 
lt_ty_qual_path :: { Spanned (Ty Span) }
  : '<<' qual_path_suf(path_segments_without_colons)
    { let (qself,path) = unspan $2 in Spanned (PathTy (Just qself) path (nudge 1 0 ($1 # $2))) ($1 # $2) }

-- parse_generic_args() but with the '<' '>'
generic_values :: { Spanned ([Lifetime Span], [Ty Span], [(Ident, Ty Span)]) }
  : '<' sep_by1(lifetime,',')  ',' sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>'
    { Spanned (toList $2,      toList $4, toList $6) ($1 # $>) }
  | '<' sep_by1(lifetime,',')  ',' sep_by1T(ty,',')                          gt '>'
    { Spanned (toList $2,      toList $4, []       ) ($1 # $>) }
  | '<' sep_by1(lifetime,',')  ','                     sep_by1T(binding,',') gt '>'
    { Spanned (toList $2,      [],        toList $4) ($1 # $>) }
  | '<' sep_by1T(lifetime,',')                                               gt '>'
    { Spanned (toList $2,      [],        []       ) ($1 # $>) }
  | '<'                            sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>'
    { Spanned ([],             toList $2, toList $4) ($1 # $>) }
  | '<'                            sep_by1T(ty,',')                          gt '>'
    { Spanned ([],             toList $2, []       ) ($1 # $>) }
  | '<'                                                sep_by1T(binding,',') gt '>'
    { Spanned ([],             [],        toList $2) ($1 # $>) }
  | '<'                                                                      gt '>'
    { Spanned ([],             [],        []       ) ($1 # $>) }
  | lt_ty_qual_path            ',' sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>'
    { Spanned ([], unspan $1 : toList $3, toList $5) ($1 # $>) }
  | lt_ty_qual_path            ',' sep_by1T(ty,',')                          gt '>'
    { Spanned ([], unspan $1 : toList $3, []       ) ($1 # $>) }
  | lt_ty_qual_path                                ',' sep_by1T(binding,',') gt '>'
    { Spanned ([],            [unspan $1],toList $3) ($1 # $>) }
  | lt_ty_qual_path                                                          gt '>'
    { Spanned ([],            [unspan $1],[]       ) ($1 # $>) }

binding :: { (Ident, Ty Span) }
  : ident '=' ty                             { (unspan $1, $3) }


-- Type related:
-- parse_path(PathStyle::Type)
ty_path :: { Path Span }
  : ntPath                                   { $1 }
  | path_segments_without_colons             { Path False $1 (spanOf $1) }
  | '::' path_segments_without_colons        { Path True $2 ($1 # $2) }

ty_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(path_segments_without_colons)  { $1 }

-- parse_path_segments_without_colons()
path_segments_without_colons :: { [PathSegment Span] }
  : sep_by1(path_segment_without_colons, '::') %prec SEG  { toList $1 }

-- No corresponding function - see path_segments_without_colons
path_segment_without_colons :: { PathSegment Span }
  : self_or_ident path_parameter                     { PathSegment (unspan $1) $2 ($1 # $>) }

path_parameter  :: { Maybe (PathParameters Span) }
  : generic_values                           { let (lts, tys, bds) = unspan $1
                                               in Just (AngleBracketed lts tys bds (spanOf $1)) }
  | '(' sep_byT(ty,',') ')'                  { Just (Parenthesized $2 Nothing ($1 # $>)) }
  | '(' sep_byT(ty,',') ')' '->' ty_no_plus  { Just (Parenthesized $2 (Just $>) ($1 # $>)) }
  | {- empty -}                  %prec IDENT { Nothing }


-- Expression related:
-- parse_path(PathStyle::Expr)
expr_path :: { Path Span }
  : ntPath                                   { $1 }
  | path_segments_with_colons                { Path False (toList $1) (spanOf $1) }
  | '::' path_segments_with_colons           { Path True (toList $2) ($1 # $2) }

expr_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(path_segments_with_colons)     { $1 }

-- parse_path_segments_with_colons()
path_segments_with_colons :: { Reversed NonEmpty (PathSegment Span) }
  : self_or_ident
    { [PathSegment (unspan $1) Nothing (spanOf $1)] }
  | path_segments_with_colons '::' self_or_ident
    { $1 <> [PathSegment (unspan $3) Nothing (spanOf $3)] }
  | path_segments_with_colons '::' generic_values
    {%
      case (unsnoc $1, unspan $3) of
        ((rst, PathSegment i Nothing x), (lts, tys, bds)) ->
          let seg = PathSegment i (Just (AngleBracketed lts tys bds (spanOf $3))) (x # $3)
          in pure $ snoc rst seg 
        _ -> fail "invalid path segment in expression path"
    }

-- Mod related:
-- parse_path(PathStyle::Mod)
--
-- TODO: This is O(n^2) in the segment length! I haven't been able to make the grammar work out in
--       order to refactor this nicely
mod_path :: { Path Span  }
  : ntPath               { $1 }
  | self_or_ident        { Path False [PathSegment (unspan $1) Nothing (spanOf $1)] (spanOf $1) }
  | '::' self_or_ident   { Path True  [PathSegment (unspan $2) Nothing (spanOf $2)] ($1 # $>) }
  | mod_path '::' self_or_ident  {
      let Path g segs _ = $1 in
      Path g (segs <> [PathSegment (unspan $3) Nothing (spanOf $3) ]) ($1 # $3)
    }

self_or_ident :: { Spanned Ident }
  : ident                   { $1 }
  | crate                   { Spanned "crate" (spanOf $1) }
  | self                    { Spanned "self" (spanOf $1) }
  | Self                    { Spanned "Self" (spanOf $1) }
  | super                   { Spanned "super" (spanOf $1) }


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
  : ty_no_plus                                                    { $1 }
  | poly_trait_ref_mod_bound '+' sep_by1T(ty_param_bound_mod,'+') { TraitObject ($1 <| toNonEmpty $3) ($1 # $3) }

-- parse_ty_no_plus()
ty_no_plus :: { Ty Span }
  : ntTy                             { $1 }
  | no_for_ty                        { $1 }
  | for_ty_no_plus                   { $1 }

-- All types not starting with a '(' or '<'
ty_prim :: { Ty Span }
  : no_for_ty_prim                   { $1 }
  | for_ty_no_plus                   { $1 }
  | poly_trait_ref_mod_bound '+' sep_by1T(ty_param_bound_mod,'+') { TraitObject ($1 <| toNonEmpty $3) ($1 # $3) }

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
  | unsafe extern abi fn fn_decl(arg_general)     { BareFn Unsafe $3 [] $> ($1 # $>) }
  | unsafe fn fn_decl(arg_general)                { BareFn Unsafe Rust [] $> ($1 # $>) }
  | extern abi fn fn_decl(arg_general)            { BareFn Normal $2 [] $> ($1 # $>) }
  | fn fn_decl(arg_general)                       { BareFn Normal Rust [] $> ($1 # $>) }
  | typeof '(' expr ')'              { Typeof $3 ($1 # $>) }
  | '[' ty ';' expr ']'              { Array $2 $4 ($1 # $>) }
  | '?' trait_ref                    { TraitObject [TraitTyParamBound (PolyTraitRef [] $2 (spanOf $2)) Maybe ($1 # $2)] ($1 # $2) }
  | '?' for_lts trait_ref            { TraitObject [TraitTyParamBound (PolyTraitRef (unspan $2) $3 ($2 # $3)) Maybe ($1 # $3)] ($1 # $3) }
  | impl sep_by1(ty_param_bound_mod,'+') %prec IMPLTRAIT { ImplTrait (toNonEmpty $2) ($1 # $2) }
  | dyn  sep_by1(ty_param_bound_mod,'+') %prec IMPLTRAIT { TraitObject (toNonEmpty $2) ($1 # $2) }

-- All (non-sum) types starting with a 'for'
for_ty_no_plus :: { Ty Span }
  : for_lts unsafe extern abi fn fn_decl(arg_general) { BareFn Unsafe $4 (unspan $1) $> ($1 # $>) }
  | for_lts unsafe fn fn_decl(arg_general)            { BareFn Unsafe Rust (unspan $1) $> ($1 # $>) }
  | for_lts extern abi fn fn_decl(arg_general)        { BareFn Normal $3 (unspan $1) $> ($1 # $>) }
  | for_lts fn fn_decl(arg_general)                   { BareFn Normal Rust (unspan $1) $> ($1 # $>) }
  | for_lts trait_ref                                 {
      let poly = PolyTraitRef (unspan $1) $2 ($1 # $2) in
      TraitObject [TraitTyParamBound poly None ($1 # $2)] ($1 # $2)
    }

-- An optional lifetime followed by an optional mutability
lifetime_mut :: { (Maybe (Lifetime Span), Mutability) }
  : lifetime mut  { (Just $1, Mutable) }
  | lifetime      { (Just $1, Immutable) }
  |          mut  { (Nothing, Mutable) }
  | {- empty -}   { (Nothing, Immutable) }

-- The argument list and return type in a function
fn_decl(arg) :: { FnDecl Span }
  : '(' sep_by1(arg,',') ',' '...' ')' ret_ty  { FnDecl (toList $2) $> True ($1 # $5 # $6) }
  | '(' sep_byT(arg,',')           ')' ret_ty  { FnDecl $2 $> False ($1 # $3 # $4) }

-- Like 'fn_decl', but also accepting a self argument
fn_decl_with_self_general :: { FnDecl Span }
  : '(' arg_self_general ',' sep_byT(arg_general,',') ')' ret_ty  { FnDecl ($2 : $4) $> False ($1 # $5 # $6) }
  | '(' arg_self_general                              ')' ret_ty  { FnDecl [$2] $> False ($1 # $3 # $4) }
  | '('                                               ')' ret_ty  { FnDecl [] $> False ($1 # $2 # $3) }

-- Like 'fn_decl', but also accepting a self argument
fn_decl_with_self_named :: { FnDecl Span }
  : '(' arg_self_named ',' sep_by1(arg_named,',') ',' ')' ret_ty  { FnDecl ($2 : toList $4) $> False ($1 # $6 # $7) }
  | '(' arg_self_named ',' sep_by1(arg_named,',')     ')' ret_ty  { FnDecl ($2 : toList $4) $> False ($1 # $5 # $6) }
  | '(' arg_self_named ','                            ')' ret_ty  { FnDecl [$2] $> False ($1 # $3 # $4) }
  | '(' arg_self_named                                ')' ret_ty  { FnDecl [$2] $> False ($1 # $3 # $4) }
  | fn_decl(arg_named)                                            { $1 }


-- parse_ty_param_bounds(BoundParsingMode::Bare) == sep_by1(ty_param_bound,'+')
ty_param_bound :: { TyParamBound Span }
  : lifetime                { RegionTyParamBound $1 (spanOf $1) }
  | poly_trait_ref          { TraitTyParamBound $1 None (spanOf $1) }
  | '(' poly_trait_ref ')'  { TraitTyParamBound $2 None ($1 # $3) }

poly_trait_ref_mod_bound :: { TyParamBound Span }
  : poly_trait_ref       { TraitTyParamBound $1 None (spanOf $1) }
  | '?' poly_trait_ref   { TraitTyParamBound $2 Maybe ($1 # $2) }

-- parse_ty_param_bounds(BoundParsingMode::Modified) == sep_by1(ty_param_bound_mod,'+')
ty_param_bound_mod :: { TyParamBound Span }
  : ty_param_bound       { $1 }
  | '?' poly_trait_ref   { TraitTyParamBound $2 Maybe ($1 # $2) }

-- Sort of like parse_opt_abi() -- currently doesn't handle raw string ABI
abi :: { Abi }
  : str             {% case unspan $1 of
                         LiteralTok (StrTok "cdecl") Nothing ->              pure Cdecl             
                         LiteralTok (StrTok "stdcall") Nothing ->            pure Stdcall          
                         LiteralTok (StrTok "fastcall") Nothing ->           pure Fastcall         
                         LiteralTok (StrTok "vectorcall") Nothing ->         pure Vectorcall       
                         LiteralTok (StrTok "aapcs") Nothing ->              pure Aapcs            
                         LiteralTok (StrTok "win64") Nothing ->              pure Win64            
                         LiteralTok (StrTok "sysv64") Nothing ->             pure SysV64           
                         LiteralTok (StrTok "ptx-kernel") Nothing ->         pure PtxKernel        
                         LiteralTok (StrTok "msp430-interrupt") Nothing ->   pure Msp430Interrupt  
                         LiteralTok (StrTok "x86-interrupt") Nothing ->      pure X86Interrupt     
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

-- parse_ret_ty
ret_ty :: { Maybe (Ty Span) }
  : '->' ty_no_plus                                  { Just $2 }
  | {- empty -}                                      { Nothing }

-- parse_poly_trait_ref()
poly_trait_ref :: { PolyTraitRef Span }
  :         trait_ref                                { PolyTraitRef [] $1 (spanOf $1) }
  | for_lts trait_ref                                { PolyTraitRef (unspan $1) $2 ($1 # $2) }

-- parse_for_lts()
-- Unlike the Rust libsyntax version, this _requires_ the 'for'
for_lts :: { Spanned [LifetimeDef Span] }
  : for '<' sep_byT(lifetime_def,',') '>'            { Spanned $3 ($1 # $>) }

-- Definition of a lifetime: attributes can come before the lifetime, and a list of bounding
-- lifetimes can come after the lifetime.
lifetime_def :: { LifetimeDef Span }
  : many(outer_attribute) lifetime ':' sep_by1T(lifetime,'+')  { LifetimeDef $1 $2 (toList $4) ($1 # $2 # $>) }
  | many(outer_attribute) lifetime                             { LifetimeDef $1 $2 [] ($1 # $2 # $>) }


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
  
-- Self argument (only allowed in trait function signatures)
arg_self_general :: { Arg Span }
  : mut self              { SelfValue Mutable ($1 # $>) }
  |     self ':' ty       { SelfExplicit $3 Immutable ($1 # $>) }
  | mut self ':' ty       { SelfExplicit $4 Mutable ($1 # $>) }
  | arg_general           {
      case $1 of
        Arg Nothing (PathTy Nothing (Path False [PathSegment "self" Nothing _] _) _) x -> SelfValue Immutable x
        Arg Nothing (Rptr l m (PathTy Nothing (Path False [PathSegment "self" Nothing _] _) _) _) x -> SelfRegion l m x
        _ -> $1
    }

-- Self argument (only allowed in impl function signatures)
arg_self_named :: { Arg Span }
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

-- There is a funky trick going on here around 'IdentP'. When there is a binding mode (ie a 'mut' or
-- 'ref') or an '@' pattern, everything is fine, but otherwise there is no difference between an
-- expression variable path and a pattern. To deal with this, we intercept expression paths with
-- only one segment, no path parameters, and not global and turn them into identifier patterns.
pat :: { Pat Span }
  : ntPat                           { $1 }
  | '_'                             { WildP (spanOf $1) }
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
  | lit_or_path '...' lit_or_path   { RangeP $1 $3 ($1 # $>) }
  | lit_or_path '..=' lit_or_path   { RangeP $1 $3 ($1 # $>) }
  | expr_path '{' '..' '}'          { StructP $1 [] True ($1 # $>) }
  | expr_path '{' pat_fields '}'    { let (fs,b) = $3 in StructP $1 fs b ($1 # $>) }
  | expr_path '(' pat_tup ')'       { let (ps,m,_) = $3 in TupleStructP $1 ps m ($1 # $>) }
  | expr_mac                        { MacP $1 (spanOf $1) }
  | '[' pat_slice ']'               { let (b,s,a) = $2 in SliceP b s a ($1 # $3) }
  | '(' pat_tup ')'                 {%
      case $2 of
        ([p], Nothing, False) -> parseError (CloseDelim Paren)
        (ps,m,t) -> pure (TupleP ps m ($1 # $3))
    }


-- The first element is the spans, the second the position of '..', and the third if there is a
-- trailing comma
pat_tup :: { ([Pat Span], Maybe Int, Bool) }
  : sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',')     { (toList ($1 <> $5), Just (length $1), False) }
  | sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',') ',' { (toList ($1 <> $5), Just (length $1), True) }
  | sep_by1(pat,',') ',' '..'                          { (toList $1,         Just (length $1), False) }
  | sep_by1(pat,',')                                   { (toList $1,         Nothing,          False) }
  | sep_by1(pat,',') ','                               { (toList $1,         Nothing,          True) }
  |                      '..' ',' sep_by1(pat,',')     { (toList $3,         Just 0,           False) }
  |                      '..' ',' sep_by1(pat,',') ',' { (toList $3,         Just 0,           True) }
  |                      '..'                          { ([],                Just 0,           False) }
  | {- empty -}                                        { ([],                Nothing,          False) }

-- The first element is the patterns at the beginning of the slice, the second the optional binding
-- for the middle slice ('Nothing' if there is no '..' and 'Just (WildP mempty) is there is one, but
-- unlabelled), and the third is the patterns at the end of the slice.
pat_slice :: { ([Pat Span], Maybe (Pat Span), [Pat Span]) }
  : sep_by1(pat,',') ',' '..' ',' sep_by1T(pat,',')    { (toList $1, Just (WildP mempty), toList $5) }
  | sep_by1(pat,',') ',' '..'                          { (toList $1, Just (WildP mempty), []) }
  | sep_by1(pat,',')     '..' ',' sep_by1T(pat,',')    { let (xs, x) = unsnoc $1 in (toList xs, Just x,    toList $4) }
  | sep_by1(pat,',')     '..'                          { let (xs, x) = unsnoc $1 in (toList xs, Just x,    []) }
  |                               sep_by1T(pat,',')    { (toList $1, Nothing,             []) }
  |                      '..' ',' sep_by1T(pat,',')    { ([],        Just (WildP mempty), toList $3) }
  |                      '..'                          { ([],        Just (WildP mempty), []) }
  | {- empty -}                                        { ([],        Nothing,             []) }


-- Endpoints of range patterns
lit_or_path :: { Expr Span }
  : expr_path         { PathExpr [] Nothing $1 (spanOf $1) }
  | expr_qual_path    { PathExpr [] (Just (fst (unspan $1))) (snd (unspan $1)) (spanOf $1) }
  | '-' lit_expr      { Unary [] Neg $2 ($1 # $2) }
  |     lit_expr      { $1 }

-- Used in patterns for tuple and expression patterns
pat_fields :: { ([FieldPat Span], Bool) }
  : sep_byT(pat_field,',')           { ($1, False) }
  | sep_by1(pat_field,',') ',' '..'  { (toList $1, True) }

pat_field :: { FieldPat Span }
  :     binding_mode ident
    { FieldPat Nothing (IdentP (unspan $1) (unspan $2) Nothing (spanOf $2)) ($1 # $2) }
  | box binding_mode ident
    { FieldPat Nothing (BoxP (IdentP (unspan $2) (unspan $3) Nothing ($2 # $3)) ($1 # $3)) ($1 # $3) }
  |     binding_mode ident ':' pat
    { FieldPat (Just (unspan $2)) $4 ($1 # $2 # $4) }


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
--   * 'rhs2' - expressions allowed on the right extremity following '..'/'.../..='
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
  | return rhs                       { Ret [] (Just $2) ($1 # $2) }
  | yield                            { Yield [] Nothing (spanOf $1) }
  | yield rhs                        { Yield [] (Just $2) ($1 # $2) }
  | continue                         { Continue [] Nothing (spanOf $1) }
  | continue label                   { Continue [] (Just $2) ($1 # $2) }
  | break                            { Break [] Nothing Nothing (spanOf $1) }
  | break       rhs                  { Break [] Nothing (Just $2) ($1 # $2) }
  | break label                      { Break [] (Just $2) Nothing ($1 # $2) }
  | break label rhs      %prec break { Break [] (Just $2) (Just $3) ($1 # $3) }
  -- lambda expressions
  | static move lambda_args rhs   %prec LAMBDA
    { Closure [] Immovable Value (FnDecl (unspan $3) Nothing False (spanOf $3)) $> ($1 # $>) }
  |        move lambda_args rhs   %prec LAMBDA
    { Closure [] Movable Value (FnDecl (unspan $2) Nothing False (spanOf $2)) $> ($1 # $>) }
  | static      lambda_args rhs   %prec LAMBDA
    { Closure [] Immovable Ref   (FnDecl (unspan $2) Nothing False (spanOf $2)) $> ($1 # $>) }
  |             lambda_args rhs   %prec LAMBDA
    { Closure [] Movable Ref   (FnDecl (unspan $1) Nothing False (spanOf $1)) $> ($1 # $>) }

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
  | lhs '<-' rhs                     { InPlace [] $1 $3 ($1 # $>) }
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
  | lhs '.' ident       %prec FIELD  { FieldAccess [] $1 (unspan $3) ($1 # $>) }
  | lhs '.' ident '(' sep_byT(expr,',') ')'
    { MethodCall [] $1 (unspan $3) Nothing $5 ($1 # $>) }
  | lhs '.' ident '::' '<' sep_byT(ty,',') '>' '(' sep_byT(expr,',') ')'
    { MethodCall [] $1 (unspan $3) (Just $6) $9 ($1 # $>) }
  | lhs '.' int                      {%
      case lit $3 of
        Int Dec i Unsuffixed _ -> pure (TupField [] $1 (fromIntegral i) ($1 # $3))
        _ -> parseError $3
    }

-- Postfix expressions that can come after an expression block, in a 'stmt'
--
--  * `{ 1 }[0]` isn't here because it is treated as `{ 1 }; [0]`
--  * `{ 1 }(0)` isn't here because it is treated as `{ 1 }; (0)`
--
postfix_blockexpr(lhs) :: { Expr Span }
  : lhs '?'                          { Try [] $1 ($1 # $>) }
  | lhs '.' ident       %prec FIELD  { FieldAccess [] $1 (unspan $3) ($1 # $>) }
  | lhs '.' ident '(' sep_byT(expr,',') ')'
    { MethodCall [] $1 (unspan $3) Nothing $5 ($1 # $>) }
  | lhs '.' ident '::' '<' sep_byT(ty,',') '>' '(' sep_byT(expr,',') ')'
    { MethodCall [] $1 (unspan $3) (Just $6) $9 ($1 # $>) }
  | lhs '.' int                      {%
      case lit $3 of
        Int Dec i Unsuffixed _ -> pure (TupField [] $1 (fromIntegral i) ($1 # $3))
        _ -> parseError $3
    }

-- Postfix expressions that can come after an expression block, in a 'stmt'
--
--  * `{ 1 }[0]` isn't here because it is treated as `{ 1 }; [0]`
--  * `{ 1 }(0)` isn't here because it is treated as `{ 1 }; (0)`
--
postfix_blockexpr(lhs) :: { Expr Span }
  : lhs '?'                          { Try [] $1 ($1 # $>) }
  | lhs '.' ident       %prec FIELD  { FieldAccess [] $1 (unspan $3) ($1 # $>) }
  | lhs '.' ident '(' sep_byT(expr,',') ')'
    { MethodCall [] $1 (unspan $3) Nothing $5 ($1 # $>) }
  | lhs '.' ident '::' '<' sep_byT(ty,',') '>' '(' sep_byT(expr,',') ')'
    { MethodCall [] $1 (unspan $3) (Just $6) $9 ($1 # $>) }
  | lhs '.' int                      {%
      case lit $3 of
        Int Dec i Unsuffixed _ -> pure (TupField [] $1 (fromIntegral i) ($1 # $3))
        _ -> parseError $3
    }



-- Then, we instantiate this general production into the following families of rules:
--
--   ['expr']               Most general class of expressions, no restrictions
--
--   ['nostruct_expr']      Forbids struct literals (for use as scrutinee of loops, ifs, etc)
--
--   ['nostructblock_expr'] Forbids struct literals and block expressions (but not block-like things
--                          like 'if' expressions or 'loop' expressions)
--
--   ['nonblock_expr']      Forbids expressions starting with blocks (things such as '{ 1 } + 2' are
--                          not allowed, while struct expressions are - their "block" is at the end
--                          of the expression)
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
  | lambda_expr_block                                                         { $1 }

nostruct_expr :: { Expr Span }
  : gen_expression(nostruct_expr,nostruct_expr,nonstructblock_expr)           { $1 }
  | paren_expr                                                                { $1 }
  | block_expr                                                                { $1 }

nonstructblock_expr :: { Expr Span }
  : gen_expression(nonstructblock_expr,nostruct_expr,nonstructblock_expr)     { $1 }
  | paren_expr                                                                { $1 }
  | block_like_expr                                                           { $1 }
  | unsafe inner_attrs_block
    { let (as, Block ss r x) = $> in BlockExpr as (Block ss Unsafe ($1 # x)) ($1 # x) }

nonblock_expr :: { Expr Span }
  : gen_expression(nonblock_expr,expr,expr)                                   { $1 }
  | paren_expr                                                                { $1 }
  | struct_expr                                                               { $1 }
  | lambda_expr_block                                                         { $1 }

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
  | inner_attrs_block                                   { let (as,b) = $1 in BlockExpr as b (spanOf b) }
  | unsafe inner_attrs_block
    { let (as, Block ss r x) = $> in BlockExpr as (Block ss Unsafe ($1 # x)) ($1 # x) }

-- Any expression ending in a '{ ... }' block except a block itself.
block_like_expr :: { Expr Span }
  : if_expr                                                      { $1 }
  |           loop                            inner_attrs_block  { let (as,b) = $> in Loop as b Nothing ($1 # b) }
  | label ':' loop                            inner_attrs_block  { let (as,b) = $> in Loop as b (Just $1) ($1 # b) }
  |           for pat in nostruct_expr        inner_attrs_block  { let (as,b) = $> in ForLoop as $2 $4 b Nothing ($1 # b) }
  | label ':' for pat in nostruct_expr        inner_attrs_block  { let (as,b) = $> in ForLoop as $4 $6 b (Just $1) ($1 # b) }
  |           while             nostruct_expr inner_attrs_block  { let (as,b) = $> in While as $2 b Nothing ($1 # b) }
  | label ':' while             nostruct_expr inner_attrs_block  { let (as,b) = $> in While as $4 b (Just $1) ($1 # b) }
  |           while let pats '=' nostruct_expr inner_attrs_block { let (as,b) = $> in WhileLet as $3 $5 b Nothing ($1 # b) }
  | label ':' while let pats '=' nostruct_expr inner_attrs_block { let (as,b) = $> in WhileLet as $5 $7 b (Just $1) ($1 # b) }
  | match nostruct_expr '{'                  '}'                 { Match [] $2 [] ($1 # $>) }
  | match nostruct_expr '{' inner_attrs      '}'                 { Match (toList $4) $2 [] ($1 # $>) }
  | match nostruct_expr '{'             arms '}'                 { Match [] $2 $4 ($1 # $>) }
  | match nostruct_expr '{' inner_attrs arms '}'                 { Match (toList $4) $2 $5 ($1 # $>) }
  | expr_path '!' '{' token_stream '}'                           { MacExpr [] (Mac $1 $4 ($1 # $>)) ($1 # $>) }
  | do catch inner_attrs_block                                   { let (as,b) = $> in Catch as b ($1 # b) }

-- 'if' expressions are a bit special since they can have an arbitrary number of 'else if' chains.
if_expr :: { Expr Span }
  : if             nostruct_expr block else_expr        { If [] $2 $3 $4 ($1 # $3 # $>) }
  | if let pats '=' nostruct_expr block else_expr       { IfLet [] $3 $5 $6 $7 ($1 # $6 # $>) }

else_expr :: { Maybe (Expr Span) }
  : else block                                          { Just (BlockExpr [] $2 (spanOf $2)) }
  | else if_expr                                        { Just $2 }
  | {- empty -}                                         { Nothing }

-- Match arms usually have to be seperated by commas (with an optional comma at the end). This
-- condition is loosened (so that there is no seperator needed) if the arm ends in a safe block.
arms :: { [Arm Span] }
  : ntArm                                               { [$1] }
  | ntArm arms                                          { $1 : $2 }
  | many(outer_attribute) pats arm_guard '=>' expr_arms { let (e,as) = $> in (Arm $1 $2 $3 e ($1 # $2 # e) : as) }

pats :: { NonEmpty (Pat Span) }
  : '|' sep_by1(pat,'|')   { toNonEmpty $2 }
  |     sep_by1(pat,'|')   { toNonEmpty $1 }

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


-- Closure ending in blocks
lambda_expr_block :: { Expr Span }
  : static move lambda_args '->' ty_no_plus block
    { Closure [] Immovable Value (FnDecl (unspan $3) (Just $5) False (spanOf $3)) (BlockExpr [] $> (spanOf $>)) ($1 # $>) }
  |        move lambda_args '->' ty_no_plus block
    { Closure [] Movable Value (FnDecl (unspan $2) (Just $4) False (spanOf $2)) (BlockExpr [] $> (spanOf $>)) ($1 # $>) }
  | static      lambda_args '->' ty_no_plus block
    { Closure [] Immovable Ref   (FnDecl (unspan $2) (Just $4) False (spanOf $2)) (BlockExpr [] $> (spanOf $>)) ($1 # $>) }
  |             lambda_args '->' ty_no_plus block
    { Closure [] Movable Ref   (FnDecl (unspan $1) (Just $3) False (spanOf $1)) (BlockExpr [] $> (spanOf $>)) ($1 # $>) }

-- Lambda expression arguments block
lambda_args :: { Spanned [Arg Span] }
  : '||'                                                { Spanned [] (spanOf $1) }
  | '|' sep_byT(lambda_arg,',') pipe '|'                { Spanned $2 ($1 # $4) }


-- Struct expression literal
struct_expr :: { Expr Span }
  : expr_path '{'                                    '..' expr '}'  { Struct [] $1 [] (Just $4) ($1 # $>) }
  | expr_path '{' inner_attrs                        '..' expr '}'  { Struct (toList $3) $1 [] (Just $5) ($1 # $>) }
  | expr_path '{'             sep_by1(field,',') ',' '..' expr '}'  { Struct [] $1 (toList $3) (Just $6) ($1 # $>) }
  | expr_path '{' inner_attrs sep_by1(field,',') ',' '..' expr '}'  { Struct (toList $3) $1 (toList $4) (Just $7) ($1 # $>) }
  | expr_path '{'             sep_byT(field,',')               '}'  { Struct [] $1 $3 Nothing ($1 # $>) }
  | expr_path '{' inner_attrs sep_byT(field,',')               '}'  { Struct (toList $3) $1 $4 Nothing ($1 # $>) }

field :: { Field Span }
  : ident ':' expr  { Field (unspan $1) (Just $3) ($1 # $3) }
  | ident           { Field (unspan $1) Nothing (spanOf $1) }

-- an expression block that won't cause conflicts with stmts
vis_safety_block :: { Expr Span }
  : pub_or_inherited safety inner_attrs_block {%
       let (as, Block ss r x) = $3
           e = BlockExpr as (Block ss (unspan $2) ($2 # x)) ($2 # x)
       in noVis $1 e
    }

-- an expression starting with 'union' or 'default' (as identifiers) that won't cause conflicts with stmts
vis_union_def_nonblock_expr :: { Expr Span }
  : union_default_expr                                               { $1 }
  | left_gen_expression(vis_union_def_nonblock_expr, expr, expr) { $1 }

union_default_expr :: { Expr Span }
  : pub_or_inherited union         {%
      noVis $1 (PathExpr [] Nothing (Path False [PathSegment "union" Nothing (spanOf $2)] (spanOf $1)) (spanOf $1))
    }
  | pub_or_inherited default         {%
      noVis $1 (PathExpr [] Nothing (Path False [PathSegment "default" Nothing (spanOf $2)] (spanOf $1)) (spanOf $1))
    }


----------------
-- Statements --
----------------

stmt :: { Stmt Span }
  : ntStmt                                                 { $1 }
  | many(outer_attribute) let pat ':' ty initializer ';'   { Local $3 (Just $5) $6 $1 ($1 # $2 # $>) }
  | many(outer_attribute) let pat        initializer ';'   { Local $3 Nothing $4 $1 ($1 # $2 # $>) }
  | many(outer_attribute) nonblock_expr ';'                { toStmt ($1 `addAttrs` $2) True  False ($1 # $2 # $3) }
  | many(outer_attribute) block_like_expr ';'              { toStmt ($1 `addAttrs` $2) True  True  ($1 # $2 # $3) }
  | many(outer_attribute) blockpostfix_expr ';'            { toStmt ($1 `addAttrs` $2) True  True  ($1 # $2 # $3) }
  | many(outer_attribute) vis_union_def_nonblock_expr ';'  { toStmt ($1 `addAttrs` $2) True  False ($1 # $2 # $3) } 
  | many(outer_attribute) block_like_expr    %prec NOSEMI  { toStmt ($1 `addAttrs` $2) False True  ($1 # $2) }
  | many(outer_attribute) vis_safety_block ';'             { toStmt ($1 `addAttrs` $2) True True ($1 # $2 # $>) }
  | many(outer_attribute) vis_safety_block   %prec NOSEMI  { toStmt ($1 `addAttrs` $2) False True ($1 # $2) }
  | gen_item(pub_or_inherited)                             { ItemStmt $1 (spanOf $1) }
  | many(outer_attribute) expr_path '!' ident '[' token_stream ']' ';'
    { ItemStmt (macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) expr_path '!' ident '(' token_stream ')' ';'
    { ItemStmt (macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) expr_path '!' ident '{' token_stream '}'
    { ItemStmt (macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>)) ($1 # $2 # $>) }

pub_or_inherited :: { Spanned (Visibility Span) }
  : pub                                          %prec VIS { Spanned PublicV (spanOf $1) }
  | {- empty -}                                  %prec VIS { pure InheritedV }

stmtOrSemi :: { Maybe (Stmt Span) }
  : ';'                                                    { Nothing }
  | stmt                                                   { Just $1 }

-- List of statements where the last statement might be a no-semicolon statement.
stmts_possibly_no_semi :: { [Maybe (Stmt Span)] }
  : stmtOrSemi stmts_possibly_no_semi                      { $1 : $2 }
  | stmtOrSemi                                             { [$1] }
  | many(outer_attribute) nonblock_expr                    { [Just (toStmt ($1 `addAttrs` $2) False False ($1 # $2))] }
  | many(outer_attribute) blockpostfix_expr                { [Just (toStmt ($1 `addAttrs` $2) False True  ($1 # $2))] }

initializer :: { Maybe (Expr Span) }
  : '=' expr                                               { Just $2 }
  | {- empty -}                                            { Nothing }

block :: { Block Span }
  : ntBlock                                                { $1 }
  | '{' '}'                                                { Block [] Normal ($1 # $>) }
  | '{' stmts_possibly_no_semi '}'                         { Block [ s | Just s <- $2 ] Normal ($1 # $>) }

inner_attrs_block :: { ([Attribute Span], Block Span) }
  : block                                                  { ([], $1) }
  | '{' inner_attrs '}'                                    { (toList $2, Block [] Normal ($1 # $>)) }
  | '{' inner_attrs stmts_possibly_no_semi '}'             { (toList $2, Block [ s | Just s <- $3 ] Normal ($1 # $>)) }


-----------
-- Items --
-----------

-- Given the types of permitted visibilities, generate a rule for items. The reason this production
-- is useful over just having 'item :: { ItemSpan }' and then 'many(outer_attribute) vis item' is
-- that (1) not all items have visibility and (2) attributes and visibility are fields on the 'Item'
-- algebraic data type.
gen_item(vis) :: { Item Span }
  : many(outer_attribute) vis static     ident ':' ty '=' expr ';'
    { Static $1 (unspan $2) (unspan $4) $6 Immutable $8 ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis static mut ident ':' ty '=' expr ';'
    { Static $1 (unspan $2) (unspan $5) $7 Mutable $9 ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis const ident ':' ty '=' expr ';'
    { ConstItem $1 (unspan $2) (unspan $4) $6 $8 ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis type ident generics where_clause '=' ty ';'
    { TyAlias $1 (unspan $2) (unspan $4) $8 ($5 `withWhere` $6) ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis use use_tree ';'
    { Use $1 (unspan $2) $4 ($1 # $2 # $3 # $>) }
  | many(outer_attribute) vis safety extern crate ident ';'
    {% noSafety $3 (ExternCrate $1 (unspan $2) (unspan $6) Nothing ($1 # $2 # $4 # $>)) }
  | many(outer_attribute) vis safety extern crate ident as ident ';'
    {% noSafety $3 (ExternCrate $1 (unspan $2) (unspan $8) (Just (unspan $6)) ($1 # $2 # $4 # $>)) }
  | many(outer_attribute) vis const safety  fn ident generics fn_decl(arg_named) where_clause inner_attrs_block
    { Fn ($1 ++ fst $>) (unspan $2) (unspan $6) $8 (unspan $4) Const Rust ($7 `withWhere` $9) (snd $>) ($1 # $2 # $3 # snd $>) }
  | many(outer_attribute) vis safety extern abi fn ident generics fn_decl(arg_named) where_clause inner_attrs_block
    { Fn ($1 ++ fst $>) (unspan $2) (unspan $7) $9 (unspan $3) NotConst $5 ($8 `withWhere` $10) (snd $>) ($1 # $2 # $3 # $4 # snd $>) }
  | many(outer_attribute) vis safety            fn ident generics fn_decl(arg_named) where_clause inner_attrs_block
    { Fn ($1 ++ fst $>) (unspan $2) (unspan $5) $7 (unspan $3) NotConst Rust ($6 `withWhere` $8) (snd $>) ($1 # $2 # $3 # $4 # snd $>) }
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
  | many(outer_attribute) vis safety trait ident generics ':' sep_by1T(ty_param_bound,'+') where_clause '{' many(trait_item) '}'
    { Trait $1 (unspan $2) (unspan $5) False (unspan $3) ($6 `withWhere` $9) (toList $8) $11 ($1 # $2 # $3 # $4 # $>) }
  | many(outer_attribute) vis safety trait ident generics where_clause '{' many(trait_item) '}'
    { Trait $1 (unspan $2) (unspan $5) False (unspan $3) ($6 `withWhere` $7) [] $9 ($1 # $2 # $3 # $4 # $>) }
  | many(outer_attribute) vis safety auto trait ident generics ':' sep_by1T(ty_param_bound,'+') where_clause '{' many(trait_item) '}'
    { Trait $1 (unspan $2) (unspan $6) True (unspan $3) ($7 `withWhere` $10) (toList $9) $12 ($1 # $2 # $3 # $5 # $>) }
  | many(outer_attribute) vis safety auto trait ident generics where_clause '{' many(trait_item) '}'
    { Trait $1 (unspan $2) (unspan $6) True (unspan $3) ($7 `withWhere` $8) [] $10 ($1 # $2 # $3 # $5 # $>) }
  | many(outer_attribute) vis safety trait ident generics '=' sep_by1T(ty_param_bound,'+') ';'
    {% noSafety $3 (TraitAlias $1 (unspan $2) (unspan $5) $6 (toNonEmpty $8) ($1 # $2 # $3 # $>)) }
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

-- Most general type of item
mod_item :: { Item Span }
  : ntItem                                             { $1 }
  | gen_item(vis)                                      { $1 }
  | many(outer_attribute) expr_path '!' ident '[' token_stream ']' ';'
    { macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) expr_path '!'       '[' token_stream ']' ';'
    { macroItem $1 Nothing            (Mac $2 $5 ($2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) expr_path '!' ident '(' token_stream ')' ';'
    { macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) expr_path '!'       '(' token_stream ')' ';'
    { macroItem $1 Nothing            (Mac $2 $5 ($2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) expr_path '!' ident '{' token_stream '}'
    { macroItem $1 (Just (unspan $4)) (Mac $2 $6 ($2 # $>)) ($1 # $2 # $>) }
  | many(outer_attribute) expr_path '!'       '{' token_stream '}'
    { macroItem $1 Nothing            (Mac $2 $5 ($2 # $>)) ($1 # $2 # $>) }

foreign_item :: { ForeignItem Span }
  : many(outer_attribute) vis static     ident ':' ty ';'
    { ForeignStatic $1 (unspan $2) (unspan $4) $6 Immutable ($1 # $2 # $>) }
  | many(outer_attribute) vis static mut ident ':' ty ';'
    { ForeignStatic $1 (unspan $2) (unspan $5) $7 Mutable ($1 # $2 # $>) }
  | many(outer_attribute) vis fn ident generics fn_decl(arg_named) where_clause ';'
    { ForeignFn $1 (unspan $2) (unspan $4) $6 ($5 `withWhere` $7) ($1 # $2 # $>) }
  | many(outer_attribute) vis type ident ';'
    { ForeignTy $1 (unspan $2) (unspan $4) ($1 # $2 # $>) }

-- parse_generics
-- Leaves the WhereClause empty
generics :: { Generics Span }
  : ntGenerics                                                      { $1 }
  | '<' sep_by1(lifetime_def,',') ',' sep_by1T(ty_param,',') gt '>' { Generics (toList $2) (toList $4) (WhereClause [] mempty) ($1 # $>) }
  | '<' sep_by1T(lifetime_def,',')                           gt '>' { Generics (toList $2) []          (WhereClause [] mempty) ($1 # $>) }
  | '<'                               sep_by1T(ty_param,',') gt '>' { Generics []          (toList $2) (WhereClause [] mempty) ($1 # $>) }
  | '<'                                                      gt '>' { Generics []          []          (WhereClause [] mempty) ($1 # $>) }
  | {- empty -}                                                     { Generics []          []          (WhereClause [] mempty) mempty }

ty_param :: { TyParam Span }
  : many(outer_attribute) ident                                              { TyParam $1 (unspan $2) [] Nothing ($1 # $>) }
  | many(outer_attribute) ident ':' sep_by1T(ty_param_bound_mod,'+')         { TyParam $1 (unspan $2) (toList $4) Nothing ($1 # $>) }
  | many(outer_attribute) ident                                      '=' ty  { TyParam $1 (unspan $2) [] (Just $>) ($1 # $>) }
  | many(outer_attribute) ident ':' sep_by1T(ty_param_bound_mod,'+') '=' ty  { TyParam $1 (unspan $2) (toList $4) (Just $>) ($1 # $>) }


struct_decl_args :: { (WhereClause Span, VariantData Span) }
  : where_clause ';'                                         { ($1, UnitD ($1 # $>)) }
  | where_clause '{' sep_byT(struct_decl_field,',') '}'      { ($1, StructD $3 ($1 # $>)) }
  | '(' sep_byT(tuple_decl_field,',') ')' where_clause ';'   { ($4, TupleD $2 ($1 # $>)) }

struct_decl_field :: { StructField Span }
  : many(outer_attribute) vis ident ':' ty                   { StructField (Just (unspan $3)) (unspan $2) $5 $1 ($1 # $2 # $5) }

tuple_decl_field :: { StructField Span }
  : many(outer_attribute) vis ty                             { StructField Nothing (unspan $2) $3 $1 ($1 # $2 # $3) }

enum_def :: { Variant Span }
  : many(outer_attribute) ident '{' sep_byT(struct_decl_field,',') '}'  { Variant (unspan $2) $1 (StructD $4 ($3 # $5)) Nothing ($1 # $2 # $>) }
  | many(outer_attribute) ident '(' sep_byT(tuple_decl_field,',')  ')'  { Variant (unspan $2) $1 (TupleD $4 ($3 # $5)) Nothing ($1 # $2 # $>) }
  | many(outer_attribute) ident initializer                             { Variant (unspan $2) $1 (UnitD mempty) $3 ($1 # $2 # $>) }


-- parse_where_clause
where_clause :: { WhereClause Span }
  : {- empty -}                                        { WhereClause [] mempty }
  | ntWhereClause                                      { $1 }
  | where sep_by(where_predicate,',')      %prec WHERE { WhereClause $2 ($1 # $2) }
  | where sep_by1(where_predicate,',') ',' %prec WHERE { WhereClause (toList $2) ($1 # $3) }

where_predicate :: { WherePredicate Span }
  : lifetime                                               { RegionPredicate $1 [] (spanOf $1) }
  | lifetime ':' sep_by1T(lifetime,'+')                    { RegionPredicate $1 (toList $3) ($1 # $3) }
  | no_for_ty                                     %prec EQ { BoundPredicate [] $1 [] (spanOf $1) }
  | no_for_ty '='  ty                                      { EqPredicate $1 $3 ($1 # $3) }
  | no_for_ty '==' ty                                      { EqPredicate $1 $3 ($1 # $3) }
  | no_for_ty ':' sep_by1T(ty_param_bound_mod,'+')         { BoundPredicate [] $1 (toList $3) ($1 # $3) }
  | for_lts no_for_ty                                      { BoundPredicate (unspan $1) $2 [] ($1 # $2) }
  | for_lts no_for_ty ':' sep_by1T(ty_param_bound_mod,'+') { BoundPredicate (unspan $1) $2 (toList $4) ($1 # $>) }

impl_items :: { ([Attribute Span], [ImplItem Span]) }
  :             many(impl_item)  { ([], $1) }
  | inner_attrs many(impl_item)  { (toList $1, $2) }

impl_item :: { ImplItem Span }
  : ntImplItem                                          { $1 }
  | many(outer_attribute) vis def type ident '=' ty ';'           { TypeI $1 (unspan $2) (unspan $3) (unspan $5) $7 ($1 # $2 # $3 # $4 # $>) }
  | many(outer_attribute) vis def const ident ':' ty '=' expr ';' { ConstI $1 (unspan $2) (unspan $3) (unspan $5) $7 $9 ($1 # $2 # $3 # $4 # $>) }
  | many(outer_attribute)     def mod_mac                         { MacroI $1 (unspan $2) $3 ($1 # $2 # $>) }
  | many(outer_attribute) vis def const safety fn ident generics fn_decl_with_self_named where_clause inner_attrs_block
    { let methodSig = MethodSig (unspan $5) Const Rust $9; generics = $8 `withWhere` $10
      in MethodI ($1 ++ fst $>) (unspan $2) (unspan $3) (unspan $7) generics methodSig (snd $>) ($1 # $2 # $3 # $4 # snd $>) }
  | many(outer_attribute) vis def safety ext_abi fn ident generics fn_decl_with_self_named where_clause inner_attrs_block
    { let methodSig = MethodSig (unspan $4) NotConst (unspan $5) $9; generics = $8 `withWhere` $10
      in MethodI ($1 ++ fst $>) (unspan $2) (unspan $3) (unspan $7) generics methodSig (snd $>) ($1 # $2 # $3 # $4 # $5 # $6 # snd $>) }

trait_item :: { TraitItem Span }
  : ntTraitItem                                              { $1 }
  | many(outer_attribute) const ident ':' ty initializer ';' { ConstT $1 (unspan $3) $5 $6 ($1 # $2 # $>) }
  | many(outer_attribute) mod_mac                            { MacroT $1 $2 ($1 # $>) }
  | many(outer_attribute) type ident ';'                     { TypeT $1 (unspan $3) [] Nothing ($1 # $2 # $>) }
  | many(outer_attribute) type ident '=' ty ';'              { TypeT $1 (unspan $3) [] (Just $5) ($1 # $2 # $>) }
  | many(outer_attribute) type ident ':' sep_by1T(ty_param_bound_mod,'+') ';'
    { TypeT $1 (unspan $3) (toList $5) Nothing ($1 # $2 # $>) }
  | many(outer_attribute) type ident ':' sep_by1T(ty_param_bound_mod,'+') '=' ty ';'
    { TypeT $1 (unspan $3) (toList $5) (Just $7) ($1 # $2 # $>) }
  | many(outer_attribute) safety ext_abi fn ident generics fn_decl_with_self_general where_clause ';'
    { let methodSig = MethodSig (unspan $2) NotConst (unspan $3) $7; generics = $6 `withWhere` $8
      in MethodT $1 (unspan $5) generics methodSig Nothing ($1 # $2 # $3 # $4 # $>) }
  | many(outer_attribute) safety ext_abi fn ident generics fn_decl_with_self_general where_clause inner_attrs_block
    { let methodSig = MethodSig (unspan $2) NotConst (unspan $3) $7; generics = $6 `withWhere` $8
      in MethodT ($1 ++ fst $>) (unspan $5) generics methodSig (Just (snd $>)) ($1 # $2 # $3 # $4 # snd $>) }

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
  | crate                   { Spanned CrateV (spanOf $1) }
  | pub '(' in mod_path ')' { Spanned (RestrictedV $4) ($1 # $5) }
  | pub '(' super ')'       { Spanned (RestrictedV (Path False [PathSegment "super" Nothing (spanOf
  $3)] (spanOf $3))) ($1 # $4) }
  | pub '(' self ')'        { Spanned (RestrictedV (Path False [PathSegment "self" Nothing (spanOf
  $3)] (spanOf $3))) ($1 # $4) }

def :: { Spanned Defaultness }
  : {- empty -}  %prec DEF        { pure Final }
  | default              { Spanned Default (spanOf $1) }

use_tree :: { UseTree Span }
  : mod_path                                    { UseTreeSimple $1 Nothing (spanOf $1) }
  | mod_path as ident                           { UseTreeSimple $1 (Just (unspan $3)) ($1 # $3) }
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
  | box        { $1 }
  | break      { $1 }
  | const      { $1 }
  | continue   { $1 }
  | crate      { $1 }
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
  | type       { $1 }
  | unsafe     { $1 }
  | use        { $1 }
  | where      { $1 }
  | while      { $1 }
  -- Keywords reserved for future use
  | abstract   { $1 }
  | alignof    { $1 }
  | become     { $1 }
  | do         { $1 }
  | final      { $1 }
  | macro      { $1 }
  | offsetof   { $1 }
  | override   { $1 }
  | priv       { $1 }
  | proc       { $1 }
  | pure       { $1 }
  | sizeof     { $1 }
  | typeof     { $1 }
  | unsized    { $1 }
  | virtual    { $1 }
  -- Weak keywords, have special meaning only in specific contexts.
  | default    { $1 }
  | union      { $1 }
  | catch      { $1 }
  | auto       { $1 }
  | yield      { $1 }
  | dyn        { $1 }
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
  | safety '{' stmts_possibly_no_semi '}'                  { Block [ s | Just s <- $3 ] (unspan $1) ($1 # $2 # $>) }

{
-- | Parser for literals.
parseLit :: P (Lit Span)

-- | Parser for attributes.
parseAttr :: P (Attribute Span)

-- | Parser for types.
parseTy :: P (Ty Span)

-- | Parser for patterns.
parsePat :: P (Pat Span)

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

-- | Parser for lifetime definitions.
parseLifetimeDef :: P (LifetimeDef Span)

-- | Parser for a type parameter.
parseTyParam :: P (TyParam Span)

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

  ignore = words "ntItem ntBlock ntStmt ntPat ntExpr ntTy ntIdent ntPath ntTT \
                  ntArm ntImplItem ntTraitItem ntGenerics ntWhereClause ntArg ntLit"

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
                words "'<' '!' '-' '*' '&' '|' '...' '..=' '..' '::' \
                       '||' '&&' '<<' '(' '[' '{' box break continue \
                       for if loop match move return Self self       \
                       static super unsafe while do default union    \
                       catch auto yield dyn"

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

  identifier = words "IDENT"
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

-- | Return the second argument, as long as the visibility is 'InheritedV'
noVis :: Spanned (Visibility Span) -> a -> P a
noVis (Spanned InheritedV _) x = pure x
noVis _ _ = fail "visibility is not allowed here"

-- | Fill in the where clause in a generic
withWhere :: Generics a -> WhereClause a -> Generics a
withWhere (Generics l t _ x) w = Generics l t w x

-- | Return the second argument, as long as the safety is 'Normal'
noSafety :: Spanned Unsafety -> a -> P a
noSafety (Spanned Normal _) x = pure x
noSafety _ _ = fail "safety is not allowed here"

-- | Make a macro item, which may be a 'MacroDef'
macroItem :: [Attribute Span] -> (Maybe Ident) -> Mac Span -> Span -> Item Span
macroItem as (Just i) (Mac (Path False [PathSegment "macro_rules" Nothing _] _) tts _) x = MacroDef as i tts x
macroItem as i mac x = MacItem as i mac x

-- | Add attributes to an expression
addAttrs :: [Attribute Span] -> Expr Span -> Expr Span
addAttrs as (Box as' e s)            = Box (as ++ as') e s
addAttrs as (InPlace as' e1 e2 s)    = InPlace (as ++ as') e1 e2 s
addAttrs as (Vec as' e s)            = Vec (as ++ as') e s
addAttrs as (Call as' f es s)        = Call (as ++ as') f es s
addAttrs as (MethodCall as' i s tys es s') = MethodCall (as ++ as') i s tys es s'
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
addAttrs as (Closure as' m c f e s)  = Closure (as ++ as') m c f e s
addAttrs as (BlockExpr as' b s)      = BlockExpr (as ++ as') b s
addAttrs as (Catch as' b s)          = Catch (as ++ as') b s
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
lit :: Spanned Token -> Lit Span
lit (Spanned (IdentTok (Ident "true" False _)) s) = Bool True Unsuffixed s
lit (Spanned (IdentTok (Ident "false" False _)) s) = Bool False Unsuffixed s
lit (Spanned (LiteralTok litTok suffix_m) s) = translateLit litTok suffix s
  where
    suffix = case suffix_m of
               Nothing -> Unsuffixed
               (Just "isize") -> Is
               (Just "usize") -> Us
               (Just "i8")    -> I8
               (Just "u8")    -> U8
               (Just "i16")   -> I16
               (Just "u16")   -> U16
               (Just "i32")   -> I32
               (Just "u32")   -> U32
               (Just "i64")   -> I64
               (Just "u64")   -> U64
               (Just "i128")  -> I128
               (Just "u128")  -> U128
               (Just "f32")   -> F32
               (Just "f64")   -> F64
               _ -> error "invalid literal"

isTraitTyParamBound TraitTyParamBound{} = True
isTraitTyParamBound _ = False

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
