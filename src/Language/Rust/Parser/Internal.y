{
{-|
Module      : Language.Rust.Parser.Internal
Description : Rust parser
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

The parsers in this file are all re-exported to 'Language.Rust.Parser' via the 'Parse' class. The
parsers are based off:

  * the reference @rustc@ [implementation](https://github.com/rust-lang/rust/blob/master/src/libsyntax/parse/parser.rs)
  * a slightly outdated [ANTLR grammar](https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y)
  * some documentation on [rust-lang](https://doc.rust-lang.org/grammar.html)

To log Happy's debug information (about transition states and such), run @happy --info=happyinfo.txt
-o /dev/null src/Language/Rust/Parser/Internal.y@.
-}
{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}

module Language.Rust.Parser.Internal (
  parseLit, parseAttr, parseTy, parsePat, parseStmt, parseExpr, parseItem, parseSourceFile,
  parseBlock, parseImplItem, parseTraitItem, parseTt,
) where

import Language.Rust.Syntax
import Language.Rust.Data.Position
import Language.Rust.Parser.Lexer (lexNonSpace, lexShebangLine)
import Language.Rust.Parser.ParseMonad (pushToken, getPosition, P, parseError)
import Language.Rust.Parser.Literals (translateLit)

import Data.List.NonEmpty (NonEmpty(..), (<|), toList)
import qualified Data.List.NonEmpty as N
import Data.Semigroup ((<>))
import Text.Read (readMaybe)
}

-- in order to document the parsers, we have to alias them
%name parseLit lit
%name parseAttr attribute
%name parseTy ty_general
%name parsePat pat
%name parseStmt stmt
%name parseExpr expr
%name parseItem mod_item
%name parseSourceFileContents source_file
%name parseBlock block
%name parseImplItem impl_item
%name parseTraitItem trait_item
%name parseTt token_tree

%tokentype { Spanned Token }

%monad { P } { >>= } { return }
%error { parseError }
%lexer { lexNonSpace >>= } { Spanned Eof _ }

-- Conflicts caused in
--  * (1) around the '::' in path_segments_without_colons
--  * (1) around the '=' in where_clause
-- However, they are all S/R and seem to be currently doing what they should
%expect 2

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
  yield          { Spanned (IdentTok "yield") _ }

  -- Weak keywords, have special meaning only in specific contexts.
  default        { Spanned (IdentTok "default") _ }
  union          { Spanned (IdentTok "union") _ }
  catch          { Spanned (IdentTok "catch") _ }

  -- Comments
  outerDoc       { Spanned (Doc _ OuterDoc) _ }
  innerDoc       { Spanned (Doc _ InnerDoc) _ }

  -- Identifiers.
  IDENT          { Spanned IdentTok{} _ }
  '_'            { Spanned Underscore _ }

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
  ntMeta         { Spanned (Interpolated (NtMeta $$)) _ }
  ntPath         { Spanned (Interpolated (NtPath $$)) _ }
  ntTT           { Spanned (Interpolated (NtTT $$)) _ }
  ntArm          { Spanned (Interpolated (NtArm $$)) _ }
  ntImplItem     { Spanned (Interpolated (NtImplItem $$)) _ }
  ntTraitItem    { Spanned (Interpolated (NtTraitItem $$)) _ }
  ntGenerics     { Spanned (Interpolated (NtGenerics $$)) _ }
  ntWhereClause  { Spanned (Interpolated (NtWhereClause $$)) _ }
  ntArg          { Spanned (Interpolated (NtArg $$)) _ }
  ntLit          { Spanned (Interpolated (NtLit $$)) _ }


-- This needs to be lower precedence than 'IDENT' so that in 'pat', something like "&mut x"
-- associates the "mut" to a refence pattern and not to the identifier pattern "x".
%nonassoc mut

-- These are all identifiers of sorts ('union' and 'default' are "weak" keywords)
%nonassoc IDENT ntIdent union default catch

-- These are all very low precedence unary operators
%nonassoc box return break continue IMPLTRAIT

-- These are the usual arithmetic precedences. 'UNARY' is introduced here for '*', '!', '-', '&'
%right '=' '>>=' '<<=' '-=' '+=' '*=' '/=' '^=' '|=' '&=' '%='
%right '<-'
%nonassoc SINGLERNG
%nonassoc INFIXRNG
%nonassoc POSTFIXRNG
%nonassoc PREFIXRNG
%nonassoc '..' '...'
%left '||'
%left '&&'
%left '==' '!=' '<' '>' '<=' '>='
%left '|'
%left '^'
%left '&'
%left '<<' '>>'
%left '+' '-'
%left '*' '/' '%'
%left ':' as
%left UNARY

-- These are all generated precedence tokens.
--
--  * 'POSTFIX' for postfix operators (which bind more tightly than pretty much anything but parens)
--  * 'VIS' for adjusting the precedence of 'pub' compared to other visbility modifiers (see 'vis')
--  * 'PATH' boosts the precedences of paths in types and expressions
--  * 'DOLLAR' is for a single '$' token not in sequence expression
--  * 'WHERE' is for non-empty where clauses
--
%nonassoc POSTFIX VIS PATH DOLLAR WHERE

-- Delimiters have the highest precedence. 'ntBlock' counts as a delimiter since it always starts
-- and ends with '{' and '}'
%nonassoc '{' ntBlock '[' '(' '!'

%%

-- Unwraps the IdentTok into just an Ident
-- For questionable reasons of backwards compatibility, 'union' and 'default' can be used as
-- identifiers, even if they are also keywords. They are "contextual" keywords.
--
-- Union's RFC: https://github.com/rust-lang/rfcs/blob/master/text/1444-union.md
ident :: { Spanned Ident }
  : ntIdent                       { fmap (\(Interpolated (NtIdent i)) -> i) $1 }
  | union                         { toIdent $1 }
  | default                       { toIdent $1 }
  | catch                         { toIdent $1 }
  | IDENT                         { toIdent $1 }

-- This should precede any '>' token which could be absorbed in a '>>', '>=', or '>>=' token. Its
-- purpose is to check if the lookahead token starts with '>' but contains more that. If that is
-- the case, it pushes two tokens, the first of which is '>'. We exploit the %% feature of threaded
-- lexers to discard what would have been the troublesome '>>', '>=', or '>>=' token.
gt :: { () }
  : {- empty -}   {%% \(Spanned tok s) ->
      let s' = nudge 1 0 s; s'' = nudge 0 (-1) s
      in case tok of
           GreaterGreater      -> pushToken (Spanned Greater s')      *> pushToken (Spanned Greater s'')
           GreaterEqual        -> pushToken (Spanned Equal s')        *> pushToken (Spanned Greater s'')
           GreaterGreaterEqual -> pushToken (Spanned GreaterEqual s') *> pushToken (Spanned Greater s'')
           _                   -> pushToken (Spanned tok s)
    }

-------------
-- Utility --
-------------

-- | One or more occurences of 'p'
some(p) :: { NonEmpty a }
  : some(p) p          { $1 |> $2 }
  | p                  { [$1] }

-- | Zero or more occurences of 'p'
many(p) :: { [a] }
  : some(p)            { toList $1 }
  | {- empty -}        { [] }

-- | One or more occurences of 'p', seperated by 'sep'
sep_by1(p,sep) :: { NonEmpty a }
  : sep_by1(p,sep) sep p  { $1 |> $3 }
  | p                     { [$1] }

-- | Zero or more occurrences of 'p', separated by 'sep'
sep_by(p,sep) :: { [a] }
  : sep_by1(p,sep)     { toList $1 }
  | {- empty -}        { [] }

-- | One or more occurrences of 'p', seperated by 'sep', optionally ending in 'sep'
sep_by1T(p,sep) :: { NonEmpty a }
  : sep_by1(p,sep) sep { $1 }
  | sep_by1(p,sep)     { $1 }

-- | Zero or more occurences of 'p', seperated by 'sep', optionally ending in 'sep' (only if there
-- is at least one 'p')
sep_byT(p,sep) :: { [a] }
  : sep_by1T(p,sep)    { toList $1 }
  | {- empty -}        { [] }


--------------------------
-- Whole file
--------------------------

-- shebang is dealt with at the top level, outside Happy/Alex
source_file :: { ([Attribute a],[Item a]) }
  : inner_attrs many(mod_item)   { (toList $1, $2) }
  |             many(mod_item)   { ([],        $1) }


--------------------------
-- Attributes
--------------------------

attribute :: { Attribute Span }
  : inner_attribute { $1 }
  | outer_attribute { $1 }

outer_attribute :: { Attribute Span }
  : '#' '[' meta_item ']'        { Attribute Outer $3 False ($1 # $>) }
  | outerDoc                     { mkDocAttribute $1 }

inner_attribute :: { Attribute Span }
  : '#' '!' '[' meta_item ']'    { Attribute Inner $4 False ($1 # $>) }
  | '#!'    '[' meta_item ']'    { Attribute Inner $3 False ($1 # $>) }
  | innerDoc                     { mkDocAttribute $1 }

-- TODO: for some precedence related reason, using 'some' here doesn't work
inner_attrs :: { NonEmpty (Attribute Span) }
  : inner_attrs inner_attribute  { $1 |> $2 }
  | inner_attribute              { [$1] }


-- parse_meta_item()
meta_item :: { MetaItem Span }
  : ntMeta                                         { $1 }
  | any_ident                                      { Word (unspan $1) (spanOf $1) }
  | any_ident '=' unsuffixed                       { NameValue (unspan $1) $3 ($1 # $>) }
  | any_ident '(' sep_byT(meta_item_inner,',') ')' { List (unspan $1) $3 ($1 # $>) }

-- parse_meta_item_inner()
meta_item_inner :: { NestedMetaItem Span }
  : unsuffixed                                     { Literal $1 (spanOf $1) }
  | meta_item                                      { MetaItem $1 (spanOf $1) }

-- All identifiers should be accepted in meta items, even those that are keywords
any_ident :: { Spanned Ident }
  : ident                  { $1 }
  | as         %prec IDENT { toIdent $1 } 
  | box        %prec IDENT { toIdent $1 }
  | break      %prec IDENT { toIdent $1 }
  | const      %prec IDENT { toIdent $1 }
  | continue   %prec IDENT { toIdent $1 }
  | crate      %prec IDENT { toIdent $1 }
  | else       %prec IDENT { toIdent $1 }
  | enum       %prec IDENT { toIdent $1 }
  | extern     %prec IDENT { toIdent $1 }
  | fn         %prec IDENT { toIdent $1 }
  | for        %prec IDENT { toIdent $1 }
  | if         %prec IDENT { toIdent $1 }
  | impl       %prec IDENT { toIdent $1 }
  | in         %prec IDENT { toIdent $1 }
  | let        %prec IDENT { toIdent $1 }
  | loop       %prec IDENT { toIdent $1 }
  | match      %prec IDENT { toIdent $1 }
  | mod        %prec IDENT { toIdent $1 }
  | move       %prec IDENT { toIdent $1 }
  | mut        %prec IDENT { toIdent $1 }
  | pub        %prec IDENT { toIdent $1 }
  | ref        %prec IDENT { toIdent $1 }
  | return     %prec IDENT { toIdent $1 }
  | Self       %prec IDENT { toIdent $1 }
  | self       %prec IDENT { toIdent $1 }
  | static     %prec IDENT { toIdent $1 }
  | struct     %prec IDENT { toIdent $1 }
  | super      %prec IDENT { toIdent $1 }
  | trait      %prec IDENT { toIdent $1 }
  | type       %prec IDENT { toIdent $1 }
  | unsafe     %prec IDENT { toIdent $1 }
  | use        %prec IDENT { toIdent $1 }
  | where      %prec IDENT { toIdent $1 }
  | while      %prec IDENT { toIdent $1 }
  | abstract   %prec IDENT { toIdent $1 }
  | alignof    %prec IDENT { toIdent $1 }
  | become     %prec IDENT { toIdent $1 }
  | do         %prec IDENT { toIdent $1 }
  | final      %prec IDENT { toIdent $1 }
  | macro      %prec IDENT { toIdent $1 }
  | offsetof   %prec IDENT { toIdent $1 }
  | override   %prec IDENT { toIdent $1 }
  | priv       %prec IDENT { toIdent $1 }
  | proc       %prec IDENT { toIdent $1 }
  | pure       %prec IDENT { toIdent $1 }
  | sizeof     %prec IDENT { toIdent $1 }
  | typeof     %prec IDENT { toIdent $1 }
  | unsized    %prec IDENT { toIdent $1 }
  | virtual    %prec IDENT { toIdent $1 }
  | yield      %prec IDENT { toIdent $1 }


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

unsuffixed :: { Lit Span }
  : lit               {%
      case suffix $1 of
        Unsuffixed -> pure $1
        _ -> fail "expected unsuffixed literal"
    }


-----------
-- Paths --
-----------

-- parse_qualified_path(PathStyle::Type)
-- qual_path :: Spanned (NonEmpty (Ident, PathParameters Span)) -> P (Spanned (QSelf Span, Path Span))
qual_path(segs) :: { Spanned (QSelf Span, Path Span) }
  : '<' qual_path_suf(segs)                     { let Spanned x _ = $2 in Spanned x ($1 # $2) }
  | lt_ty_qual_path as ty_path '>' '::' segs   {
      let segs = segments $3 <> unspan $6
      in Spanned (QSelf (unspan $1) (length (segments $3)), $3{ segments = segs }) ($1 # $>)
    }

-- Basically a qualified path, but ignoring the very first '<' token
qual_path_suf(segs) :: { Spanned (QSelf Span, Path Span) }
  : ty '>' '::' segs                { Spanned (QSelf $1 0, Path False (unspan $4) (spanOf $4)) ($1 # $>) }
  | ty as ty_path '>' '::' segs     {
      let segs = segments $3 <> unspan $6
      in Spanned (QSelf $1 (length (segments $3)), $3{ segments = segs }) ($1 # $>) 
    }

-- Usually qual_path_suf is for... type paths! This consumes these but with a starting '<<' token.
-- The underlying type has the right 'Span' (it doesn't include the very first '<', while the
-- 'Spanned' wrapper does) 
lt_ty_qual_path :: { Spanned (Ty Span) }
  : '<<' qual_path_suf(path_segments_without_colons)
     { let (qself,path) = unspan $2 in Spanned (PathTy (Just qself) path (nudge 1 0 ($1 # $2))) ($1 # $2) }

-- parse_generic_args() but with the '<' '>'
generic_values :: { Spanned ([Lifetime Span], [Ty Span], [(Ident, Ty Span)]) }
  : '<' sep_by1(lifetime,',')  ',' sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>' { Spanned (toList $2,      toList $4, toList $6) ($1 # $>) }
  | '<' sep_by1(lifetime,',')  ',' sep_by1T(ty,',')                          gt '>' { Spanned (toList $2,      toList $4, []       ) ($1 # $>) }
  | '<' sep_by1(lifetime,',')  ','                     sep_by1T(binding,',') gt '>' { Spanned (toList $2,      [],        toList $4) ($1 # $>) }
  | '<' sep_by1T(lifetime,',')                                               gt '>' { Spanned (toList $2,      [],        []       ) ($1 # $>) }
  | '<'                            sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>' { Spanned ([],             toList $2, toList $4) ($1 # $>) }
  | '<'                            sep_by1T(ty,',')                          gt '>' { Spanned ([],             toList $2, []       ) ($1 # $>) }
  | '<'                                                sep_by1T(binding,',') gt '>' { Spanned ([],             [],        toList $2) ($1 # $>) }
  | '<'                                                                      gt '>' { Spanned ([],             [],        []       ) ($1 # $>) }
  | lt_ty_qual_path            ',' sep_by1(ty,',') ',' sep_by1T(binding,',') gt '>' { Spanned ([], unspan $1 : toList $3, toList $5) ($1 # $>) }
  | lt_ty_qual_path            ',' sep_by1T(ty,',')                          gt '>' { Spanned ([], unspan $1 : toList $3, []       ) ($1 # $>) }
  | lt_ty_qual_path                                ',' sep_by1T(binding,',') gt '>' { Spanned ([],            [unspan $1],toList $3) ($1 # $>) }
  | lt_ty_qual_path                                                          gt '>' { Spanned ([],            [unspan $1],[]       ) ($1 # $>) }

binding : ident '=' ty                             { (unspan $1, $3) }


-- Type related:
-- parse_path(PathStyle::Type)
ty_path :: { Path Span }
  : ntPath                                   { $1 }
  | path_segments_without_colons             { Path False (unspan $1) (spanOf $1) }
  | '::' path_segments_without_colons        { Path True (unspan $2) ($1 # $2) }

ty_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(path_segments_without_colons)  { $1 }

-- parse_path_segments_without_colons()
path_segments_without_colons :: { Spanned (NonEmpty (Ident, PathParameters Span)) }
  : sep_by1(path_segment_without_colons, '::')  { sequence $1 }

-- No corresponding function - see path_segments_without_colons
path_segment_without_colons :: { Spanned (Ident, PathParameters Span) }
  : Self path_parameter1                     { Spanned ("Self", $2) ($1 # $>) }
  | ident path_parameter1                    { Spanned (unspan $1, $2) ($1 # $>) }

path_parameter1 :: { PathParameters Span }
  : generic_values                           { let (lts, tys, bds) = unspan $1 in AngleBracketed lts tys bds (spanOf $1) }
  | '(' sep_byT(ty,',') ')'                  { Parenthesized $2 Nothing ($1 # $>) }
  | '(' sep_byT(ty,',') ')' '->' ty_no_plus  { Parenthesized $2 (Just $>) ($1 # $>) }
  | {- empty -}                  %prec IDENT { NoParameters mempty }


-- Expression related:
-- parse_path(PathStyle::Expr)
expr_path :: { Path Span }
  : ntPath                                   { $1 }
  | path_segments_with_colons                { Path False (unspan $1) (spanOf $1) }
  | '::' path_segments_with_colons           { Path True (unspan $2) ($1 # $2) }

expr_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(path_segments_with_colons)     { $1 }

-- parse_path_segments_with_colons()
path_segments_with_colons :: { Spanned (NonEmpty (Ident, PathParameters Span)) }
  : self_or_ident
     { Spanned [(unspan $1, NoParameters mempty)] (spanOf $1) }
  | path_segments_with_colons '::' self_or_ident
     { Spanned (unspan $1 |> (unspan $3, NoParameters mempty)) ($1 # $>) }
  | path_segments_with_colons '::' generic_values
     {%
       case (N.last (unspan $1), unspan $3) of
         ((i, NoParameters{}), (lts, tys, bds)) -> let seg = (i, AngleBracketed lts tys bds (spanOf $3))
                                                   in pure $ Spanned (N.init (unspan $1) |: seg) ($1 # $3)
         _ -> error "invalid path segment in expression path"
    }

-- Mod related:
-- parse_path(PathStyle::Mod)
mod_path :: { Path Span  }
  : ntPath               { $1 }
  | self_or_ident        { Path False [(unspan $1, NoParameters mempty)] (spanOf $1) }
  | '::' self_or_ident   { Path True  [(unspan $2, NoParameters mempty)] ($1 # $>) }
  | mod_path '::' ident  {
       Path (global $1) (segments $1 |> (unspan $3, NoParameters mempty)) ($1 # $>)
     }


-----------
-- Types --
-----------

lifetime :: { Lifetime Span }
  : LIFETIME                         { let Spanned (LifetimeTok (Ident l _)) s = $1 in Lifetime l s }

-- parse_trait_ref()
trait_ref :: { TraitRef Span }
  : ty_path                          { TraitRef $1 }

-- All types, including trait types with plus and impl trait types
ty_general :: { Ty Span }
  : ty                               { $1 }
  | impl_ty                          { $1 }

-- parse_ty()
-- See https://github.com/rust-lang/rfcs/blob/master/text/0438-precedence-of-plus.md
-- All types, including trait types with plus
ty :: { Ty Span }
  : ty_no_plus                                                    { $1 }
  | poly_trait_ref_mod_bound '+' sep_by1T(ty_param_bound_mod,'+') { TraitObject ($1 <| $3) ($1 # $3) }

-- parse_ty_no_plus()
ty_no_plus :: { Ty Span }
  : ntTy                             { $1 }
  | no_for_ty                        { $1 }
  | for_ty_no_plus                   { $1 }

-- All types not starting with a '(' or '<'
ty_prim :: { Ty Span }
  : no_for_ty_prim                   { $1 }
  | for_ty_no_plus                   { $1 }
  | poly_trait_ref_mod_bound '+' sep_by1T(ty_param_bound_mod,'+') { TraitObject ($1 <| $3) ($1 # $3) }

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
  | '&'  lifetime_mut ty_no_plus     { Rptr (fst $2) (snd $2) $3 ($1 # $>) }
  | '&&' lifetime_mut ty_no_plus     { Rptr Nothing Immutable (Rptr (fst $2) (snd $2) $3 ($1 # $>)) ($1 # $>) }
  | ty_path               %prec PATH { PathTy Nothing $1 ($1 # $>) }
  | ty_mac                           { MacTy $1 ($1 # $>) }
  | unsafe extern abi fn fn_decl     { BareFn Unsafe $3 [] $> ($1 # $>) }
  | unsafe fn fn_decl                { BareFn Unsafe Rust [] $> ($1 # $>) }
  | extern abi fn fn_decl            { BareFn Normal $2 [] $> ($1 # $>) }
  | fn fn_decl                       { BareFn Normal Rust [] $> ($1 # $>) }
  | typeof '(' expr ')'              { Typeof $3 ($1 # $>) }
  | '[' ty ';' expr ']'              { Array $2 $4 ($1 # $>) }
  | '?' trait_ref                    { TraitObject [TraitTyParamBound (PolyTraitRef [] $2 (spanOf $2)) Maybe ($1 # $2)] ($1 # $2) }
  | '?' for_lts trait_ref            { TraitObject [TraitTyParamBound (PolyTraitRef (unspan $2) $3 ($2 # $3)) Maybe ($1 # $3)] ($1 # $3) }

-- All (non-sum) types starting with a 'for'
for_ty_no_plus :: { Ty Span }
  : for_lts unsafe extern abi fn fn_decl { BareFn Unsafe $4 (unspan $1) $> ($1 # $>) }
  | for_lts unsafe fn fn_decl            { BareFn Unsafe Rust (unspan $1) $> ($1 # $>) }
  | for_lts extern abi fn fn_decl        { BareFn Normal $3 (unspan $1) $> ($1 # $>) }
  | for_lts fn fn_decl                   { BareFn Normal Rust (unspan $1) $> ($1 # $>) }
  | for_lts trait_ref                    {
      let poly = PolyTraitRef (unspan $1) $2 ($1 # $2)
      in TraitObject [TraitTyParamBound poly None ($1 # $2)] ($1 # $2)
    }

impl_ty :: { Ty Span }
  : impl sep_by1(ty_param_bound_mod,'+') %prec IMPLTRAIT { ImplTrait $2 ($1 # $2) }

-- An optional lifetime followed by an optional mutability
lifetime_mut :: { (Maybe (Lifetime Span), Mutability) }
  : lifetime mut  { (Just $1, Mutable) }
  | lifetime      { (Just $1, Immutable) }
  |          mut  { (Nothing, Mutable) }
  | {- empty -}   { (Nothing, Immutable) }

-- The argument list and return type in a function
fn_decl :: { FnDecl Span }
  : '(' sep_by1(arg_general,',') ',' '...' ')' ret_ty  { FnDecl (toList $2) $> True ($1 # $5 # $6) }
  | '(' sep_byT(arg_general,',')           ')' ret_ty  { FnDecl $2 $> False ($1 # $3 # $4) }

-- Like 'fn_decl', but also accepting a self argument
fn_decl_with_self :: { FnDecl Span }
  : '(' arg_self ',' sep_byT(arg_general,',') ')' ret_ty  { FnDecl ($2 : $4) $> False ($1 # $5 # $6) }
  | '(' arg_self                              ')' ret_ty  { FnDecl [$2] $> False ($1 # $3 # $4) }
  | fn_decl                                               { $1 }


-- parse_ty_param_bounds(BoundParsingMode::Bare) == sep_by1(ty_param_bound,'+')
ty_param_bound :: { TyParamBound Span }
  : lifetime             { RegionTyParamBound $1 (spanOf $1) }
  | poly_trait_ref       { TraitTyParamBound $1 None (spanOf $1) }

poly_trait_ref_mod_bound :: { TyParamBound Span }
  : poly_trait_ref       { TraitTyParamBound $1 None (spanOf $1) }
  | '?' poly_trait_ref   { TraitTyParamBound $2 Maybe ($1 # $2) }

-- parse_ty_param_bounds(BoundParsingMode::Modified) == sep_by1(ty_param_bound_mod,'+')
ty_param_bound_mod :: { TyParamBound Span }
  : ty_param_bound       { $1 }
  | '?' poly_trait_ref   { TraitTyParamBound $2 Maybe ($1 # $2) }


-- parse_arg_general(false) -- does not require name
-- NOT ALL PATTERNS ARE ACCEPTED: <https://github.com/rust-lang/rust/issues/35203>
arg_general :: { Arg Span }
  :               ty  { Arg Nothing $1 (spanOf $1) }
  |     ident ':' ty  { Arg (Just (IdentP (ByValue Immutable) (unspan $1) Nothing (spanOf $1))) $3 ($1 # $3) }
  | mut ident ':' ty  { Arg (Just (IdentP (ByValue Mutable) (unspan $2) Nothing ($1 # $2))) $4 ($1 # $4) }
  |     '_'   ':' ty  { Arg (Just (WildP (spanOf $1))) $3 ($1 # $3) }

arg_self :: { Arg Span }
  :                  self { SelfValue Immutable ($1 # $>) }
  |              mut self { SelfValue Mutable ($1 # $>) }
  | '&' lifetime_mut self { SelfRegion (fst $2) (snd $2) ($1 # $>) }
  |     self ':' ty       { SelfExplicit $3 Immutable ($1 # $>) }
  | mut self ':' ty       { SelfExplicit $4 Mutable ($1 # $>) }


-- Sort of like parse_opt_abi() -- currently doesn't handle raw string ABI
abi :: { Abi }
  : str             {% case unspan $1 of
                         (LiteralTok (StrTok s) Nothing) -> maybe (fail "invalid ABI") pure (readMaybe s)
                         _ -> fail "invalid ABI"
                    }
  | {- empty -}     { C }

-- parse_ret_ty
-- Note that impl traits are still at RFC stage - they may eventually become accepted in more places
-- than just return types.
ret_ty :: { Maybe (Ty Span) }
  : '->' ty_no_plus                                  { Just $2 }
  | '->' impl_ty                                     { Just $2 }
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


--------------
-- Patterns --
--------------

-- TODO: Double-check that the error message in the one element tuple case makes sense. It should...
--
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
         Path False ((i, NoParameters _) :| []) _ -> IdentP (ByValue Immutable) i Nothing (spanOf $1)
         _                                        -> PathP Nothing $1 (spanOf $1)
    }
  | expr_qual_path                  { PathP (Just (fst (unspan $1))) (snd (unspan $1)) ($1 # $>) }
  | lit_or_path '...' lit_or_path   { RangeP $1 $3 ($1 # $>) }
  | expr_path '{' '..' '}'          { StructP $1 [] True ($1 # $>) }
  | expr_path '{' pat_fields '}'    { let (fs,b) = $3 in StructP $1 fs b ($1 # $>) }
  | expr_path '(' pat_tup ')'       { let (ps,m,_) = $3 in TupleStructP $1 ps m ($1 # $>) }
  | expr_mac                        { MacP $1 (spanOf $1) }
  | '[' pat_slice ']'               { let (b,s,a) = $2 in SliceP b s a ($1 # $3) }
  | '(' pat_tup ')'                 {%
      case $2 of
        ([p], Nothing, False) -> fail "Syntax error: the symbol `)' does not fit here"
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
  | sep_by1(pat,',')     '..' ',' sep_by1T(pat,',')    { (N.init $1, Just (N.last $1),    toList $4) }
  | sep_by1(pat,',')     '..'                          { (N.init $1, Just (N.last $1),    []) }
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

-- General postfix expression
gen_postfix_expr(lhs) :: { Expr Span }
  : ntExpr                                   { $1 }
  | lit_expr                                 { $1 }
  | expr_path                     %prec PATH { PathExpr [] Nothing $1 (spanOf $1) }
  | expr_qual_path                           { PathExpr [] (Just (fst (unspan $1))) (snd (unspan $1)) (spanOf $1) }
  | expr_mac                                 { MacExpr [] $1 (spanOf $1) }
  | '[' sep_byT(expr,',') ']'                { Vec [] $2 ($1 # $>) }
  | '[' expr ';' expr ']'                    { Repeat [] $2 $4 ($1 # $>) }
  | lhs '[' expr ']'                         { Index [] $1 $3 ($1 # $>) }
  | lhs '?'                                  { Try [] $1 ($1 # $>) }
  | lhs '(' sep_byT(expr,',') ')'            { Call [] $1 $3 ($1 # $>) }
  | lhs '.' ident '(' sep_byT(expr,',') ')'  { MethodCall [] $1 (unspan $3) Nothing $5 ($1 # $>) }
  | lhs '.' ident              %prec POSTFIX { FieldAccess [] $1 (unspan $3) ($1 # $>) }
  | lhs '.' ident '::' '<' sep_byT(ty,',') '>' '(' sep_byT(expr,',') ')'
     { MethodCall [] $1 (unspan $3) (Just $6) $9 ($1 # $>) }
  | lhs '.' int                              {%
      case lit $3 of
        Int Dec i Unsuffixed _ -> pure (TupField [] $1 (fromIntegral i) ($1 # $3))
        _ -> fail "make better error message"
    }

-- Arithmetic (unary and binary) generalized expressions. Precedences are handled by Happy (right
-- at the end of the token section)
gen_arithmetic(lhs,rhs,rhs2) :: { Expr Span }
  : '*' lhs          %prec UNARY     { Unary [] Deref $2 ($1 # $>) }
  | '!' lhs          %prec UNARY     { Unary [] Not $2 ($1 # $>) }
  | '-' lhs          %prec UNARY     { Unary [] Neg $2 ($1 # $>) }
  | '&'      lhs     %prec UNARY     { AddrOf [] Immutable $2 ($1 # $>) }
  | '&'  mut lhs     %prec UNARY     { AddrOf [] Mutable $3 ($1 # $>) }
  | '&&'     lhs     %prec UNARY     { AddrOf [] Immutable (AddrOf [] Immutable $2 (nudge 1 0 ($1 # $2))) ($1 # $2) }
  | '&&' mut lhs     %prec UNARY     { AddrOf [] Immutable (AddrOf [] Mutable $3 (nudge 1 0 ($1 # $3))) ($1 # $3) }
  | box lhs          %prec UNARY     { Box [] $2 ($1 # $>) }
  | lhs ':' ty_no_plus               { TypeAscription [] $1 $3 ($1 # $>) }
  | lhs as ty_no_plus                { Cast [] $1 $3 ($1 # $>) }
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
  |     '..'  rhs2  %prec PREFIXRNG  { Range [] Nothing (Just $2) HalfOpen ($1 # $2) }
  |     '...' rhs2  %prec PREFIXRNG  { Range [] Nothing (Just $2) Closed ($1 # $2) }
  | lhs '..'        %prec POSTFIXRNG { Range [] (Just $1) Nothing HalfOpen ($1 # $>) }
  | lhs '...'       %prec POSTFIXRNG { Range [] (Just $1) Nothing Closed ($1 # $>) }
  | lhs '..'  rhs2  %prec INFIXRNG   { Range [] (Just $1) (Just $3) HalfOpen ($1 # $>) }
  | lhs '...' rhs2  %prec INFIXRNG   { Range [] (Just $1) (Just $3) Closed ($1 # $>) }
  |     '..'        %prec SINGLERNG  { Range [] Nothing Nothing HalfOpen (spanOf $1) }
  |     '...'       %prec SINGLERNG  { Range [] Nothing Nothing Closed (spanOf $1) }

-- Lowest precedence generalized expression
gen_expr :: { Expr Span }
  : return                       { Ret [] Nothing (spanOf $1) }
  | return expr                  { Ret [] (Just $2) ($1 # $2) }
  | continue                     { Continue [] Nothing (spanOf $1) }
  | continue lifetime            { Continue [] (Just $2) ($1 # $2) }
  | break                        { Break [] Nothing Nothing (spanOf $1) }
  | break          expr          { Break [] Nothing (Just $2) ($1 # $2) }
  | break lifetime               { Break [] (Just $2) Nothing ($1 # $2) }
  | break lifetime expr          { Break [] (Just $2) (Just $3) ($1 # $3) }


-- Then, we instantiate these general productions into the following families of rules:
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
-- There is also a later instantiation revolving around 'match' expressions, but it has some
-- different types.

expr :: { Expr Span }
  : gen_expr                                                                  { $1 }
  | lambda_expr                                                               { $1 }
  | arithmetic_expr                                                           { $1 }
arithmetic_expr :: { Expr Span }
  : gen_arithmetic(arithmetic_expr,arithmetic_expr,arithmetic_expr)           { $1 }
  | postfix_expr                                                              { $1 }
postfix_expr  :: { Expr Span }
  : gen_postfix_expr(postfix_expr)                                            { $1 }
  | paren_expr                                                                { $1 }
  | struct_expr                                                               { $1 }
  | block_expr                                                                { $1 }

nostruct_expr :: { Expr Span }
  : gen_expr                                                                  { $1 }
  | ns_arithmetic_expr                                                        { $1 }
  | lambda_expr_nostruct                                                      { $1 }
ns_arithmetic_expr :: { Expr Span }
  : gen_arithmetic(ns_arithmetic_expr,ns_arithmetic_expr,nsb_arithmetic_expr) { $1 }
  | ns_postfix_expr                                                           { $1 }
ns_postfix_expr  :: { Expr Span }
  : gen_postfix_expr(ns_postfix_expr)                                         { $1 }
  | paren_expr                                                                { $1 }
  | block_expr                                                                { $1 }

nostructblock_expr :: { Expr Span }
  : gen_expr                                                                  { $1 }
  | nsb_arithmetic_expr                                                       { $1 }
  | lambda_expr_nostruct                                                      { $1 }
nsb_arithmetic_expr :: { Expr Span }
  : gen_arithmetic(nsb_arithmetic_expr,ns_arithmetic_expr,nsb_arithmetic_expr){ $1 }
  | nsb_postfix_expr                                                          { $1 }
nsb_postfix_expr  :: { Expr Span }
  : gen_postfix_expr(nsb_postfix_expr)                                        { $1 }
  | paren_expr                                                                { $1 }
  | block_like_expr                                                           { $1 }

nonblock_expr :: { Expr Span }
  : gen_expr                                                                  { $1 }
  | nb_arithmetic_expr                                                        { $1 }
  | lambda_expr_nostruct                                                      { $1 }
nb_arithmetic_expr :: { Expr Span }
  : gen_arithmetic(nb_arithmetic_expr,arithmetic_expr,nsb_arithmetic_expr)    { $1 }  -- TODO: 3rd arg should be 'arithmetic_expr'
  | nb_postfix_expr                                                           { $1 }
nb_postfix_expr :: { Expr Span }
  : gen_postfix_expr(nb_postfix_expr)                                         { $1 }
  | paren_expr                                                                { $1 }
  | struct_expr                                                               { $1 }


-- Finally, what remains is the more mundane definitions of particular types of expressions.

-- Literal expressions (composed of just literals)
lit_expr :: { Expr Span }
  : lit       { Lit [] $1 (spanOf $1) }


-- An expression ending in a '{ ... }' block. Useful since "There is a convenience rule that allows
-- one to omit the separating ';' after 'if', 'match', 'loop', 'for', 'while'"
block_expr :: { Expr Span }
  : block_like_expr                                     { $1 }
  | block                                               { BlockExpr [] $1 (spanOf $1) }

-- Any expression ending in a '{ ... }' block except a block itself.
block_like_expr :: { Expr Span }
  : if_expr                                             { $1 }
  |              loop                            block  { Loop [] $2 Nothing ($1 # $>) }
  | lifetime ':' loop                            block  { Loop [] $4 (Just $1) ($1 # $>) }
  |              for pat in nostruct_expr        block  { ForLoop [] $2 $4 $5 Nothing ($1 # $>) }
  | lifetime ':' for pat in nostruct_expr        block  { ForLoop [] $4 $6 $7 (Just $1) ($1 # $>) }
  |              while             nostruct_expr block  { While [] $2 $3 Nothing ($1 # $>) }
  | lifetime ':' while             nostruct_expr block  { While [] $4 $5 (Just $1) ($1 # $>) }
  |              while let pat '=' nostruct_expr block  { WhileLet [] $3 $5 $6 Nothing ($1 # $>) }
  | lifetime ':' while let pat '=' nostruct_expr block  { WhileLet [] $5 $7 $8 (Just $1) ($1 # $>) }
  | match nostruct_expr '{' '}'                         { Match [] $2 [] ($1 # $>) }
  | match nostruct_expr '{' arms '}'                    { Match [] $2 $4 ($1 # $>) }
  | expr_path '!' '{' many(token_tree) '}'              { MacExpr [] (Mac $1 $4 ($1 # $>)) ($1 # $>) }
  | unsafe block                                        { BlockExpr [] $2{ rules = Unsafe } ($1 # $>) }
  | do catch block                                      { Catch [] $3 ($1 # $>) }

-- 'if' expressions are a bit special since they can have an arbitrary number of 'else if' chains.
if_expr :: { Expr Span }
  : if             nostruct_expr block else_expr        { If [] $2 $3 $4 ($1 # $3 # $>) }
  | if let pat '=' nostruct_expr block else_expr        { IfLet [] $3 $5 $6 $7 ($1 # $6 # $>) }

else_expr :: { Maybe (Expr Span) }
  : else block      { Just (BlockExpr [] $2 (spanOf $2)) }
  | else if_expr    { Just $2 }
  | {- empty -}     { Nothing }

-- Match arms usually have to be seperated by commas (with an optional comma at the end). This
-- condition is loosened (so that there is no seperator needed) if the arm ends in a safe block.
arms :: { [Arm Span] }
  : ntArm                                                  { [$1] }
  | ntArm arms                                             { $1 : $2 }
  | many(outer_attribute) sep_by1(pat,'|') arm_guard '=>' expr_arms  { let (e,as) = $> in (Arm $1 $2 $3 e ($1 # $2 # e) : as) }

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
  : gen_expr                          comma_arms              { ($1, $2) }
  | lambda_expr_nostruct              comma_arms              { ($1, $2) }
  | arithmetic_expr_arms                                      { $1 }
arithmetic_expr_arms :: { (Expr Span, [Arm Span]) }
  : gen_arithmetic(nb_arithmetic_expr,arithmetic_expr,nsb_arithmetic_expr) comma_arms  { ($1, $2) }
  | postfix_expr_arms                                         { $1 }
postfix_expr_arms :: { (Expr Span, [Arm Span]) }
  : gen_postfix_expr(nb_postfix_expr) comma_arms              { ($1, $2) }
  | paren_expr                        comma_arms              { ($1, $2) }
  | struct_expr                       comma_arms              { ($1, $2) }
  | block_like_expr                   comma_arms              { ($1, $2) }
  | block                             comma_arms              { (BlockExpr [] $1 (spanOf $1), $2) }
  | block                                   arms              { (BlockExpr [] $1 (spanOf $1), $2) }


-- As per https://github.com/rust-lang/rust/issues/15701 (as of March 10 2017), the only way to have
-- attributes on expressions should be with inner attributes on a paren expression.
paren_expr :: { Expr Span }
  : '(' ')'                             { TupExpr [] [] ($1 # $2) }
  | '(' expr ')'                        { ParenExpr [] $2 ($1 # $3) }
  | '(' inner_attrs expr ')'            { ParenExpr (toList $2) $3 ($1 # $4) }
  | '(' expr ',' ')'                    { TupExpr [] [$2] ($1 # $4) }
  | '(' expr ',' sep_by1T(expr,',') ')' { TupExpr [] ($2 : toList $4) ($1 # $5) }


-- Closure
lambda_expr :: { Expr Span }
  : move args '->' ty_no_plus block
    { Closure [] Value (FnDecl (unspan $2) (Just $4) False (spanOf $2)) (BlockExpr [] $> (spanOf $>)) ($1 # $>) }
  | move args         expr
    { Closure [] Value (FnDecl (unspan $2) Nothing   False (spanOf $2)) $> ($1 # $>) }
  |      args '->' ty_no_plus block
    { Closure [] Ref   (FnDecl (unspan $1) (Just $3) False (spanOf $1)) (BlockExpr [] $> (spanOf $>)) ($1 # $>) }
  |      args         expr
    { Closure [] Ref   (FnDecl (unspan $1) Nothing   False (spanOf $1)) $> ($1 # $>) }

-- Closure expression in a "no struct context"
lambda_expr_nostruct :: { Expr Span }
  : move args nostruct_expr  { Closure [] Value (FnDecl (unspan $2) Nothing False (spanOf $2)) $> ($1 # $3) }
  |      args nostruct_expr  { Closure [] Ref   (FnDecl (unspan $1) Nothing False (spanOf $1)) $> ($1 # $2) }

-- Closure arguments
args :: { Spanned [Arg Span] }
  : '||'                          { Spanned [] (spanOf $1) }
  | '|' sep_byT(arg,',') '|'      { Spanned $2 ($1 # $3) }

arg :: { Arg Span }
  : ntArg                         { $1 }
  | pat ':' ty                    { Arg (Just $1) $3 ($1 # $3) }
  | pat                           { Arg (Just $1) (Infer mempty) (spanOf $1) }


-- Struct expression literal
struct_expr :: { Expr Span }
  : expr_path '{'                        '..' expr '}'  { Struct [] $1 [] (Just $4) ($1 # $>) }
  | expr_path '{' sep_by1(field,',') ',' '..' expr '}'  { Struct [] $1 (toList $3) (Just $6) ($1 # $>) }
  | expr_path '{' sep_byT(field,',')               '}'  { Struct [] $1 $3 Nothing ($1 # $>) }

field :: { Field Span }
  : ident ':' expr  { Field (unspan $1) (Just $3) ($1 # $3) }
  | ident           { Field (unspan $1) Nothing (spanOf $1) }


----------------
-- Statements --
----------------

-- TODO: do something with single semicolons
stmt :: { Stmt Span }
  : ntStmt                                       { $1 }
  | many(outer_attribute) let pat ':' ty initializer ';'   { Local $3 (Just $5) $6 $1 ($1 # $2 # $>) }
  | many(outer_attribute) let pat        initializer ';'   { Local $3 Nothing $4 $1 ($1 # $2 # $>) }
  | many(outer_attribute) nonblock_expr ';'                { toStmt ($1 `addAttrs` $2) True  False ($1 # $2 # $3) }
  | many(outer_attribute) block_expr                       { toStmt ($1 `addAttrs` $2) False True  ($1 # $2) }
  | many(outer_attribute) block_expr ';'                   { toStmt ($1 `addAttrs` $2) True  False ($1 # $2 # $3) }
  | many(outer_attribute)     stmt_item                    { ItemStmt (let Item i a n v s = $2 in Item i ($1 ++ a) n v s) ($1 # $2) }
  | many(outer_attribute) pub stmt_item                    { ItemStmt (let Item i a n _ s = $3 in Item i ($1 ++ a) n PublicV s) ($1 # $2 # $3) }

-- List of statements where the last statement might be a no-semicolon statement.
stmts_possibly_no_semi :: { [Stmt Span] }
  : stmt stmts_possibly_no_semi                  { $1 : $2 }
  | stmt                                         { [$1] }
  | many(outer_attribute) nonblock_expr          { [toStmt ($1 `addAttrs` $2) False False ($1 # $2)] }

initializer :: { Maybe (Expr Span) }
  : '=' expr                                     { Just $2 }
  | {- empty -}                                  { Nothing }


block :: { Block Span }
  : ntBlock                                      { $1 }
  | '{' '}'                                      { Block [] Normal ($1 # $2) }
  | '{' stmts_possibly_no_semi '}'               { Block $2 Normal ($1 # $>) }

inner_attrs_block :: { ([Attribute Span], Block Span) }
  : block                                        { ([], $1) }
  | '{' inner_attrs '}'                          { (toList $2, Block [] Normal ($1 # $>)) }
  | '{' inner_attrs stmts_possibly_no_semi '}'   { (toList $2, Block $3 Normal ($1 # $>)) }


-----------
-- Items --
-----------

item :: { Item Span }
  : ntItem                                             { $1 }
  | stmt_item                                          { $1 }
  | expr_path '!' ident '[' many(token_tree) ']' ';'   { Item (unspan $3) [] (MacItem (Mac $1 $5 ($1 # $>))) InheritedV ($1 # $>) }
  | expr_path '!'       '[' many(token_tree) ']' ';'   { Item "" [] (MacItem (Mac $1 $4 ($1 # $>))) InheritedV ($1 # $>) }
  | expr_path '!' ident '(' many(token_tree) ')' ';'   { Item (unspan $3) [] (MacItem (Mac $1 $5 ($1 # $>))) InheritedV ($1 # $>) }
  | expr_path '!'       '(' many(token_tree) ')' ';'   { Item "" [] (MacItem (Mac $1 $4 ($1 # $>))) InheritedV ($1 # $>) }
  | expr_path '!' ident '{' many(token_tree) '}'       { Item (unspan $3) [] (MacItem (Mac $1 $5 ($1 # $>))) InheritedV ($1 # $>) }
  | expr_path '!'       '{' many(token_tree) '}'       { Item "" [] (MacItem (Mac $1 $4 ($1 # $>))) InheritedV ($1 # $>) }

mod_item :: { Item Span }
  : many(outer_attribute) vis item                     { let Item i a n _ _ = $3 in Item i ($1 ++ a) n (unspan $2) ($1 # $2 # $3) }

foreign_item :: { ForeignItem Span }
  : many(outer_attribute) vis static     ident ':' ty ';'
      { ForeignItem (unspan $4) $1 (ForeignStatic $6 False) (unspan $2) ($1 # $>) }
  | many(outer_attribute) vis static mut ident ':' ty ';'
      { ForeignItem (unspan $5) $1 (ForeignStatic $7 True) (unspan $2) ($1 # $>) }
  | many(outer_attribute) vis fn ident generics fn_decl where_clause ';'
      { ForeignItem (unspan $4) $1 (ForeignFn $6 $5{ whereClause = $7 }) (unspan $2) ($1 # $>) }

-- parse_generics
-- Leaves the WhereClause empty
generics :: { Generics Span }
  : {- empty -}                                                     { Generics [] [] (WhereClause [] mempty) mempty }
  | ntGenerics                                                      { $1 }
  | '<' sep_by1(lifetime_def,',') ',' sep_by1T(ty_param,',') gt '>' { Generics (toList $2) (toList $4) (WhereClause [] mempty) ($1 # $>) }
  | '<' sep_by1T(lifetime_def,',')                           gt '>' { Generics (toList $2) []          (WhereClause [] mempty) ($1 # $>) }
  | '<'                               sep_by1T(ty_param,',') gt '>' { Generics []          (toList $2) (WhereClause [] mempty) ($1 # $>) }
  | '<'                                                      gt '>' { Generics []          []          (WhereClause [] mempty) ($1 # $>) }

-- TODO: Attributes? The AST has them, so the parser should produce them
ty_param :: { TyParam Span }
  : ident                                              { TyParam [] (unspan $1) [] Nothing (spanOf $1) }
  | ident ':' sep_by1T(ty_param_bound_mod,'+')         { TyParam [] (unspan $1) (toList $3) Nothing ($1 # $3) }
  | ident                                      '=' ty  { TyParam [] (unspan $1) [] (Just $>) (spanOf $1) }
  | ident ':' sep_by1T(ty_param_bound_mod,'+') '=' ty  { TyParam [] (unspan $1) (toList $3) (Just $>) ($1 # $>) }

stmt_item :: { Item Span }
  : static     ident ':' ty '=' expr ';'               { Item (unspan $2) [] (Static $4 Immutable $6) InheritedV ($1 # $>) }
  | static mut ident ':' ty '=' expr ';'               { Item (unspan $3) [] (Static $5 Mutable $7) InheritedV ($1 # $>) }
  | const ident ':' ty '=' expr ';'                    { Item (unspan $2) [] (ConstItem $4 $6) InheritedV ($1 # $>) }
  | type ident generics where_clause '=' ty ';'        { Item (unspan $2) [] (TyAlias $6 $3{ whereClause = $4 }) InheritedV ($1 # $>) }
  | use view_path ';'                                  { Item "" [] (Use $2) InheritedV ($1 # $>) }
  | extern crate ident ';'                             { Item (unspan $3) [] (ExternCrate Nothing) InheritedV ($1 # $>) }
  | extern crate ident as ident ';'                    { Item (unspan $5) [] (ExternCrate (Just (unspan $3))) InheritedV ($1 # $>) }
  | const safety   fn ident generics fn_decl where_clause inner_attrs_block
    { Item (unspan $4) (fst $>) (Fn $6 $2 Const Rust $5{ whereClause = $7 } (snd $>)) InheritedV ($1 # snd $>) }
  | unsafe ext_abi fn ident generics fn_decl where_clause inner_attrs_block
    { Item (unspan $4) (fst $>) (Fn $6 Unsafe NotConst $2 $5{ whereClause = $7 } (snd $>)) InheritedV ($1 # snd $>) }
  | extern     abi fn ident generics fn_decl where_clause inner_attrs_block
    { Item (unspan $4) (fst $>) (Fn $6 Normal NotConst $2 $5{ whereClause = $7 } (snd $>)) InheritedV ($1 # snd $>) }
  |                fn ident generics fn_decl where_clause inner_attrs_block
    { Item (unspan $2) (fst $>) (Fn $4 Normal NotConst Rust $3{ whereClause = $5 } (snd $>)) InheritedV ($1 # snd $>) }
  | mod ident ';'                                        { Item (unspan $2) [] (Mod []) InheritedV ($1 # $>) }
  | mod ident '{'             many(mod_item) '}'         { Item (unspan $2) [] (Mod $4) InheritedV ($1 # $>) }
  | mod ident '{' inner_attrs many(mod_item) '}'         { Item (unspan $2) (toList $4) (Mod $5) InheritedV ($1 # $>) }
  | extern abi '{'             many(foreign_item) '}'    { Item "" [] (ForeignMod $2 $4) InheritedV ($1 # $>) }
  | extern abi '{' inner_attrs many(foreign_item) '}'    { Item "" (toList $4) (ForeignMod $2 $5) InheritedV ($1 # $>) }
  | struct ident generics struct_decl_args               { Item (unspan $2) [] (StructItem (snd $4) $3{ whereClause = fst $4 }) InheritedV ($1 # snd $>) }
  | union ident generics struct_decl_args                { Item (unspan $2) [] (Union (snd $4) $3{ whereClause = fst $4 }) InheritedV ($1 # snd $>) }
  | enum ident generics where_clause '{' sep_byT(enum_def,',') '}'
    { Item (unspan $2) [] (Enum $6 $3{ whereClause = $4 }) InheritedV ($1 # $>) }
  | item_impl                                            { $1 }
  | item_trait                                           { $1 }

item_trait :: { Item Span }
  : safety_trait ident generics ':' sep_by1T(ty_param_bound,'+') where_clause '{' many(trait_item) '}'
     { Item (unspan $2) [] (Trait (unspan $1) $3{ whereClause = $6 } (toList $5) $8) InheritedV ($1 # $>) }
  | safety_trait ident generics where_clause '{' many(trait_item) '}'
     { Item (unspan $2) [] (Trait (unspan $1) $3{ whereClause = $4 } [] $6) InheritedV ($1 # $>) }

safety_trait :: { Spanned Unsafety }
  :        trait   { Spanned Normal (spanOf $1) }
  | unsafe trait   { Spanned Unsafe ($1 # $2) }

struct_decl_args :: { (WhereClause Span, VariantData Span) }
  : where_clause ';'                                         { ($1, UnitD ($1 # $>)) }
  | where_clause '{' sep_byT(struct_decl_field,',') '}'      { ($1, StructD $3 ($1 # $>)) }
  | '(' sep_byT(tuple_decl_field,',') ')' where_clause ';'   { ($4, TupleD $2 ($1 # $>)) }

struct_decl_field :: { StructField Span }
  : many(outer_attribute) vis ident ':' ty                  { StructField (Just (unspan $3)) (unspan $2) $5 $1 ($1 # $2 # $5) }

tuple_decl_field :: { StructField Span }
  : many(outer_attribute) vis ty                       { StructField Nothing (unspan $2) $3 $1 ($1 # $2 # $3) }

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
  | no_for_ty                                              { BoundPredicate [] $1 [] (spanOf $1) }
  | no_for_ty '=' ty                                       { EqPredicate $1 $3 ($1 # $3) }
  | no_for_ty ':' sep_by1T(ty_param_bound_mod,'+')         { BoundPredicate [] $1 (toList $3) ($1 # $3) }
  | for_lts no_for_ty                                      { BoundPredicate (unspan $1) $2 [] ($1 # $2) }
  | for_lts no_for_ty ':' sep_by1T(ty_param_bound_mod,'+') { BoundPredicate (unspan $1) $2 (toList $4) ($1 # $>) }


item_impl :: { Item Span }
  : safety_impl generics ty_prim              where_clause '{' impl_items '}'
    { Item (mkIdent "") (fst $6) (Impl (unspan $1) Positive $2{ whereClause = $4 } Nothing $3 (snd $6)) InheritedV ($1 # $>) }
  | safety_impl generics '(' ty_no_plus ')'   where_clause '{' impl_items '}'
    { Item (mkIdent "") (fst $8) (Impl (unspan $1) Positive $2{ whereClause = $6 } Nothing (ParenTy $4 ($3 # $5)) (snd $8)) InheritedV ($1 # $>) }
  | safety_impl generics '!' trait_ref for ty where_clause '{' impl_items '}'
    { Item (mkIdent "") (fst $9) (Impl (unspan $1) Negative $2{ whereClause = $7 } (Just $4) $6 (snd $9)) InheritedV ($1 # $>) }
  | safety_impl generics     trait_ref for ty where_clause '{' impl_items '}'
    { Item (mkIdent "") (fst $8) (Impl (unspan $1) Positive $2{ whereClause = $6 } (Just $3) $5 (snd $8)) InheritedV ($1 # $>) }
  | safety_impl generics     trait_ref for '..'            '{'            '}'
    { Item (mkIdent "") [] (case $2 of { Generics [] [] _ _ -> (DefaultImpl (unspan $1) $3); _ -> error "todo" }) InheritedV ($1 # $>) }

safety_impl :: { Spanned Unsafety }
  :        impl   { Spanned Normal (spanOf $1) }
  | unsafe impl   { Spanned Unsafe ($1 # $2) }

impl_items :: { ([Attribute Span], [ImplItem Span]) }
  :             many(impl_item)  { ([], $1) }
  | inner_attrs many(impl_item)  { (toList $1, $2) }

impl_item :: { ImplItem Span }
  : ntImplItem                                          { $1 }
  | many(outer_attribute) vis def type ident '=' ty ';'           { ImplItem (unspan $5) (unspan $2) $3 $1 (TypeI $7) ($1 # $2 # $>) }
  | many(outer_attribute) vis def const ident ':' ty '=' expr ';' { ImplItem (unspan $5) (unspan $2) $3 $1 (ConstI $7 $9) ($1 # $2 # $>) }
  | many(outer_attribute) vis def mod_mac                         { ImplItem (mkIdent "") (unspan $2) $3 $1 (MacroI $4) ($1 # $2 # $>) }
  | many(outer_attribute) vis def const safety fn ident generics fn_decl_with_self where_clause inner_attrs_block
     { ImplItem (unspan $7) (unspan $2) $3 ($1 ++ fst $>) (MethodI (MethodSig $5 Const Rust $9 $8{ whereClause = $10 }) (snd $>)) ($1 # $2 # snd $>) }
  | many(outer_attribute) vis def safety ext_abi fn ident generics fn_decl_with_self where_clause inner_attrs_block
     { ImplItem (unspan $7) (unspan $2) $3 ($1 ++ fst $>) (MethodI (MethodSig $4 NotConst $5 $9 $8{ whereClause = $10 }) (snd $>)) ($1 # $2 # snd $>) }

-- TODO: the span on the last two cases won't cover the 'safety' or 'abi' if there are no outer_attributes
trait_item :: { TraitItem Span }
  : ntTraitItem                                    { $1 }
  | many(outer_attribute) const ident ':' ty initializer ';' { TraitItem (unspan $3) $1 (ConstT $5 $6) ($1 # $2 # $>) }
  | many(outer_attribute) mod_mac                            { TraitItem (mkIdent "") $1 (MacroT $2) ($1 # $>) }
  | many(outer_attribute) type ty_param ';'                  { let TyParam _ i b d _ = $3 in TraitItem i $1 (TypeT b d) ($1 # $2 # $>) }
  | many(outer_attribute) safety ext_abi fn ident generics fn_decl_with_self where_clause ';'
     { TraitItem (unspan $5) $1 (MethodT (MethodSig $2 NotConst $3 $7 $6{ whereClause = $8 }) Nothing) ($1 # $4 # $>)  }
  | many(outer_attribute) safety ext_abi fn ident generics fn_decl_with_self where_clause block
     { TraitItem (unspan $5) $1 (MethodT (MethodSig $2 NotConst $3 $7 $6{ whereClause = $8 }) (Just $>)) ($1 # $4 # $>) }


safety :: { Unsafety }
  : {- empty -}     { Normal }
  | unsafe          { Unsafe }

ext_abi :: { Abi }
  : {- empty -}     { Rust }
  | extern abi      { $2 }

vis :: { Spanned (Visibility Span) }
  : {- empty -}          { Spanned InheritedV mempty }
  | pub        %prec VIS { Spanned PublicV (spanOf $1) }
  | pub '(' crate ')'    { Spanned CrateV ($1 # $4) }
  | pub '(' mod_path ')' { Spanned (RestrictedV $3) ($1 # $4) }

def :: { Defaultness }
  : {- empty -}      %prec mut { Final }
  | default                    { Default }

view_path :: { ViewPath Span }
  : '::' sep_by1(self_or_ident,'::')                                 { let n = fmap unspan $2 in ViewPathSimple True (N.init n) (PathListItem (N.last n) Nothing mempty) ($1 # $>) }
  | '::' sep_by1(self_or_ident,'::') as ident                        { let n = fmap unspan $2 in ViewPathSimple True (N.init n) (PathListItem (N.last n) (Just (unspan $>)) mempty) ($1 # $>) }
  | '::'                                  '*'                        { ViewPathGlob True [] ($1 # $2) }
  | '::' sep_by1(self_or_ident,'::') '::' '*'                        { ViewPathGlob True (fmap unspan (toList $2)) ($1 # $>) }
  | '::' sep_by1(self_or_ident,'::') '::' '{' sep_byT(plist,',') '}' { ViewPathList True (map unspan (toList $2)) $5 ($1 # $>) }
  | '::'                                  '{' sep_byT(plist,',') '}' { ViewPathList True [] $3 ($1 # $>) }
  |      sep_by1(self_or_ident,'::')                                 { let n = fmap unspan $1 in ViewPathSimple False (N.init n) (PathListItem (N.last n) Nothing mempty) ($1 # $>) }
  |      sep_by1(self_or_ident,'::') as ident                        { let n = fmap unspan $1 in ViewPathSimple False (N.init n) (PathListItem (N.last n) (Just (unspan $>)) mempty) ($1 # $>) }
  |                                       '*'                        { ViewPathGlob False [] (spanOf $1) }
  |      sep_by1(self_or_ident,'::') '::' '*'                        { ViewPathGlob False (fmap unspan (toList $1)) ($1 # $>) }
  |      sep_by1(self_or_ident,'::') '::' '{' sep_byT(plist,',') '}' { ViewPathList False (map unspan (toList $1)) $4 ($1 # $>) }
  |                                       '{' sep_byT(plist,',') '}' { ViewPathList False [] $2 ($1 # $>) }


self_or_ident :: { Spanned Ident }
  : ident                   { $1 }
  | self                    { Spanned "self" (spanOf $1) }
  | super                   { Spanned "super" (spanOf $1) }


plist :: { PathListItem Span }
  : self_or_ident           { PathListItem (unspan $1) Nothing (spanOf $1) }
  | self_or_ident as ident  { PathListItem (unspan $1) (Just (unspan $3)) (spanOf $1) }


-------------------
-- Macro related --
-------------------

expr_mac :: { Mac Span }
  : expr_path '!' '[' many(token_tree) ']'     { Mac $1 $4 ($1 # $>) }
  | expr_path '!' '(' many(token_tree) ')'     { Mac $1 $4 ($1 # $>) }

ty_mac :: { Mac Span }
  : ty_path '!' '[' many(token_tree) ']'       { Mac $1 $4 ($1 # $>) }
  | ty_path '!' '{' many(token_tree) '}'       { Mac $1 $4 ($1 # $>) }
  | ty_path '!' '(' many(token_tree) ')'       { Mac $1 $4 ($1 # $>) }

mod_mac :: { Mac Span }
  : mod_path '!' '[' many(token_tree) ']' ';'  { Mac $1 $4 ($1 # $>) }
  | mod_path '!' '{' many(token_tree) '}'      { Mac $1 $4 ($1 # $>) }
  | mod_path '!' '(' many(token_tree) ')' ';'  { Mac $1 $4 ($1 # $>) }

token_tree :: { TokenTree }
  : ntTT                                                  { $1 }
  -- # Delimited
  | '(' many(token_tree) ')'                              { Delimited mempty Paren mempty $2 mempty }
  | '{' many(token_tree) '}'                              { Delimited mempty Brace mempty $2 mempty }
  | '[' many(token_tree) ']'                              { Delimited mempty Bracket mempty $2 mempty } 
  -- # Token
  -- Expression-operator symbols.
  | token                                                 { let Spanned t s = $1 in Token s t }

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
  | '$' %prec DOLLAR  { $1 }
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
  | yield      { $1 }
  -- Weak keywords, have special meaning only in specific contexts.
  | default    { $1 }
  | union      { $1 }
  -- Comments
  | outerDoc   { $1 }
  | innerDoc   { $1 }
  -- Identifiers.
  | IDENT      { $1 }
  | '_'        { $1 }
  -- Lifetimes.
  | LIFETIME   { $1 }


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


-- | Convert an 'IdentTok' into an 'Ident'
toIdent :: Spanned Token -> Spanned Ident
toIdent (Spanned (IdentTok i) s) = Spanned i s

-- | Try to convert an expression to a statement given information about whether there is a trailing
-- semicolon
toStmt :: Expr Span -> Bool -> Bool -> Span -> Stmt Span
toStmt (MacExpr a m s) hasSemi isBlock | hasSemi = MacStmt m SemicolonMac a
                                       | isBlock = MacStmt m BracesMac a
toStmt e hasSemi _ = (if hasSemi then Semi else NoSemi) e

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
addAttrs as (Closure as' c f e s)    = Closure (as ++ as') c f e s
addAttrs as (BlockExpr as' b s)      = BlockExpr (as ++ as') b s
addAttrs as (Assign as' e1 e2 s)     = Assign (as ++ as') e1 e2 s
addAttrs as (AssignOp as' b e1 e2 s) = AssignOp (as ++ as') b e1 e2 s
addAttrs as (FieldAccess as' e i s)  = FieldAccess (as ++ as') e i s
addAttrs as (TupField as' e i s)     = TupField (as ++ as') e i s
addAttrs as (Index as' e1 e2 s)      = Index (as ++ as') e1 e2 s
addAttrs as (Range as' e1 e2 r s)    = Range (as ++ as') e1 e2 r s
addAttrs as (PathExpr as' q p s)     = PathExpr (as ++ as') q p s
addAttrs as (AddrOf as' m e s)       = AddrOf (as ++ as') m e s
addAttrs as (Break as' l e s)          = Break (as ++ as') l e s
addAttrs as (Continue as' l s)       = Continue (as ++ as') l s
addAttrs as (Ret as' e s)            = Ret (as ++ as') e s
addAttrs as (InlineAsmExpr as' a s)  = InlineAsmExpr (as ++ as') a s
addAttrs as (MacExpr as' m s)        = MacExpr (as ++ as') m s
addAttrs as (Struct as' p f e a)     = Struct (as ++ as') p f e a
addAttrs as (Repeat as' e1 e2 s)     = Repeat (as ++ as') e1 e2 s
addAttrs as (ParenExpr as' e s)      = ParenExpr (as ++ as') e s
addAttrs as (Try as' e s)            = Try (as ++ as') e s


-- | Given a 'Doc' token, convert it into an attribute
mkDocAttribute :: Spanned Token -> Attribute Span
mkDocAttribute (Spanned (Doc docStr sty) s) = Attribute sty' doc True s
  where
    str = Str docStr Cooked Unsuffixed mempty
    doc = NameValue "doc" str mempty
    sty' = case sty of
             OuterDoc -> Outer
             InnerDoc -> Inner

-- | Given a 'LitTok' token that is expected to result in a valid literal, construct the associated
-- literal. Note that this should _never_ fail on a token produced by the lexer.
lit :: Spanned Token -> Lit Span
lit (Spanned (IdentTok (Ident "true" _)) s) = Bool True Unsuffixed s
lit (Spanned (IdentTok (Ident "false" _)) s) = Bool False Unsuffixed s
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
               _ -> error "lit"

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
