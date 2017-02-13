{
module Language.Rust.Parser.Parser (
  attributeP, typeP, literalP, patternP, expressionP
) where

import Language.Rust.Data.InputStream
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
import Language.Rust.Data.Located
import Language.Rust.Parser.Lexer
import Language.Rust.Parser.ParseMonad
import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Constants

import Data.Semigroup ((<>))
import Data.Maybe
import Data.List.NonEmpty hiding (length, init, last, reverse)
import qualified Data.List.NonEmpty as N
}

-- <https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y>
-- <https://github.com/rust-lang/rust/blob/master/src/libsyntax/parse/parser.rs>
-- References to <https://doc.rust-lang.org/grammar.html>
-- To see conflicts: stack exec happy -- --info=happyinfo.txt -o /dev/null src/Language/Rust/Parser/Parser.y

-- in order to document the parsers, we have to alias them
%name literalP lit
%name attributeP attribute
%name typeP ty_sum  -- the exported parser for types really is for type sums
%name patternP pat
%name stmtP stmt
%name expressionP expr

%tokentype { TokenSpace Spanned }

%monad { P } { >>= } { return }
%error { parseError }
%lexer { lexRust } { Tok (Spanned Eof _) }

-- Conflicts caused in
--  * sep_by1(segment,'::') parts of paths
--  * complex_expr_path
--  * & mut patterns
-- However, they are all S/R and seem to be currently doing what they should
%expect 3

%token

  -- Expression-operator symbols. 
  EQ         { NoSpTok $$@(Spanned Equal _) }
  LT         { NoSpTok $$@(Spanned Less _) }
  GT         { NoSpTok $$@(Spanned Greater _) }
  NOT        { NoSpTok $$@(Spanned Exclamation _) }
  TILDE      { NoSpTok $$@(Spanned Tilde _) }
  
  PLUS       { NoSpTok $$@(Spanned Plus _) }
  MINUS      { NoSpTok $$@(Spanned Minus _) }
  STAR       { NoSpTok $$@(Spanned Star _) }
  SLASH      { NoSpTok $$@(Spanned Slash _) }
  PERCENT    { NoSpTok $$@(Spanned Percent _) }
  CARET      { NoSpTok $$@(Spanned Caret _) }
  AMPERSAND  { NoSpTok $$@(Spanned Ampersand _) }
  PIPE       { NoSpTok $$@(Spanned Pipe _) }

  EQ_S       { Tok $$@(Spanned Equal _) }
  LT_S       { Tok $$@(Spanned Less _) }
  GT_S       { Tok $$@(Spanned Greater _) }
  NOT_S      { Tok $$@(Spanned Exclamation _) }
  TILDE_S    { Tok $$@(Spanned Tilde _) }

  PLUS_S     { Tok $$@(Spanned Plus _) }
  MINUS_S    { Tok $$@(Spanned Minus _) }
  STAR_S     { Tok $$@(Spanned Star _) }
  SLASH_S    { Tok $$@(Spanned Slash _) }
  PERCENT_S  { Tok $$@(Spanned Percent _) }
  CARET_S    { Tok $$@(Spanned Caret _) }
  AMPERSAND_S{ Tok $$@(Spanned Ampersand _) }
  PIPE_S     { Tok $$@(Spanned Pipe _) }

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
  Self       { Tok $$@(Identifier "Self") }
  self       { Tok $$@(Identifier "self") } 
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
  union      { Tok $$@(Identifier "union") } 

  -- Comments
  outerDoc   { Tok $$@(Spanned (Doc _ OuterDoc) _) }
  innerDoc   { Tok $$@(Spanned (Doc _ InnerDoc) _) }

  -- Identifiers.
  IDENT      { Tok $$@(Identifier _) }
  '_'        { Tok $$@(Spanned Underscore _) }

  -- Lifetimes.
  LIFETIME   { Tok $$@(Spanned (LifetimeTok _) _) }

  -- Interpolated
  ntItem         { Tok $$@(Spanned (Interpolated (NtItem _)) _) }
  ntBlock        { Tok $$@(Spanned (Interpolated (NtBlock _)) _) }
  ntStmt         { Tok $$@(Spanned (Interpolated (NtStmt _)) _) }
  ntPat          { Tok $$@(Spanned (Interpolated (NtPat _)) _) }
  ntExpr         { Tok $$@(Spanned (Interpolated (NtExpr _)) _) }
  ntTy           { Tok $$@(Spanned (Interpolated (NtTy _)) _) }
  ntIdent        { Tok $$@(Spanned (Interpolated (NtIdent _)) _) }
  ntMeta         { Tok $$@(Spanned (Interpolated (NtMeta _)) _) }
  ntPath         { Tok $$@(Spanned (Interpolated (NtPath _)) _) }
  ntTT           { Tok $$@(Spanned (Interpolated (NtTT _)) _) }
  ntArm          { Tok $$@(Spanned (Interpolated (NtArm _)) _) }
  ntImplItem     { Tok $$@(Spanned (Interpolated (NtImplItem _)) _) }
  ntTraitItem    { Tok $$@(Spanned (Interpolated (NtTraitItem _)) _) }
  ntGenerics     { Tok $$@(Spanned (Interpolated (NtGenerics _)) _) }
  ntWhereClause  { Tok $$@(Spanned (Interpolated (NtWhereClause _)) _) }
  ntArg          { Tok $$@(Spanned (Interpolated (NtArg _)) _) }

%%


---------------------
-- Extended tokens --
---------------------

-- These tokens have both space and no space versions
'=' : alt(EQ,EQ_S)                { $1 }
'<' : alt(LT,LT_S)                { $1 }
'>' : alt(GT,GT_S)                { $1 }
'!' : alt(NOT,NOT_S)              { $1 }
'~' : alt(TILDE,TILDE_S)          { $1 }

'+' : alt(PLUS, PLUS_S)           { $1 }
'-' : alt(MINUS, MINUS_S)         { $1 }
'*' : alt(STAR, STAR_S)           { $1 }
'/' : alt(SLASH, SLASH_S)         { $1 }
'%' : alt(PERCENT, PERCENT_S)     { $1 }
'^' : alt(CARET, CARET_S)         { $1 }
'&' : alt(AMPERSAND, AMPERSAND_S) { $1 }
'|' : alt(PIPE, PIPE_S)           { $1 }

-- All of these have type 'Spanned ()'
'<<=' : '<' LT EQ      { () <\$ $1 <* $3 }
'>>=' : '>' GT EQ      { () <\$ $1 <* $3 }
'-='  : '-' EQ         { () <\$ $1 <* $2 }
'&='  : '&' EQ         { () <\$ $1 <* $2 }
'|='  : '|' EQ         { () <\$ $1 <* $2 }
'+='  : '+' EQ         { () <\$ $1 <* $2 }
'*='  : '*' EQ         { () <\$ $1 <* $2 }
'/='  : '/' EQ         { () <\$ $1 <* $2 }
'^='  : '^' EQ         { () <\$ $1 <* $2 }
'%='  : '%' EQ         { () <\$ $1 <* $2 }
'||'  : '|' PIPE       { () <\$ $1 <* $2 }
'&&'  : '&' AMPERSAND  { () <\$ $1 <* $2 }
'=='  : '=' EQ         { () <\$ $1 <* $2 }
'!='  : '!' EQ         { () <\$ $1 <* $2 }
'<='  : '<' EQ         { () <\$ $1 <* $2 }
'>='  : '>' EQ         { () <\$ $1 <* $2 }
'<<'  : '<' LT         { () <\$ $1 <* $2 }
'>>'  : '>' GT         { () <\$ $1 <* $2 }

-- Unwraps the IdentTok into just an Ident
ident :: { Spanned Ident }
ident : IDENT                         { let Spanned (IdentTok i) s = $1 in Spanned i s }


-------------
-- Utility --
-------------

-- | One or more
-- some :: P a -> P (NonEmpty a)
some(p)         : some_r(p)          { N.reverse $1 }
some_r(p)       : some_r(p) p        { $2 <| $1 }
                | p                  { $1 :| [] }

-- | Zero or more
-- many :: P a -> P [a]
many(p)         : some(p)            { toList $1 }
                | {- empty -}        { [] } 


-- | One or more occurences of p, seperated by sep
-- sep_by1(p,sep) :: P a -> P b -> P (NonEmpty a)
-- TODO: Use the commented out implementation (and understand why it currently makes more conlifcts)
{-
sep_by1(p,sep)  : sep_by1_r(p,sep)   { N.reverse $1 }
sep_by1_r(p,s)  : sep_by1_r(p,s) s p { $3 <| $1 }
                | p                  { $1 :| [] }
-}
sep_by1(p,sep)  : sep_by1(p,sep) sep p  { $1 |> $3 }
                | p                     { $1 :| [] }


-- | Zero or more occurrences of p, separated by sep
-- sep_by :: P a -> P b -> P [a]
sep_by(p,sep)   : sep_by1(p,sep)     { toList $1 }
                | {- empty -}        { [] }

-- | One or the other, but of the same type
-- alt(l,r) :: P a -> P a -> P a
alt(l,r)        : l                  { $1 }
                | r                  { $1 }


--------------------------
-- Attributes
--------------------------

attribute :: { Attribute Span }
  : inner_attribute { $1 }
  | outer_attribute { $1 }

outer_attribute :: { Attribute Span }
  : '#' '[' meta_item ']'        {% withSpan $1 (Attribute Outer $3 False) }
  | outerDoc                     {% let Doc docStr OuterDoc = unspan $1 in
                                    do { str <- withSpan $1 (Str docStr Cooked Unsuffixed)
                                       ; doc <- withSpan $1 (NameValue (mkIdent "doc") str)
                                       ; withSpan $1 (Attribute Outer doc True)
                                       }
                                 }

inner_attribute :: { Attribute Span }
  : '#' '!' '[' meta_item ']'    {% withSpan $1 (Attribute Inner $4 False) } 
  | innerDoc                     {% let Doc docStr InnerDoc = unspan $1 in
                                    do { str <- withSpan $1 (Str docStr Cooked Unsuffixed)
                                       ; doc <- withSpan $1 (NameValue (mkIdent "doc") str)
                                       ; withSpan $1 (Attribute Inner doc True)
                                    }
                                 }

-- parse_meta_item()
meta_item :: { MetaItem Span }
  : ident                                           {% withSpan $1 (Word (unspan $1)) }
  | ident '=' unsuffixed                            {% withSpan $1 (NameValue (unspan $1) $3) }
  | ident '(' sep_by(meta_item_inner,',') ')'       {% withSpan $1 (List (unspan $1) $3) }
  | ident '(' sep_by1(meta_item_inner,',') ',' ')'  {% withSpan $1 (List (unspan $1) (toList $3)) }

-- parse_meta_item_inner()
meta_item_inner :: { NestedMetaItem Span }
  : unsuffixed                                      {% withSpan $1 (Literal $1) }
  | meta_item                                       {% withSpan $1 (MetaItem $1) } 


--------------
-- Literals --
--------------

-- TODO Interpolated (see parse_lit_token())
lit :: { Lit Span }
  : byte              { lit $1 }
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
  : lit {% case suffix $1 of { Unsuffixed -> pure $1; _ -> fail "expected unsuffixed literal" } }


-----------
-- Paths --
-----------

-- parse_qualified_path(PathStyle::Type)
-- qual_path :: Spanned (NonEmpty (Ident, PathParameters Span)) -> P (Spanned (QSelf Span, Path Span))
qual_path(segs)
  : '<' ty_sum '>' '::' segs            {% withSpan $1 (Spanned (QSelf $2 0, Path False (unspan $5) (posOf $5))) }
  | '<' ty_sum as ty_path '>' '::' segs {% let segs = segments $4 <> unspan $7
                                           in withSpan $1 (Spanned (QSelf $2 (length (segments $4)), $4{ segments = segs })) }

-- parse_generic_values_after_lt()
generic_values :: { ([Lifetime Span], [Ty Span], [(Ident, Ty Span)]) }
generic_values
  : sep_by1(lifetime,',') ',' sep_by1(ty_sum,',') ',' sep_by1(binding,',')  { (toList $1, toList $3, toList $5) }
  | sep_by1(lifetime,',') ',' sep_by1(ty_sum,',')                           { (toList $1, toList $3, []) }
  | sep_by1(lifetime,',') ','                         sep_by1(binding,',')  { (toList $1, [],        toList $3) }
  | sep_by1(lifetime,',')                                                   { (toList $1, [],        []) }
  |                           sep_by1(ty_sum,',') ',' sep_by1(binding,',')  { ([],        toList $1, toList $3) }
  |                           sep_by1(ty_sum,',')                           { ([],        toList $1, []) }
  |                                                   sep_by1(binding,',')  { ([],        [],        toList $1) }
  |                                                                         { ([],        [],        []) }

binding : ident '=' ty                             { (unspan $1, $3) }


-- Type related:
-- parse_path(PathStyle::Type)
ty_path :: { Path Span }
  : path_segments_without_colons            {% withSpan $1 (Path False (unspan $1)) }
  | '::' path_segments_without_colons       {% withSpan $1 (Path True (unspan $2)) }

ty_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(path_segments_without_colons)  { $1 }

-- parse_path_segments_without_colons()
path_segments_without_colons :: { Spanned (NonEmpty (Ident, PathParameters Span)) }
  : sep_by1(path_segment_without_colons, '::')  { sequence $1 }

-- No corresponding function - see path_segments_without_colons
path_segment_without_colons :: { Spanned (Ident, PathParameters Span) }
  : ident path_parameter1 
     {% if isTypePathSegmentIdent $1
          then withSpan $1 (Spanned (unspan $1, $2))
          else fail "invalid path segment in type path"
     }

path_parameter1 :: { PathParameters Span }
  : '<' generic_values '>'                    {% let (lts, tys, bds) = $2 in withSpan $1 (AngleBracketed lts tys bds) }
  | '(' sep_by(ty_sum,',') ')'                {% withSpan $1 (Parenthesized $2 Nothing) }
  | '(' sep_by1(ty_sum,',') ',' ')'           {% withSpan $1 (Parenthesized (toList $2) Nothing) }
  | '(' sep_by(ty_sum,',') ')' '->' ty        {% withSpan $1 (Parenthesized $2 (Just $>)) }
  | '(' sep_by1(ty_sum,',') ',' ')' '->' ty   {% withSpan $1 (Parenthesized (toList $2) (Just $>)) }
  | {- empty -}                               { AngleBracketed [] [] [] mempty }


-- Expression related:
-- parse_path(PathStyle::Expr)
expr_path :: { Path Span }
  : path_segments_with_colons               {% withSpan $1 (Path False (unspan $1)) }
  | '::' path_segments_with_colons          {% withSpan $1 (Path True (unspan $2)) }

-- As expr_path, but disallowing one IDENT paths
-- TODO: this duplicates a bunch of stuff
-- TODO: abstract function to make path out into code block at bottom of file
complex_expr_path :: { Path Span }
  : ident '::' '<' generic_values '>'                                       
      {% if isPathSegmentIdent $1
           then let (lts, tys, bds) = $4
                in withSpan $1 (Path False (fromList [(unspan $1, AngleBracketed lts tys bds mempty)]))
           else fail "invalid path segment in expression path" }
  | ident '::' path_segments_with_colons                              
      {% withSpan $1 (Path False ((unspan $1, NoParameters mempty) <| unspan $3)) }
  | ident '::' '<' generic_values '>' '::' path_segments_with_colons
      {% let (lts, tys, bds) = $4 in withSpan $1 (Path False ((unspan $1, AngleBracketed lts tys bds mempty) <| unspan $7)) }
  | '::' path_segments_with_colons                                          {% withSpan $1 (Path True (unspan $2)) }

expr_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(path_segments_with_colons)  { $1 }

-- parse_path_segments_with_colons()
path_segments_with_colons :: { Spanned (NonEmpty (Ident, PathParameters Span)) }
  : ident                                          {% withSpan $1 (Spanned ((unspan $1, NoParameters mempty) :| [])) }
  | path_segments_with_colons '::' path_parameter2 {%
     case (N.last (unspan $1), $3) of
       ((i, NoParameters{}), Left (lts, tys, bds)) -> withSpan $1 (Spanned (N.init (unspan $1) |: (i, AngleBracketed lts tys bds mempty)))
       (_, Right i) -> withSpan $1 (Spanned (unspan $1 |> (i, AngleBracketed [] [] [] mempty)))
       _ -> error "invalid path segment in expression path"
    }
  
path_parameter2 :: { Either ([Lifetime Span], [Ty Span], [(Ident, Ty Span)]) Ident }
  : '<' generic_values '>' { Left $2 }
  | ident                  { Right (unspan $1) }


-- Mod related:
-- parse_path(PathStyle::Mod)
mod_path :: { Path Span }
  : path_segments_without_types             {% withSpan $1 (Path False (unspan $1)) }
  | '::' path_segments_without_types        {% withSpan $1 (Path True (unspan $2)) }

mod_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(path_segments_without_types)  { $1 }

-- parse_path_segments_without_types()
path_segments_without_types :: { Spanned (NonEmpty (Ident, PathParameters Span)) }
  : sep_by1(path_segment_without_types, '::')  { sequence $1 }

-- No corresponding function - see path_segments_without_types
path_segment_without_types :: { Spanned (Ident, PathParameters Span) }
  : ident 
     {% if isPathSegmentIdent $1
          then withSpan $1 (Spanned (unspan $1, AngleBracketed [] [] [] mempty))
          else fail "invalid path segment in mod path"
     }

-----------
-- Types --
-----------

lifetime :: { Lifetime Span }
  : LIFETIME                         { let Spanned (LifetimeTok (Ident l _)) s = $1 in Lifetime l s }

-- parse_trait_ref()
trait_ref :: { TraitRef Span }
  : ty_path                          {% withSpan $1 (TraitRef $1) }

-- parse_ty()
ty :: { Ty Span }
  : '_'                              {% withSpan $1 Infer }
  | '!'                              {% withSpan $1 Never }
  | '(' ')'                          {% withSpan $1 (TupTy []) }
  | '(' ty_sum ')'                   {% withSpan $1 (ParenTy $2) }
  | '(' ty ',' ')'                   {% withSpan $1 (TupTy [$2]) }
  | '(' ty ',' sep_by1(ty,',') ')'   {% withSpan $1 (TupTy ($2 : toList $4)) }
  | '[' ty ']'                       {% withSpan $1 (Slice $2) }
  | '*' ty                           {% withSpan $1 (Ptr Immutable $2) }
  | '*' const ty                     {% withSpan $1 (Ptr Immutable $3) }
  | '*' mut ty                       {% withSpan $1 (Ptr Mutable $3) }
  | '&' ty                           {% withSpan $1 (Rptr Nothing Immutable $2) }
  | '&' mut ty                       {% withSpan $1 (Rptr Nothing Mutable $3) }
  | '&' lifetime ty                  {% withSpan $1 (Rptr (Just $2) Immutable $3) }
  | '&' lifetime mut ty              {% withSpan $1 (Rptr (Just $2) Mutable $4) }
  | ty_path                          {% withSpan $1 (PathTy Nothing $1) }
  | ty_qual_path                     {% withSpan $1 (PathTy (Just (fst (unspan $1))) (snd (unspan $1))) }
  | unsafe extern abi fn_decl        {% withSpan $1 (BareFn Unsafe $3 [] $4) }
  | unsafe fn_decl                   {% withSpan $1 (BareFn Unsafe Rust [] $2) }
  | extern abi fn_decl               {% withSpan $1 (BareFn Normal $2 [] $3) }
  | fn_decl                          {% withSpan $1 (BareFn Normal Rust [] $1) }
  | typeof '(' expr ')'              {% withSpan $1 (Typeof $3) }
  | '[' ty ';' expr ']'              {% withSpan $1 (Array $2 $4) }
  | for_lts unsafe extern abi fn_decl {% withSpan $1 (BareFn Unsafe $4 (unspan $1) $5) }
  | for_lts unsafe fn_decl           {% withSpan $1 (BareFn Unsafe Rust (unspan $1) $3) }
  | for_lts extern abi fn_decl       {% withSpan $1 (BareFn Normal $3 (unspan $1) $4) }
  | for_lts fn_decl                  {% withSpan $1 (BareFn Normal Rust (unspan $1) $2) }
  | for_lts trait_ref                {% 
      do poly <- withSpan $1 (PolyTraitRef (unspan $1) $2)
         withSpan $1 (PolyTraitRefTy (TraitTyParamBound poly None :| []))
    }
{-
  -- The following is still at RFC stage:
  -- https://github.com/rust-lang/rfcs/blob/master/text/1522-conservative-impl-trait.md
  -- https://github.com/rust-lang/rust/issues/34511
  -- For now, this is only allowed in return type position. If that remains (there is some hope that
  -- it will be more general), it would makes sense to move this as a special case of `ret_ty`
  | impl sep_by1(ty_param_bound_mod,'+')         {%
      if (any isTraitTyParamBound $2)
        then withSpan $1 (ImplTrait $2)
        else fail "at least one trait must be specified"
    }
 -}

-- parse_ty_sum()
-- See https://github.com/rust-lang/rfcs/blob/master/text/0438-precedence-of-plus.md
ty_sum :: { Ty Span }
  : ty                                   { $1 }
  | ty '+' sep_by1(ty_param_bound,'+')   {% withSpan $1 (ObjectSum $1 (toList $3)) }
 
fn_decl :: { FnDecl Span }
  : fn '(' sep_by1(arg_general,',') ',' '...' ')' ret_ty  {% withSpan $1 (FnDecl (toList $3) $> True) }
  | fn '(' sep_by1(arg_general,',') ',' ')' ret_ty        {% withSpan $1 (FnDecl (toList $3) $> False) }
  | fn '(' sep_by(arg_general,',') ')' ret_ty             {% withSpan $1 (FnDecl $3 $5 False) }


-- parse_ty_param_bounds(BoundParsingMode::Bare) == sep_by1(ty_param_bound,'+')
-- parse_ty_param_bounds(BoundParsingMode::Modified) == sep_by1(ty_param_bound_mod,'+') 

ty_param_bound :: { TyParamBound Span }
  : lifetime             { RegionTyParamBound $1 }
  | poly_trait_ref       { TraitTyParamBound $1 None }

ty_param_bound_mod :: { TyParamBound Span }
  : ty_param_bound       { $1 }
  | '?' poly_trait_ref   { TraitTyParamBound $2 Maybe }


-- parse_arg_general(false) -- does not require name
-- NOT ALL PATTERNS ARE ACCEPTED: <https://github.com/rust-lang/rust/issues/35203>
arg_general :: { Arg Span } 
  : ty_sum            {% withSpan $1 (Arg $1 Nothing) }
  | ident ':' ty_sum  {% withSpan $1 (Arg $3 (Just (IdentP (ByValue Immutable) (unspan $1) Nothing mempty))) }
  | '_'   ':' ty_sum  {% withSpan $1 (Arg $3 (Just (WildP mempty))) }

-- Sort of like parse_opt_abi() -- currently doesn't handle raw string ABI
abi :: { Abi }
  : str             {% case unspan $1 of
                         (LiteralTok (StrTok (Name s)) Nothing) | isAbi s -> pure (read s)
                         _ -> fail "invalid ABI"
                    }
  | {- empty -}     { C }

-- parse_ret_ty
ret_ty :: { Maybe (Ty Span) }
  : '->' ty         { Just $2 }
  | {- empty -}     { Nothing }

-- parse_poly_trait_ref()
poly_trait_ref :: { PolyTraitRef Span }
  : trait_ref                          {% withSpan $1 (PolyTraitRef [] $1) }
  | for_lts trait_ref {% withSpan $1 (PolyTraitRef (unspan $1) $2) }

-- parse_for_lts()
-- Unlike the Rust libsyntax version, this _requires_ the for
for_lts :: { Spanned [LifetimeDef Span] }
  : for '<' sep_by1(lifetime_def,',') ',' '>'   {% withSpan $1 (Spanned (toList $3)) } 
  | for '<' sep_by(lifetime_def,',') '>'        {% withSpan $1 (Spanned $3) } 

-- No corresponding parse function
lifetime_def :: { LifetimeDef Span }
  : outer_attribute many(outer_attribute) lifetime ':' sep_by1(lifetime,'+') {% withSpan $1 (LifetimeDef ($1 : $2) $3 (toList $5)) }
  | outer_attribute many(outer_attribute) lifetime                           {% withSpan $1 (LifetimeDef ($1 : $2) $3 []) }
  | lifetime ':' sep_by1(lifetime,'+')                                       {% withSpan $1 (LifetimeDef [] $1 (toList $3)) }
  | lifetime                                                                 {% withSpan $1 (LifetimeDef [] $1 []) }


--------------
-- Patterns --
--------------

-- TODO: Double-check that the error message in the one element tuple case makes sense. It should...
pat :: { Pat Span }
  : '_'                                         {% withSpan $1 WildP }
  | '&' mut pat                                 {% withSpan $1 (RefP $3 Mutable) }
  | '&' pat                                     {% withSpan $1 (RefP $2 Immutable) }
  | binding_mode1 ident at_pat                  {% withSpan $1 (IdentP (unspan $1) (unspan $2) $3) }
  | ident at_pat                                {% withSpan $1 (IdentP (ByValue Immutable) (unspan $1) $2) }
  | lit_expr                                    {% withSpan $1 (LitP $1) }
  | '-' lit_expr                                {% withSpan $1 (LitP (Unary [] Neg $2 mempty)) }
  | box pat                                     {% withSpan $1 (BoxP $2) }
  | complex_expr_path                           {% withSpan $1 (PathP Nothing $1) }
  | expr_qual_path                              {% withSpan $1 (PathP (Just (fst (unspan $1))) (snd (unspan $1))) }
  | lit_or_path '...' lit_or_path               {% withSpan $1 (RangeP $1 $3) }
  | expr_path '{' '..' '}'                      {% withSpan $1 (StructP $1 [] True) }
  | expr_path '{' pat_fields '}'                {% let (fs,b) = $3 in withSpan $1 (StructP $1 fs b) }
  | expr_path '(' pat_tup ')'                   {% let (ps,m,_) = $3 in withSpan $1 (TupleStructP $1 ps m) }
  | expr_mac                                    {% withSpan $1 (MacP $1) }
  | '[' pat_slice ']'                           {% let (b,s,a) = $2 in withSpan $1 (SliceP b s a) }
  | '(' pat_tup ')'                             {%
      case $2 of
        ([p], Nothing, False) -> fail "Syntax error: the symbol `)' does not fit here"
        (ps,m,t) -> withSpan $1 (TupleP ps m)
    }

-- The first element is the spans, the second the position of '..', and the third if there is a
-- trailing comma
pat_tup :: { ([Pat Span], Maybe Int, Bool) }
  : sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',')     { (toList ($1 <> $5), Just (length $1), False) }
  | sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',') ',' { (toList ($1 <> $5), Just (length $1), True) }
  | sep_by1(pat,',') ',' '..'                          { (toList $1,         Just (length $1), False) }
  | sep_by1(pat,',')                                   { (toList $1,         Nothing,          False) }
  | sep_by1(pat,',') ','                               { (toList $1,         Nothing,          True) }
  | '..' ',' sep_by1(pat,',')                          { (toList $3,         Just 0,           False) }
  | '..' ',' sep_by1(pat,',') ','                      { (toList $3,         Just 0,           True) }
  | '..'                                               { ([],                Just 0,           False) }
  | {- empty -}                                        { ([],                Nothing,          False) }

-- The first element is the patterns at the beginning of the slice, the second the optional binding
-- for the middle slice ('Nothing' if there is no '..' and 'Just (WildP mempty) is there is one, but
-- unlabelled), and the third is the patterns at the end of the slice.
pat_slice :: { ([Pat Span], Maybe (Pat Span), [Pat Span]) }
  : sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',') ',' { (toList $1, Just (WildP mempty), toList $5) }
  | sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',')     { (toList $1, Just (WildP mempty), toList $5) }
  | sep_by1(pat,',') ',' '..'                          { (toList $1, Just (WildP mempty), []) }
  | sep_by1(pat,',') '..' ',' sep_by1(pat,',')         { (N.init $1, Just (N.last $1),    toList $4) }
  | sep_by1(pat,',') '..' ',' sep_by1(pat,',') ','     { (N.init $1, Just (N.last $1),    toList $4) }
  | sep_by1(pat,',') '..'                              { (N.init $1, Just (N.last $1),    []) }
  | sep_by1(pat,',')                                   { (toList $1, Nothing,             []) }
  | sep_by1(pat,',') ','                               { (toList $1, Nothing,             []) }
  | '..' ',' sep_by1(pat,',')                          { ([],        Just (WildP mempty), toList $3) }
  | '..' ',' sep_by1(pat,',') ','                      { ([],        Just (WildP mempty), toList $3) }
  | '..'                                               { ([],        Just (WildP mempty), []) }
  | {- empty -}                                        { ([],        Nothing,             []) }


lit_or_path :: { Expr Span }
  : ident             {% withSpan $1 (PathExpr [] Nothing (Path False (fromList [(unspan $1, AngleBracketed [] [] [] mempty)]) mempty)) } 
  | complex_expr_path {% withSpan $1 (PathExpr [] Nothing $1) } 
  | expr_qual_path    {% withSpan $1 (PathExpr [] (Just (fst (unspan $1))) (snd (unspan $1))) }
  | '-' lit_expr      {% withSpan $1 (Unary [] Neg $2) }
  | lit_expr          { $1 }

pat_field :: { FieldPat Span }
  :     binding_mode ident     {% withSpan $1 (FieldPat Nothing (IdentP (unspan $1) (unspan $2) Nothing mempty)) }
  | box binding_mode ident     {% withSpan $1 (FieldPat Nothing (BoxP (IdentP (unspan $2) (unspan $3) Nothing mempty) mempty)) }
  | binding_mode ident ':' pat {% withSpan $1 (FieldPat (Just (unspan $2)) $4) }

pat_fields :: { ([FieldPat Span], Bool) }
  : pat_field ',' pat_fields   { let ~(fs,b) = $3 in ($1 : fs, b) }
  | pat_field ','              { ([$1], False) }
  | pat_field ',' '..'         { ([$1], True) }
  | pat_field                  { ([$1], False) }

-- Used prefixing IdentP patterns (not empty - that is a seperate pattern case)
binding_mode1 :: { Spanned BindingMode }
  : ref mut               {% withSpan $1 (Spanned (ByRef Mutable)) }
  | ref                   {% withSpan $1 (Spanned (ByRef Immutable)) }
  | mut                   {% withSpan $1 (Spanned (ByValue Mutable)) }

-- Used for patterns for fields (includes the empty case)
binding_mode :: { Spanned BindingMode }
  : binding_mode1         { $1 }
  | {- empty -}           { pure (ByValue Immutable) }

at_pat :: { Maybe (Pat Span) }
  : '@' pat     { Just $2 }
  | {- empty -} { Nothing }


-----------------
-- Expressions --
-----------------

lit_expr :: { Expr Span }
  : lit       {% withSpan $1 (Lit [] $1) }

field :: { Field Span }
  : ident ':' expr  {% withSpan $1 (Field (unspan $1) $3) }

expr :: { Expr Span }
  : expr_needs_semi                        { $1 }

-- Expressions that need a semicolon to turn them into statements (except in tail position of a
-- block)
-- TODO: deal with attributes
expr_needs_semi :: { Expr Span }
  : lit_expr                               { $1 }
  | '(' ')'                                {% withSpan $1 (TupExpr [] []) }
  | '(' expr ')'                           {% withSpan $1 (ParenExpr [] $2) }
  | '(' expr ',' ')'                       {% withSpan $1 (TupExpr [] [$2]) }
  | '(' expr ',' sep_by1(expr,',') ')'     {% withSpan $1 (TupExpr [] ($2 : toList $4)) }
  | block                                  {% withSpan $1 (BlockExpr [] $1) }
  | lambda_expr                            { $1 }
  | '[' expr ';' expr ']'                  {% withSpan $1 (Repeat [] $2 $4) } 
  | '[' ']'                                {% withSpan $1 (Vec [] []) }
  | '[' sep_by1(expr,',') ']'              {% withSpan $1 (Vec [] (toList $2)) }
  | '[' sep_by1(expr,',') ',' ']'          {% withSpan $1 (Vec [] (toList $2)) }
 -- | expr_path                              {% withSpan $1 (PathExpr [] Nothing $1) }  
 -- | expr_qual_path                         {% withSpan $1 (PathExpr [] (Just (fst $1)) (snd $1)) }  
  | return                                 {% withSpan $1 (Ret [] Nothing) }
 -- | return expr                            {% withSpan $1 (Ret [] (Just $2)) }
  | continue                               {% withSpan $1 (Continue [] Nothing) }
  | continue lifetime                      {% withSpan $1 (Continue [] (Just $2)) }
  | break                                  {% withSpan $1 (Break [] Nothing) }
  | break lifetime                         {% withSpan $1 (Break [] (Just $2)) }

block :: { Block Span }
  : alt(unsafe_block,safe_block)           { $1 }

unsafe_block : unsafe '{' many(stmt) '}' {% withSpan $1 (Block $3 (UnsafeBlock False)) }
safe_block   :        '{' many(stmt) '}' {% withSpan $1 (Block $2 DefaultBlock) }

lambda_expr :: { Expr Span }
  : move args '->' ty safe_block {% withSpan $1 (Closure [] Value (FnDecl $2 (Just $4) False mempty) (BlockExpr [] $> mempty)) }
  | move args         expr       {% withSpan $1 (Closure [] Value (FnDecl $2 Nothing   False mempty) $>) }
  |      args '->' ty safe_block {% withSpan $1 (Closure [] Ref   (FnDecl $1 (Just $3) False mempty) (BlockExpr [] $> mempty)) }
  |      args         expr       {% withSpan $1 (Closure [] Ref   (FnDecl $1 Nothing   False mempty) $>) }

arg :: { Arg Span }
  : pat ':' ty  {% withSpan $1 (Arg $3 (Just $1)) }
 -- |         ty  {% withSpan $1 (Arg $1 Nothing) }  -- conlicts with types (for instance, PathTy and PathPat). :(

args :: { [Arg Span] }
  : '|' '|'                       { [] }
  | '|' sep_by1(arg,',') '|'      { toList $2 }
  | '|' sep_by1(arg,',') ',' '|'  { toList $2 }

----------------
-- Statements --
----------------

-- TODO: consider attributes
stmt :: { Stmt Span }
  : let pat ':' ty '=' initializer ';'   {% withSpan $1 (Local $2 (Just $4) $6 []) }
  | let pat '=' initializer ';'          {% withSpan $1 (Local $2 Nothing $4 []) } 
  | expr ';'                             {% withSpan $1 (Semi $1) }
  | expr                                 {% withSpan $1 (NoSemi $1) }
--  | ';'                                  
 
initializer :: { Maybe (Expr Span) }
  : '=' expr      { Just $2 }
  | {- empty -}   { Nothing }


-------------------
-- Macro related --
-------------------

expr_mac :: { Mac Span }
  : expr_path '!' '[' many(token_tree) ']'      {% withSpan $1 (Mac $1 $4) }
  | expr_path '!' '{' many(token_tree) '}'      {% withSpan $1 (Mac $1 $4) }
  | expr_path '!' '(' many(token_tree) ')'      {% withSpan $1 (Mac $1 $4) }

ty_mac :: { Mac Span }
  : ty_path '!' '[' many(token_tree) ']'      {% withSpan $1 (Mac $1 $4) }
  | ty_path '!' '{' many(token_tree) '}'      {% withSpan $1 (Mac $1 $4) }
  | ty_path '!' '(' many(token_tree) ')'      {% withSpan $1 (Mac $1 $4) }

token_tree :: { TokenTree }
  : '(' many(token_tree) ')' { Delimited mempty Paren mempty $2 mempty }
  | '{' many(token_tree) ')' { Delimited mempty Brace mempty $2 mempty }
  | '[' many(token_tree) ']' { Delimited mempty Bracket mempty $2 mempty }
  -- Baaad
  | '$'        {% fail "unimplemented: this should do some SubstNt related stuff" } 
  -- Expression-operator symbols. 
  | '='        { Token mempty (unspan $1) }
  | '<'        { Token mempty (unspan $1) }
  | '>'        { Token mempty (unspan $1) }
  | '!'        { Token mempty (unspan $1) }
  | '~'        { Token mempty (unspan $1) }
  | '+'        { Token mempty (unspan $1) }
  | '-'        { Token mempty (unspan $1) }
  | '*'        { Token mempty (unspan $1) }
  | '/'        { Token mempty (unspan $1) }
  | '%'        { Token mempty (unspan $1) }
  | '^'        { Token mempty (unspan $1) }
  | '&'        { Token mempty (unspan $1) }
  | '|'        { Token mempty (unspan $1) }
{-  | '<<='    {  }
  | '>>='      {  }
  | '-='       {  }
  | '&='       {  }
  | '|='       {  }
  | '+='       {  }
  | '*='       {  }
  | '/='       {  }
  | '^='       {  }
  | '%='       {  }
  | '||'       {  }
  | '&&'       {  }
  | '=='       {  }
  | '!='       {  }
  | '<='       {  }
  | '>='       {  }
  | '<<'       {  }
  | '>>'       {  } -}
  -- Structural symbols.
  | '@'        { Token mempty (unspan $1) } 
  | '...'      { Token mempty (unspan $1) } 
  | '..'       { Token mempty (unspan $1) } 
  | '.'        { Token mempty (unspan $1) } 
  | ','        { Token mempty (unspan $1) } 
  | ';'        { Token mempty (unspan $1) } 
  | '::'       { Token mempty (unspan $1) } 
  | ':'        { Token mempty (unspan $1) } 
  | '->'       { Token mempty (unspan $1) } 
  | '<-'       { Token mempty (unspan $1) } 
  | '=>'       { Token mempty (unspan $1) } 
  | '#'        { Token mempty (unspan $1) } 
  | '?'        { Token mempty (unspan $1) } 
  -- Literals.
  | byte       { Token mempty (unspan $1) } 
  | char       { Token mempty (unspan $1) } 
  | int        { Token mempty (unspan $1) } 
  | float      { Token mempty (unspan $1) } 
  | str        { Token mempty (unspan $1) } 
  | byteStr    { Token mempty (unspan $1) } 
  | rawStr     { Token mempty (unspan $1) } 
  | rawByteStr { Token mempty (unspan $1) } 
  -- Strict keywords used in the language
  | as         { Token mempty (unspan $1) }
  | box        { Token mempty (unspan $1) }
  | break      { Token mempty (unspan $1) }
  | const      { Token mempty (unspan $1) }
  | continue   { Token mempty (unspan $1) }
  | crate      { Token mempty (unspan $1) }
  | else       { Token mempty (unspan $1) }
  | enum       { Token mempty (unspan $1) }
  | extern     { Token mempty (unspan $1) }
  | false      { Token mempty (unspan $1) }
  | fn         { Token mempty (unspan $1) }
  | for        { Token mempty (unspan $1) }
  | if         { Token mempty (unspan $1) }
  | impl       { Token mempty (unspan $1) }
  | in         { Token mempty (unspan $1) }
  | let        { Token mempty (unspan $1) }
  | loop       { Token mempty (unspan $1) }
  | match      { Token mempty (unspan $1) }
  | mod        { Token mempty (unspan $1) }
  | move       { Token mempty (unspan $1) }
  | mut        { Token mempty (unspan $1) }
  | pub        { Token mempty (unspan $1) }
  | ref        { Token mempty (unspan $1) }
  | return     { Token mempty (unspan $1) }
  | Self       { Token mempty (unspan $1) }
  | self       { Token mempty (unspan $1) }
  | static     { Token mempty (unspan $1) }
  | struct     { Token mempty (unspan $1) }
  | super      { Token mempty (unspan $1) }
  | trait      { Token mempty (unspan $1) }
  | true       { Token mempty (unspan $1) }
  | type       { Token mempty (unspan $1) }
  | unsafe     { Token mempty (unspan $1) }
  | use        { Token mempty (unspan $1) }
  | where      { Token mempty (unspan $1) }
  | while      { Token mempty (unspan $1) }
  -- Keywords reserved for future use
  | abstract   { Token mempty (unspan $1) }
  | alignof    { Token mempty (unspan $1) } 
  | become     { Token mempty (unspan $1) }
  | do         { Token mempty (unspan $1) }
  | final      { Token mempty (unspan $1) }
  | macro      { Token mempty (unspan $1) }
  | offsetof   { Token mempty (unspan $1) }
  | override   { Token mempty (unspan $1) }
  | priv       { Token mempty (unspan $1) }
  | proc       { Token mempty (unspan $1) }
  | pure       { Token mempty (unspan $1) }
  | sizeof     { Token mempty (unspan $1) }
  | typeof     { Token mempty (unspan $1) }
  | unsized    { Token mempty (unspan $1) } 
  | virtual    { Token mempty (unspan $1) } 
  | yield      { Token mempty (unspan $1) }
  -- Weak keywords, have special meaning only in specific contexts.
  | default    { Token mempty (unspan $1) }
  | union      { Token mempty (unspan $1) }
  -- Comments
  | outerDoc   { Token mempty (unspan $1) }
  | innerDoc   { Token mempty (unspan $1) }
  -- Identifiers.
  | IDENT      { Token mempty (unspan $1) }
  | '_'        { Token mempty (unspan $1) }
  -- Lifetimes.
  | LIFETIME   { Token mempty (unspan $1) }


{

-- | Given a 'LitTok' token that is expected to result in a valid literal, construct the associated
-- literal. Note that this should _never_ fail on a token produced by the lexer.
lit :: Spanned Token -> Lit Span
lit (Spanned (IdentTok (Ident (Name "true") _)) s) = Bool True Unsuffixed s
lit (Spanned (IdentTok (Ident (Name "false") _)) s) = Bool False Unsuffixed s
lit (Spanned (LiteralTok litTok suffix_m) s) = parseLit litTok suffix s
  where
    suffix = case suffix_m of
               Nothing -> Unsuffixed
               (Just (Name "isize")) -> Is
               (Just (Name "usize")) -> Us
               (Just (Name "i8"))    -> I8
               (Just (Name "u8"))    -> U8
               (Just (Name "i16"))   -> I16
               (Just (Name "u16"))   -> U16
               (Just (Name "i32"))   -> I32
               (Just (Name "u32"))   -> U32
               (Just (Name "i64"))   -> I64
               (Just (Name "u64"))   -> U64
               (Just (Name "i128"))  -> I128
               (Just (Name "u128"))  -> U128
               (Just (Name "f32"))   -> F32
               (Just (Name "f64"))   -> F64
               _ -> error "lit"

isPathSegmentIdent :: Spanned Ident -> Bool
isPathSegmentIdent i = True

isTypePathSegmentIdent :: Spanned Ident -> Bool
isTypePathSegmentIdent i = True

-- | Check if a given string is one of the accepted ABIs
isAbi :: InternedString -> Bool
isAbi s = s `elem` abis
  where abis = [ "Cdecl", "Stdcall", "Fastcall", "Vectorcall", "Aapcs", "Win64", "SysV64"
               , "Rust", "C", "System", "RustIntrinsic", "RustCall", "PlatformIntrinsic"
               ]

isTraitTyParamBound TraitTyParamBound{} = True
isTraitTyParamBound _ = False

-- | The second argument is the thing you are trying to add a 'Span' to. The first argument is
-- the first constituent of the thing we are annotating - it is passed in so that we can extract the
-- start of the 'Span'. The end of the 'Span' is determined from the current parser position.
withSpan :: Located node => node -> (Span -> a) -> P a
withSpan node mkNode = do
  let Span lo _ = posOf node
  hi <- getPosition
  pure (mkNode (Span lo hi))


-- Functions related to `NonEmpty` that really should already exist...

-- | Append an element to a list to get a nonempty list (flipped version of '(:|)')
(|:) :: [a] -> a -> NonEmpty a
[] |: y = y :| []
(x:xs) |: y = x :| (xs ++ [y])

-- | Append an element to a nonempty list to get anothg nonempty list (flipped version of '(<|)')
(|>) :: NonEmpty a -> a -> NonEmpty a
(x:|xs) |> y = x :| (xs ++ [y])

}
