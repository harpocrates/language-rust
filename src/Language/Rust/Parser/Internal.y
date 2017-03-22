{
{-|
Module      : Language.Rust.Parser.Internal
Description : Rust parser
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

The parsers in this file are all re-exported to 'Language.Rust.Parser' via the 'Parse' class.
-}
{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE OverloadedStrings, OverloadedLists #-}


module Language.Rust.Parser.Internal (
  parseLit, parseAttr, parseTy, parsePat, parseStmt, parseExpr, parseItem, parseCrate,
  parseBlock, parseImplItem, parseTraitItem, parseTt,
) where

import Language.Rust.Data.InputStream
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
import Language.Rust.Parser.Lexer
import Language.Rust.Parser.ParseMonad
import Language.Rust.Syntax.AST
import Language.Rust.Parser.Literals

import Data.Semigroup ((<>))
import Data.List.NonEmpty (NonEmpty(..), (<|), toList)
import qualified Data.List.NonEmpty as N
}

-- <https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y>
-- <https://github.com/rust-lang/rust/blob/master/src/libsyntax/parse/parser.rs>
-- References to <https://doc.rust-lang.org/grammar.html>
-- To see conflicts: stack exec happy -- --info=happyinfo.txt -o /dev/null src/Language/Rust/Parser/Parser.y

-- in order to document the parsers, we have to alias them
%name parseLit lit
%name parseAttr attribute
%name parseTy ty_sum     -- the exported parser for types really is for type sums (with object sums)
%name parsePat pat
%name parseStmt stmt
%name parseExpr expr
%name parseItem mod_item   -- the exported parser for items really is for mod items (with visibility)
%name parseCrate crate_
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
-- However, they are all S/R and seem to be currently doing what they should
%expect 1

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
  as             { Spanned (IdentTok (Ident "as" _)) _ }
  box            { Spanned (IdentTok (Ident "box" _)) _ } 
  break          { Spanned (IdentTok (Ident "break" _)) _ } 
  const          { Spanned (IdentTok (Ident "const" _)) _ } 
  continue       { Spanned (IdentTok (Ident "continue" _)) _ }
  crate          { Spanned (IdentTok (Ident "crate" _)) _ } 
  else           { Spanned (IdentTok (Ident "else" _)) _ }
  enum           { Spanned (IdentTok (Ident "enum" _)) _ }
  extern         { Spanned (IdentTok (Ident "extern" _)) _ }
  false          { Spanned (IdentTok (Ident "false" _)) _ } 
  fn             { Spanned (IdentTok (Ident "fn" _)) _ }
  for            { Spanned (IdentTok (Ident "for" _)) _ } 
  if             { Spanned (IdentTok (Ident "if" _)) _ }
  impl           { Spanned (IdentTok (Ident "impl" _)) _ }
  in             { Spanned (IdentTok (Ident "in" _)) _ }
  let            { Spanned (IdentTok (Ident "let" _)) _ } 
  loop           { Spanned (IdentTok (Ident "loop" _)) _ }
  match          { Spanned (IdentTok (Ident "match" _)) _ } 
  mod            { Spanned (IdentTok (Ident "mod" _)) _ } 
  move           { Spanned (IdentTok (Ident "move" _)) _ }
  mut            { Spanned (IdentTok (Ident "mut" _)) _ } 
  pub            { Spanned (IdentTok (Ident "pub" _)) _ } 
  ref            { Spanned (IdentTok (Ident "ref" _)) _ } 
  return         { Spanned (IdentTok (Ident "return" _)) _ }
  Self           { Spanned (IdentTok (Ident "Self" _)) _ }
  self           { Spanned (IdentTok (Ident "self" _)) _ } 
  static         { Spanned (IdentTok (Ident "static" _)) _ }
  struct         { Spanned (IdentTok (Ident "struct" _)) _ }
  super          { Spanned (IdentTok (Ident "super" _)) _ } 
  trait          { Spanned (IdentTok (Ident "trait" _)) _ } 
  true           { Spanned (IdentTok (Ident "true" _)) _ }
  type           { Spanned (IdentTok (Ident "type" _)) _ }
  unsafe         { Spanned (IdentTok (Ident "unsafe" _)) _ }
  use            { Spanned (IdentTok (Ident "use" _)) _ } 
  where          { Spanned (IdentTok (Ident "where" _)) _ } 
  while          { Spanned (IdentTok (Ident "while" _)) _ } 
  
  -- Keywords reserved for future use
  abstract       { Spanned (IdentTok (Ident "abstract" _)) _ }
  alignof        { Spanned (IdentTok (Ident "alignof" _)) _ } 
  become         { Spanned (IdentTok (Ident "become" _)) _ }
  do             { Spanned (IdentTok (Ident "do" _)) _ }
  final          { Spanned (IdentTok (Ident "final" _)) _ } 
  macro          { Spanned (IdentTok (Ident "macro" _)) _ } 
  offsetof       { Spanned (IdentTok (Ident "offsetof" _)) _ }
  override       { Spanned (IdentTok (Ident "override" _)) _ }
  priv           { Spanned (IdentTok (Ident "priv" _)) _ }
  proc           { Spanned (IdentTok (Ident "proc" _)) _ }
  pure           { Spanned (IdentTok (Ident "pure" _)) _ }
  sizeof         { Spanned (IdentTok (Ident "sizeof" _)) _ }
  typeof         { Spanned (IdentTok (Ident "typeof" _)) _ }
  unsized        { Spanned (IdentTok (Ident "unsized" _)) _ } 
  virtual        { Spanned (IdentTok (Ident "virtual" _)) _ } 
  yield          { Spanned (IdentTok (Ident "yield" _)) _ } 

  -- Weak keywords, have special meaning only in specific contexts.
  default        { Spanned (IdentTok (Ident "default" _)) _ } 
  union          { Spanned (IdentTok (Ident "union" _)) _ } 

  -- Comments
  outerDoc       { Spanned (Doc _ OuterDoc) _ }
  innerDoc       { Spanned (Doc _ InnerDoc) _ }

  -- Identifiers.
  IDENT          { Spanned (IdentTok (Ident _ _)) _ }
  '_'            { Spanned Underscore _ }

  -- Lifetimes.
  LIFETIME       { Spanned (LifetimeTok _) _ }

  -- macro related
  substNt        { Spanned (SubstNt _ ) _ }
  matchNt        { Spanned (MatchNt _ _) _ }

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

-- 'mut' should be lower precedence than 'IDENT' so that in the pat rule,
-- "& mut pat" has higher precedence than "binding_mode1 ident [@ pat]"
%nonassoc mut
%nonassoc IDENT ntIdent union default

%nonassoc box
%nonassoc return break continue IMPLTRAIT

%right '=' '>>=' '<<=' '-=' '+=' '*=' '/=' '^=' '|=' '&=' '%=' '..' '...' 
%right '<-'
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

%nonassoc POSTFIX VIS TYPATH EXPRPATH DOLLAR
%nonassoc '{' ntBlock '[' '(' '!'

%nonassoc WHERE

%%

-- Unwraps the IdentTok into just an Ident
-- For questionable reasons of backwards compatibility, 'union' and 'default' can be used as
-- identifiers, even if they are also keywords. They are "contextual" keywords.
--
-- Union's RFC: https://github.com/rust-lang/rfcs/blob/master/text/1444-union.md
ident :: { Spanned Ident }
  : ntIdent                       { fmap (\(Interpolated (NtIdent i)) -> i) $1 }
  | union                         { let Spanned (IdentTok i) s = $1 in Spanned i s }
  | default                       { let Spanned (IdentTok i) s = $1 in Spanned i s }
  | IDENT                         { let Spanned (IdentTok i) s = $1 in Spanned i s }

-- This should precede any '>' token which could be absorbed in a '>>', '>=', or '>>=' token. Its
-- purpose is to check if the lookahead token starts with '>' but contains more that. If that is
-- the case, it pushes two tokens, the first of which is '>'. We exploit the %% feature of threaded
-- lexers to discard what would have been the troublesome '>>', '>=', or '>>=' token.
gt :: { () }
  : {- empty -}   {%% \(Spanned tok s) ->
      case tok of
        GreaterGreater      -> pushToken (Spanned Greater s)      *> pushToken (Spanned Greater s)
        GreaterEqual        -> pushToken (Spanned Equal s)        *> pushToken (Spanned Greater s)
        GreaterGreaterEqual -> pushToken (Spanned GreaterEqual s) *> pushToken (Spanned Greater s)
        _                   -> pushToken (Spanned tok s)
    }


-------------
-- Utility --
-------------

-- | One or more
some(p) :: { NonEmpty a }
  : some(p) p          { $1 |> $2 }
  | p                  { [$1] }

-- | Zero or more
many(p) :: { [a] }
  : some(p)            { toList $1 }
  | {- empty -}        { [] }


-- | One or more occurences of p, seperated by sep
sep_by1(p,sep) :: { NonEmpty a }
  : sep_by1(p,sep) sep p  { $1 |> $3 }
  | p                     { [$1] }


-- | Zero or more occurrences of p, separated by sep
sep_by(p,sep) :: { [a] }
  : sep_by1(p,sep)     { toList $1 }
  | {- empty -}        { [] }


--------------------------
-- Whole file
--------------------------

-- shebang is dealt with at the top level, outside Happy/Alex
crate_ :: { Crate Span }
  : inner_attrs many(mod_item)   {% withSpan $1 (Crate $2 (toList $1) []) }
  |             many(mod_item)   {% withSpan $1 (Crate $1 [] []) }


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
                                       ; doc <- withSpan $1 (NameValue "doc" str)
                                       ; withSpan $1 (Attribute Outer doc True)
                                       }
                                 }

outer_attrs :: { NonEmpty (Attribute Span) }
  : some(outer_attribute)        { $1 }


inner_attribute :: { Attribute Span }
  : '#' '!' '[' meta_item ']'    {% withSpan $1 (Attribute Inner $4 False) } 
  | '#!'    '[' meta_item ']'    {% withSpan $1 (Attribute Inner $3 False) } 
  | innerDoc                     {% let Doc docStr InnerDoc = unspan $1 in
                                    do { str <- withSpan $1 (Str docStr Cooked Unsuffixed)
                                       ; doc <- withSpan $1 (NameValue "doc" str)
                                       ; withSpan $1 (Attribute Inner doc True)
                                       }
                                 }

-- TODO: for some precedence related reason, using 'some' here doesn't work
inner_attrs :: { NonEmpty (Attribute Span) }
  : inner_attrs inner_attribute  { $1 |> $2 }
  | inner_attribute              { [$1] }


-- parse_meta_item()
meta_item :: { MetaItem Span }
  : ntMeta                                          { $1 }
  | ident                                           {% withSpan $1 (Word (unspan $1)) }
  | ident '=' unsuffixed                            {% withSpan $1 (NameValue (unspan $1) $3) }
  | ident '(' sep_by(meta_item_inner,',')      ')'  {% withSpan $1 (List (unspan $1) $3) }
  | ident '(' sep_by1(meta_item_inner,',') ',' ')'  {% withSpan $1 (List (unspan $1) (toList $3)) }

-- parse_meta_item_inner()
meta_item_inner :: { NestedMetaItem Span }
  : unsuffixed                                      {% withSpan $1 (Literal $1) }
  | meta_item                                       {% withSpan $1 (MetaItem $1) } 


--------------
-- Literals --
--------------

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
  : '<' qual_path_suf(segs)                          { $2 }
  | '<<' ty_qual_path_suf as ty_path '>' '::' segs   {%
      let segs = segments $4 <> unspan $7
      in withSpan $1 (Spanned (QSelf $2 (length (segments $4)), $4{ segments = segs }))
    }

-- Basically a qualified path, but ignoring the very first '<' token
qual_path_suf(segs) :: { Spanned (QSelf Span, Path Span) }
  : ty_sum '>' '::' segs                {% withSpan $1 (Spanned (QSelf $1 0, Path False (unspan $4) (spanOf $4))) }
  | ty_sum as ty_path '>' '::' segs     {%
      let segs = segments $3 <> unspan $6
      in withSpan $1 (Spanned (QSelf $1 (length (segments $3)), $3{ segments = segs }))
    }

-- Usually qual_path_suf is for... type paths! (Since it deals with annoying '<<', like generic_values below!
ty_qual_path_suf :: { Ty Span }
  : qual_path_suf(path_segments_without_colons)
     {% let (qself,path) = unspan $1 in withSpan $1 (PathTy (Just qself) path) }
  | qual_path_suf(path_segments_without_colons) '+' sep_by1(ty_param_bound,'+')
     {% let (qself,path) = unspan $1 in withSpan $1 (ObjectSum (PathTy (Just qself) path mempty) (toList $3)) }

-- parse_generic_values_after_lt() but with the '<' '>'
generic_values :: { ([Lifetime Span], [Ty Span], [(Ident, Ty Span)]) }
  : '<' sep_by1(lifetime,',') ',' sep_by1(ty_sum,',') ',' sep_by1(binding,',') gt '>' { (toList $2, toList $4, toList $6) }
  | '<' sep_by1(lifetime,',') ',' sep_by1(ty_sum,',')                          gt '>' { (toList $2, toList $4, []) }
  | '<' sep_by1(lifetime,',') ','                         sep_by1(binding,',') gt '>' { (toList $2, [],        toList $4) }
  | '<' sep_by1(lifetime,',')                                                  gt '>' { (toList $2, [],        []) }
  | '<'                           sep_by1(ty_sum,',') ',' sep_by1(binding,',') gt '>' { ([],        toList $2, toList $4) }
  | '<'                           sep_by1(ty_sum,',')                          gt '>' { ([],        toList $2, []) }
  | '<'                                                   sep_by1(binding,',') gt '>' { ([],        [],        toList $2) }
  | '<'                                                                        gt '>' { ([],        [],        []) }
  | '<<' ty_qual_path_suf     ',' sep_by1(ty_sum,',') ',' sep_by1(binding,',') gt '>' { ([],   $2 : toList $4, toList $6) }      
  | '<<' ty_qual_path_suf     ',' sep_by1(ty_sum,',')                          gt '>' { ([],   $2 : toList $4, []) }
  | '<<' ty_qual_path_suf                             ',' sep_by1(binding,',') gt '>' { ([],        [$2],      toList $4) }
  | '<<' ty_qual_path_suf                                                      gt '>' { ([],        [$2],      []) }

binding : ident '=' ty                             { (unspan $1, $3) }


-- Type related:
-- parse_path(PathStyle::Type)
ty_path :: { Path Span }
  : ntPath                                   { $1 }
  | path_segments_without_colons             {% withSpan $1 (Path False (unspan $1)) }
  | '::' path_segments_without_colons        {% withSpan $1 (Path True (unspan $2)) }

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
  : generic_values                            { let (lts, tys, bds) = $1 in (AngleBracketed lts tys bds mempty) }
  | '(' sep_by(ty_sum,',')      ')'           {% withSpan $1 (Parenthesized $2 Nothing) }
  | '(' sep_by1(ty_sum,',') ',' ')'           {% withSpan $1 (Parenthesized (toList $2) Nothing) }
  | '(' sep_by(ty_sum,',')      ')' '->' ty   {% withSpan $1 (Parenthesized $2 (Just $>)) }
  | '(' sep_by1(ty_sum,',') ',' ')' '->' ty   {% withSpan $1 (Parenthesized (toList $2) (Just $>)) }
  | {- empty -}                   %prec IDENT { NoParameters mempty }


-- Expression related:
-- parse_path(PathStyle::Expr)
expr_path :: { Path Span }
  : ntPath                                  { $1 }
  | path_segments_with_colons               {% withSpan $1 (Path False (unspan $1)) }
  | '::' path_segments_with_colons          {% withSpan $1 (Path True (unspan $2)) }

expr_qual_path :: { Spanned (QSelf Span, Path Span) }
  : qual_path(path_segments_with_colons)  { $1 }

-- parse_path_segments_with_colons()
path_segments_with_colons :: { Spanned (NonEmpty (Ident, PathParameters Span)) }
  : ident
     {% withSpan $1 (Spanned [(unspan $1, NoParameters mempty)]) }
  | path_segments_with_colons '::' ident
     {% withSpan $1 (Spanned (unspan $1 |> (unspan $3, NoParameters mempty))) }
  | path_segments_with_colons '::' generic_values
     {%
       case (N.last (unspan $1), $3) of
         ((i, NoParameters{}), (lts, tys, bds)) -> withSpan $1 (Spanned (N.init (unspan $1) |: (i, AngleBracketed lts tys bds mempty)))
         _ -> error "invalid path segment in expression path"
    }
  
-- Mod related:
-- parse_path(PathStyle::Mod)
mod_path :: { Path Span  }
  : ntPath               { $1 }
  | self_or_ident        {% withSpan $1 (Path False [(unspan $1, NoParameters mempty)]) }
  | '::' self_or_ident   {% withSpan $1 (Path True  [(unspan $2, NoParameters mempty)]) }
  | mod_path '::' ident  {%
       withSpan $1 (Path (global $1) (segments $1 |> (unspan $3, NoParameters mempty)))
     }


-----------
-- Types --
-----------

lifetime :: { Lifetime Span }
  : LIFETIME                         { let Spanned (LifetimeTok (Ident l _)) s = $1 in Lifetime l s }

-- parse_trait_ref()
trait_ref :: { TraitRef Span }
  : ty_path                          { TraitRef $1 }

-- parse_ty()
ty :: { Ty Span }
  : ntTy                             { $1 }
  | no_for_ty                        { $1 }
  | for_ty                           { $1 }

-- All (non-sum) types not starting with '(' or '<'
ty_prim :: { Ty Span }
  : no_for_ty_prim                   { $1 }
  | for_ty                           { $1 }

-- All (non-sum) types not starting with a 'for'
no_for_ty :: { Ty Span }
  : no_for_ty_prim                   { $1 }
  | '(' ')'                          {% withSpan $1 (TupTy []) }
  | '(' ty_sum ')'                   {% withSpan $1 (ParenTy $2) }
  | '(' ty_sum ',' ')'               {% withSpan $1 (TupTy [$2]) }
  | '(' ty_sum ',' sep_by1(ty_sum,',') ')' {% withSpan $1 (TupTy ($2 : toList $4)) }
  | ty_qual_path                     {% withSpan $1 (PathTy (Just (fst (unspan $1))) (snd (unspan $1))) }

-- All (non-sum) types not starting with a 'for', '(', or '<'
no_for_ty_prim :: { Ty Span }
  : '_'                              {% withSpan $1 Infer }
  | '!'                              {% withSpan $1 Never }
  | '[' ty ']'                       {% withSpan $1 (Slice $2) }
  | '*' ty                           {% withSpan $1 (Ptr Immutable $2) }
  | '*' const ty                     {% withSpan $1 (Ptr Immutable $3) }
  | '*' mut ty                       {% withSpan $1 (Ptr Mutable $3) }
  | '&' lifetime_mut ty              {% withSpan $1 (Rptr (fst $2) (snd $2) $3) }
  | '&&' lifetime_mut ty             {% withSpan $1 (Rptr Nothing Immutable (Rptr (fst $2) (snd $2) $3 mempty)) }
  | ty_path             %prec TYPATH {% withSpan $1 (PathTy Nothing $1) }
  | ty_mac                           {% withSpan $1 (MacTy $1) } 
  | unsafe extern abi fn fn_decl     {% withSpan $1 (BareFn Unsafe $3 [] $>) }
  | unsafe fn fn_decl                {% withSpan $1 (BareFn Unsafe Rust [] $>) }
  | extern abi fn fn_decl            {% withSpan $1 (BareFn Normal $2 [] $>) }
  | fn fn_decl                       {% withSpan $1 (BareFn Normal Rust [] $>) }
  | typeof '(' expr ')'              {% withSpan $1 (Typeof $3) }
  | '[' ty ';' expr ']'              {% withSpan $1 (Array $2 $4) }
  | Self                             {% withSpan $1 ImplicitSelf }

-- All (non-sum) types starting with a 'for'
for_ty :: { Ty Span }
  : for_lts unsafe extern abi fn fn_decl {% withSpan $1 (BareFn Unsafe $4 (unspan $1) $>) }
  | for_lts unsafe fn fn_decl            {% withSpan $1 (BareFn Unsafe Rust (unspan $1) $>) }
  | for_lts extern abi fn fn_decl        {% withSpan $1 (BareFn Normal $3 (unspan $1) $>) }
  | for_lts fn fn_decl                   {% withSpan $1 (BareFn Normal Rust (unspan $1) $>) }
  | for_lts trait_ref                    {% 
      do poly <- withSpan $1 (PolyTraitRef (unspan $1) $2)
         withSpan $1 (TraitObject (TraitTyParamBound poly None :| []))
    }

-- An optional lifetime followed by an optional mutability
lifetime_mut :: { (Maybe (Lifetime Span), Mutability) }
  : lifetime mut  { (Just $1, Mutable) }
  | lifetime      { (Just $1, Immutable) }
  |          mut  { (Nothing, Mutable) }
  | {- empty -}   { (Nothing, Immutable) }

-- parse_ty_sum()
-- See https://github.com/rust-lang/rfcs/blob/master/text/0438-precedence-of-plus.md
-- All types, including sum types
ty_sum :: { Ty Span }
  : ty                                        { $1 }
  | ty '+' sep_by1(ty_param_bound,'+')        {% withSpan $1 (ObjectSum $1 (toList $3)) }

-- All types not starting with a '(' or '<'
ty_prim_sum :: { Ty Span }
  : ty_prim                                   { $1 }
  | ty_prim '+' sep_by1(ty_param_bound,'+')   {% withSpan $1 (ObjectSum $1 (toList $3)) }


-- The argument list and return type in a function
fn_decl :: { FnDecl Span }
  : '(' sep_by1(arg_general,',') ',' '...' ')' ret_ty  {% withSpan $1 (FnDecl (toList $2) $> True) }
  | '(' sep_by1(arg_general,',') ','       ')' ret_ty  {% withSpan $1 (FnDecl (toList $2) $> False) }
  | '(' sep_by(arg_general,',')            ')' ret_ty  {% withSpan $1 (FnDecl $2 $> False) }

-- Like 'fn_decl', but also accepting a self argument
fn_decl_with_self :: { FnDecl Span }
  : '(' arg_self ',' sep_by1(arg_general,',') ',' ')' ret_ty   {% withSpan $1 (FnDecl ($2 : toList $4) $> False) }
  | '(' arg_self ',' sep_by1(arg_general,',')     ')' ret_ty   {% withSpan $1 (FnDecl ($2 : toList $4) $> False) } 
  | '(' arg_self ','                              ')' ret_ty   {% withSpan $1 (FnDecl [$2] $> False) }
  | '(' arg_self                                  ')' ret_ty   {% withSpan $1 (FnDecl [$2] $> False) }
  | fn_decl                                                    { $1 }


-- parse_ty_param_bounds(BoundParsingMode::Bare) == sep_by1(ty_param_bound,'+')
ty_param_bound :: { TyParamBound Span }
  : lifetime             { RegionTyParamBound $1 }
  | poly_trait_ref       { TraitTyParamBound $1 None }

-- parse_ty_param_bounds(BoundParsingMode::Modified) == sep_by1(ty_param_bound_mod,'+') 
ty_param_bound_mod :: { TyParamBound Span }
  : ty_param_bound       { $1 }
  | '?' poly_trait_ref   { TraitTyParamBound $2 Maybe }


-- parse_arg_general(false) -- does not require name
-- NOT ALL PATTERNS ARE ACCEPTED: <https://github.com/rust-lang/rust/issues/35203>
arg_general :: { Arg Span } 
  :               ty_sum  {% withSpan $1 (Arg Nothing $1) }
  |     ident ':' ty_sum  {% withSpan $1 (Arg (Just (IdentP (ByValue Immutable) (unspan $1) Nothing mempty)) $3) }
  | mut ident ':' ty_sum  {% withSpan $1 (Arg (Just (IdentP (ByValue Mutable) (unspan $2) Nothing mempty)) $4) }
  |     '_'   ':' ty_sum  {% withSpan $1 (Arg (Just (WildP mempty)) $3) }

arg_self :: { Arg Span }
  :                  self {% withSpan $1 (SelfValue Immutable) }
  |              mut self {% withSpan $1 (SelfValue Mutable) }
  | '&' lifetime_mut self {% withSpan $1 (SelfRegion (fst $2) (snd $2)) }
  |     self ':' ty_sum   {% withSpan $1 (SelfExplicit $3 Immutable) }
  | mut self ':' ty_sum   {% withSpan $1 (SelfExplicit $4 Mutable) }


-- Sort of like parse_opt_abi() -- currently doesn't handle raw string ABI
abi :: { Abi }
  : str             {% case unspan $1 of
                         (LiteralTok (StrTok s) Nothing) | isAbi s -> pure (read s)
                         _ -> fail "invalid ABI"
                    }
  | {- empty -}     { C }

-- parse_ret_ty
-- Note that impl traits are still at RFC stage - they may eventually become accepted in more places
-- than just return types.
ret_ty :: { Maybe (Ty Span) }
  : '->' ty                                                { Just $2 }
  | '->' impl sep_by1(ty_param_bound,'+')  %prec IMPLTRAIT { Just (ImplTrait $3 mempty) }
  | {- empty -}                                            { Nothing }

-- parse_poly_trait_ref()
poly_trait_ref :: { PolyTraitRef Span }
  : trait_ref                                   {% withSpan $1 (PolyTraitRef [] $1) }
  | for_lts trait_ref                           {% withSpan $1 (PolyTraitRef (unspan $1) $2) }

-- parse_for_lts()
-- Unlike the Rust libsyntax version, this _requires_ the 'for'
for_lts :: { Spanned [LifetimeDef Span] }
  : for '<' sep_by1(lifetime_def,',') ',' '>'   {% withSpan $1 (Spanned (toList $3)) } 
  | for '<' sep_by(lifetime_def,',')      '>'   {% withSpan $1 (Spanned $3) } 

-- Definition of a lifetime: attributes can come before the lifetime, and a list of bounding
-- lifetimes can come after the lifetime.
lifetime_def :: { LifetimeDef Span }
  : outer_attribute many(outer_attribute) lifetime ':' sep_by1(lifetime,'+')
    {% withSpan $1 (LifetimeDef ($1 : $2) $3 (toList $5)) }
  | outer_attribute many(outer_attribute) lifetime
    {% withSpan $1 (LifetimeDef ($1 : $2) $3 []) }
  |                                       lifetime ':' sep_by1(lifetime,'+')
    {% withSpan $1 (LifetimeDef [] $1 (toList $3)) }
  |                                       lifetime
    {% withSpan $1 (LifetimeDef [] $1 []) }


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
  | '_'                             {% withSpan $1 WildP }
  | '&' mut pat                     {% withSpan $1 (RefP $3 Mutable) }
  | '&' pat                         {% withSpan $1 (RefP $2 Immutable) }
  | '&&' mut pat                    {% withSpan $1 (RefP (RefP $3 Mutable mempty) Immutable) }
  | '&&' pat                        {% withSpan $1 (RefP (RefP $2 Immutable mempty) Immutable) }
  |     lit_expr                    {% withSpan $1 (LitP $1) }
  | '-' lit_expr                    {% withSpan $1 (LitP (Unary [] Neg $2 mempty)) }
  | box pat                         {% withSpan $1 (BoxP $2) }
  | binding_mode1 ident '@' pat     {% withSpan $1 (IdentP (unspan $1) (unspan $2) (Just $4)) }
  | binding_mode1 ident             {% withSpan $1 (IdentP (unspan $1) (unspan $2) Nothing) }
  |               ident '@' pat     {% withSpan $1 (IdentP (ByValue Immutable) (unspan $1) (Just $3)) }
  | expr_path                       {%
       case $1 of
         Path False ((i, NoParameters _) :| []) _ -> withSpan $1 (IdentP (ByValue Immutable) i Nothing)
         _                                        -> withSpan $1 (PathP Nothing $1)
    }
  | expr_qual_path                  {% withSpan $1 (PathP (Just (fst (unspan $1))) (snd (unspan $1))) }
  | lit_or_path '...' lit_or_path   {% withSpan $1 (RangeP $1 $3) }
  | expr_path '{' '..' '}'          {% withSpan $1 (StructP $1 [] True) }
  | expr_path '{' pat_fields '}'    {% let (fs,b) = $3 in withSpan $1 (StructP $1 fs b) }
  | expr_path '(' pat_tup ')'       {% let (ps,m,_) = $3 in withSpan $1 (TupleStructP $1 ps m) }
  | expr_mac                        {% withSpan $1 (MacP $1) }
  | '[' pat_slice ']'               {% let (b,s,a) = $2 in withSpan $1 (SliceP b s a) }
  | '(' pat_tup ')'                 {%
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
  |                      '..' ',' sep_by1(pat,',')     { (toList $3,         Just 0,           False) }
  |                      '..' ',' sep_by1(pat,',') ',' { (toList $3,         Just 0,           True) }
  |                      '..'                          { ([],                Just 0,           False) }
  | {- empty -}                                        { ([],                Nothing,          False) }

-- The first element is the patterns at the beginning of the slice, the second the optional binding
-- for the middle slice ('Nothing' if there is no '..' and 'Just (WildP mempty) is there is one, but
-- unlabelled), and the third is the patterns at the end of the slice.
pat_slice :: { ([Pat Span], Maybe (Pat Span), [Pat Span]) }
  : sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',') ',' { (toList $1, Just (WildP mempty), toList $5) }
  | sep_by1(pat,',') ',' '..' ',' sep_by1(pat,',')     { (toList $1, Just (WildP mempty), toList $5) }
  | sep_by1(pat,',') ',' '..'                          { (toList $1, Just (WildP mempty), []) }
  | sep_by1(pat,',')     '..' ',' sep_by1(pat,',')     { (N.init $1, Just (N.last $1),    toList $4) }
  | sep_by1(pat,',')     '..' ',' sep_by1(pat,',') ',' { (N.init $1, Just (N.last $1),    toList $4) }
  | sep_by1(pat,',')     '..'                          { (N.init $1, Just (N.last $1),    []) }
  | sep_by1(pat,',')                                   { (toList $1, Nothing,             []) }
  | sep_by1(pat,',') ','                               { (toList $1, Nothing,             []) }
  |                      '..' ',' sep_by1(pat,',')     { ([],        Just (WildP mempty), toList $3) }
  |                      '..' ',' sep_by1(pat,',') ',' { ([],        Just (WildP mempty), toList $3) }
  |                      '..'                          { ([],        Just (WildP mempty), []) }
  | {- empty -}                                        { ([],        Nothing,             []) }


-- Endpoints of range patterns
lit_or_path :: { Expr Span }
  : expr_path         {% withSpan $1 (PathExpr [] Nothing $1) }
  | expr_qual_path    {% withSpan $1 (PathExpr [] (Just (fst (unspan $1))) (snd (unspan $1))) }
  | '-' lit_expr      {% withSpan $1 (Unary [] Neg $2) }
  |     lit_expr      { $1 }

-- Used in patterns for tuple and expression patterns
pat_fields :: { ([FieldPat Span], Bool) }
  : sep_by1(pat_field,',')           { (toList $1, False) }
  | sep_by1(pat_field,',') ','       { (toList $1, False) }
  | sep_by1(pat_field,',') ',' '..'  { (toList $1, True) }
  | {- empty -}                      { ([], False) }

pat_field :: { FieldPat Span }
  :     binding_mode ident
    {% withSpan $1 (FieldPat Nothing (IdentP (unspan $1) (unspan $2) Nothing mempty)) }
  | box binding_mode ident
    {% withSpan $1 (FieldPat Nothing (BoxP (IdentP (unspan $2) (unspan $3) Nothing mempty) mempty)) }
  |     binding_mode ident ':' pat
    {% withSpan $1 (FieldPat (Just (unspan $2)) $4) }


-- Used prefixing IdentP patterns (not empty - that is a seperate pattern case)
binding_mode1 :: { Spanned BindingMode }
  : ref mut                          {% withSpan $1 (Spanned (ByRef Mutable)) }
  | ref                              {% withSpan $1 (Spanned (ByRef Immutable)) }
  |     mut                          {% withSpan $1 (Spanned (ByValue Mutable)) }

-- Used for patterns for fields (includes the empty case)
binding_mode :: { Spanned BindingMode }
  : binding_mode1                    { $1 }
  | {- empty -}                      { pure (ByValue Immutable) }


-----------------
-- Expressions --
-----------------

lit_expr :: { Expr Span }
  : lit       {% withSpan $1 (Lit [] $1) }


-- General expressions, not restrictions
expr :: { Expr Span }
  : gen_expr                                                           { $1 }
  | arithmetic_expr                                                    { $1 }
  | lambda_expr                                                        { $1 }
arithmetic_expr :: { Expr Span }
  : gen_arithmetic(arithmetic_expr,arithmetic_expr,nsb_arithmetic_expr) { $1 }
  | postfix_expr                                                       { $1 } 
postfix_expr  :: { Expr Span }
  : gen_postfix_expr(postfix_expr)                                     { $1 } 
  | paren_expr                                                         { $1 }
  | struct_expr                                                        { $1 }
  | block_expr                                                         { $1 }


-- General expressions, but no structs
nostruct_expr :: { Expr Span }
  : gen_expr                                                           { $1 }
  | ns_arithmetic_expr                                                 { $1 }
  | lambda_expr_nostruct                                               { $1 }
ns_arithmetic_expr :: { Expr Span }
  : gen_arithmetic(ns_arithmetic_expr,ns_arithmetic_expr,nsb_arithmetic_expr) { $1 }
  | ns_postfix_expr                                                    { $1 } 
ns_postfix_expr  :: { Expr Span }
  : gen_postfix_expr(ns_postfix_expr)                                  { $1 } 
  | paren_expr                                                         { $1 }
  | block_expr                                                         { $1 }


-- General expressions, but no structs and no blocks (block-like things like if expressions or loops
-- are fine)
nostructblock_expr :: { Expr Span }
  : gen_expr                                                           { $1 }
  | nsb_arithmetic_expr                                                { $1 }
  | lambda_expr_nostruct                                               { $1 }
nsb_arithmetic_expr :: { Expr Span }
  : gen_arithmetic(nsb_arithmetic_expr,arithmetic_expr,nsb_arithmetic_expr) { $1 }
  | nsb_postfix_expr                                                   { $1 } 
nsb_postfix_expr  :: { Expr Span }
  : gen_postfix_expr(nsb_postfix_expr)                                 { $1 } 
  | paren_expr                                                         { $1 }
  | block_like_expr                                                    { $1 }


-- General expressions, but no blocks on the left
nonblock_expr :: { Expr Span }
  : gen_expr                                                           { $1 }
  | nb_arithmetic_expr                                                 { $1 }
  | lambda_expr_nostruct                                               { $1 }
nb_arithmetic_expr :: { Expr Span }
  : gen_arithmetic(nb_arithmetic_expr,arithmetic_expr,nsb_arithmetic_expr) { $1 }
  | nb_postfix_expr                                                    { $1 } 
nb_postfix_expr :: { Expr Span }
  : gen_postfix_expr(nb_postfix_expr)                                  { $1 }
  | paren_expr                                                         { $1 }
  | struct_expr                                                        { $1 }


-- "There is a convenience rule that allows one to omit the separating ; after if, match, loop, for, while"
block_expr :: { Expr Span }
  : block_like_expr                                           { $1 } 
  | block                                                     {% withSpan $1 (BlockExpr [] $1) }

-- Any block like expression except a block itself
block_like_expr :: { Expr Span }
  : if_expr                                                   { $1 }
  |              loop                            block        {% withSpan $1 (Loop [] $2 Nothing) }
  | lifetime ':' loop                            block        {% withSpan $1 (Loop [] $4 (Just $1)) }
  |              for pat in nostruct_expr        block        {% withSpan $1 (ForLoop [] $2 $4 $5 Nothing) }
  | lifetime ':' for pat in nostruct_expr        block        {% withSpan $1 (ForLoop [] $4 $6 $7 (Just $1)) }
  |              while             nostruct_expr block        {% withSpan $1 (While [] $2 $3 Nothing) }
  | lifetime ':' while             nostruct_expr block        {% withSpan $1 (While [] $4 $5 (Just $1)) }
  |              while let pat '=' nostruct_expr block        {% withSpan $1 (WhileLet [] $3 $5 $6 Nothing) }
  | lifetime ':' while let pat '=' nostruct_expr block        {% withSpan $1 (WhileLet [] $5 $7 $8 (Just $1)) }
  | match nostruct_expr '{' '}'                               {% withSpan $1 (Match [] $2 []) }
  | match nostruct_expr '{' arms '}'                          {% withSpan $1 (Match [] $2 $4) }
  | expr_path '!' '{' many(token_tree) '}'                    {% withSpan $1 (MacExpr [] (Mac $1 $4 mempty)) }
  | unsafe block                                              {% withSpan $1 (BlockExpr [] $2{ rules = Unsafe }) }

-- As per https://github.com/rust-lang/rust/issues/15701 (as of March 10 2017), the only way to have
-- attributes on expressions should be with inner attributes on a paren expression.
paren_expr :: { Expr Span }
  : '(' ')'                                {% withSpan $1 (TupExpr [] []) }
  | '(' expr ')'                           {% withSpan $1 (ParenExpr [] $2) }
  | '(' inner_attrs expr ')'               {% withSpan $1 (ParenExpr (toList $2) $3) }
  | '(' expr ',' ')'                       {% withSpan $1 (TupExpr [] [$2]) }
  | '(' expr ',' sep_by1(expr,',') ')'     {% withSpan $1 (TupExpr [] ($2 : toList $4)) }

-- General postfix expression
gen_postfix_expr(lhs) :: { Expr Span }
  : lit_expr                                                                    { $1 }
  | self                                                                        {% withSpan $1 (PathExpr [] Nothing (Path False [("self", NoParameters mempty)] mempty)) }
  | expr_path                                                    %prec EXPRPATH {% withSpan $1 (PathExpr [] Nothing $1) }
  | expr_qual_path                                                              {% withSpan $1 (PathExpr [] (Just (fst (unspan $1))) (snd (unspan $1))) }
  | expr_mac                                                                    {% withSpan $1 (MacExpr [] $1) }
  | '[' ']'                                                                     {% withSpan $1 (Vec [] []) }
  | '[' sep_by1(expr,',') ']'                                                   {% withSpan $1 (Vec [] (toList $2)) }
  | '[' sep_by1(expr,',') ',' ']'                                               {% withSpan $1 (Vec [] (toList $2)) }
  | '[' expr ';' expr ']'                                                       {% withSpan $1 (Repeat [] $2 $4) }
  | lhs '[' expr ']'                                                            {% withSpan $1 (Index [] $1 $3) }
  | lhs '?'                                                                     {% withSpan $1 (Try [] $1) }
  | lhs '(' ')'                                                                 {% withSpan $1 (Call [] $1 []) }
  | lhs '(' sep_by1(expr,',') ')'                                               {% withSpan $1 (Call [] $1 (toList $3)) }
  | lhs '(' sep_by1(expr,',') ',' ')'                                           {% withSpan $1 (Call [] $1 (toList $3)) }
  | lhs '.' ident '(' ')'                                                       {% withSpan $1 (MethodCall [] $1 (unspan $3) Nothing []) }
  | lhs '.' ident '(' sep_by1(expr,',') ')'                                     {% withSpan $1 (MethodCall [] $1 (unspan $3) Nothing (toList $5)) }
  | lhs '.' ident '(' sep_by1(expr,',') ',' ')'                                 {% withSpan $1 (MethodCall [] $1 (unspan $3) Nothing (toList $5)) }
  | lhs '.' ident '::' '<' sep_by(ty_sum,',') '>' '(' ')'                       {% withSpan $1 (MethodCall [] $1 (unspan $3) (Just $6) []) }
  | lhs '.' ident '::' '<' sep_by(ty_sum,',') '>' '(' sep_by1(expr,',') ')'     {% withSpan $1 (MethodCall [] $1 (unspan $3) (Just $6) (toList $9)) }
  | lhs '.' ident '::' '<' sep_by(ty_sum,',') '>' '(' sep_by1(expr,',') ',' ')' {% withSpan $1 (MethodCall [] $1 (unspan $3) (Just $6) (toList $9)) }
  | lhs '.' ident                                                 %prec POSTFIX {% withSpan $1 (FieldAccess [] $1 (unspan $3)) }
  | lhs '.' int                                                                 {%
      case lit $3 of
        Int i Unsuffixed _ -> withSpan $1 (TupField [] $1 (fromIntegral i))
        _ -> fail "make better error message"
    }

-- Arithmetic (unary and binary) generalized expressions. Precedences are handled by Happy (right
-- at the end of the token section)
gen_arithmetic(lhs,rhs,rhs2) :: { Expr Span }
  : '*' lhs                %prec UNARY     {% withSpan $1 (Unary [] Deref $2) }
  | '!' lhs                %prec UNARY     {% withSpan $1 (Unary [] Not $2) }
  | '-' lhs                %prec UNARY     {% withSpan $1 (Unary [] Neg $2) }
  | '&' lhs                %prec UNARY     {% withSpan $1 (AddrOf [] Immutable $2) }
  | '&' mut lhs            %prec UNARY     {% withSpan $1 (AddrOf [] Mutable $3) }
  | '&&' lhs               %prec UNARY     {% withSpan $1 (AddrOf [] Immutable (AddrOf [] Immutable $2 mempty)) }
  | '&&' mut lhs           %prec UNARY     {% withSpan $1 (AddrOf [] Immutable (AddrOf [] Mutable $3 mempty)) }
  | box lhs                %prec UNARY     {% withSpan $1 (Box [] $2) }
  | lhs ':' ty                             {% withSpan $1 (TypeAscription [] $1 $3) }
  | lhs as ty                              {% withSpan $1 (Cast [] $1 $3) }
  | lhs '*' rhs                            {% withSpan $1 (Binary [] MulOp $1 $3) }
  | lhs '/' rhs                            {% withSpan $1 (Binary [] DivOp $1 $3) }
  | lhs '%' rhs                            {% withSpan $1 (Binary [] RemOp $1 $3) }
  | lhs '+' rhs                            {% withSpan $1 (Binary [] AddOp $1 $3) }
  | lhs '-' rhs                            {% withSpan $1 (Binary [] SubOp $1 $3) }
  | lhs '<<' rhs                           {% withSpan $1 (Binary [] ShlOp $1 $3) }
  | lhs '>>' rhs                           {% withSpan $1 (Binary [] ShrOp $1 $3) }
  | lhs '&' rhs                            {% withSpan $1 (Binary [] BitAndOp $1 $3) }
  | lhs '^' rhs                            {% withSpan $1 (Binary [] BitXorOp $1 $3) }
  | lhs '|' rhs                            {% withSpan $1 (Binary [] BitOrOp $1 $3) }
  | lhs '==' rhs                           {% withSpan $1 (Binary [] EqOp $1 $3) }
  | lhs '!=' rhs                           {% withSpan $1 (Binary [] NeOp $1 $3) }
  | lhs '<'  rhs                           {% withSpan $1 (Binary [] LtOp $1 $3) }
  | lhs '>'  rhs                           {% withSpan $1 (Binary [] GtOp $1 $3) }
  | lhs '<=' rhs                           {% withSpan $1 (Binary [] LeOp $1 $3) }
  | lhs '>=' rhs                           {% withSpan $1 (Binary [] GeOp $1 $3) }
  | lhs '&&' rhs                           {% withSpan $1 (Binary [] AndOp $1 $3) }
  | lhs '||' rhs                           {% withSpan $1 (Binary [] OrOp $1 $3) }
  | lhs '<-' rhs                           {% withSpan $1 (InPlace [] $1 $3) }
  | lhs '=' rhs                            {% withSpan $1 (Assign   [] $1 $3) }
  | lhs '>>=' rhs                          {% withSpan $1 (AssignOp [] ShrOp $1 $3) }
  | lhs '<<=' rhs                          {% withSpan $1 (AssignOp [] ShlOp $1 $3) }
  | lhs '-=' rhs                           {% withSpan $1 (AssignOp [] SubOp $1 $3) }
  | lhs '+=' rhs                           {% withSpan $1 (AssignOp [] AddOp $1 $3) }
  | lhs '*=' rhs                           {% withSpan $1 (AssignOp [] MulOp $1 $3) }
  | lhs '/=' rhs                           {% withSpan $1 (AssignOp [] DivOp $1 $3) }
  | lhs '^=' rhs                           {% withSpan $1 (AssignOp [] BitXorOp $1 $3) }
  | lhs '|=' rhs                           {% withSpan $1 (AssignOp [] BitOrOp $1 $3) }
  | lhs '&=' rhs                           {% withSpan $1 (AssignOp [] BitAndOp $1 $3) }
  | lhs '%=' rhs                           {% withSpan $1 (AssignOp [] RemOp $1 $3) }
  | lhs '..'                               {% withSpan $1 (Range [] (Just $1) Nothing Closed) }
  | lhs '...'                              {% withSpan $1 (Range [] (Just $1) Nothing Closed) }
  | lhs '..' rhs2                          {% withSpan $1 (Range [] (Just $1) (Just $3) Closed) }
  | lhs '...' rhs2                         {% withSpan $1 (Range [] (Just $1) (Just $3) HalfOpen) }

-- Lowest precedence generalized expression
gen_expr :: { Expr Span }
  : ntExpr                                 { $1 }
  | return                                 {% withSpan $1 (Ret [] Nothing) }
  | return expr                            {% withSpan $1 (Ret [] (Just $2)) }
  | '..'                                   {% withSpan $1 (Range [] Nothing Nothing Closed) }
  | '...'                                  {% withSpan $1 (Range [] Nothing Nothing HalfOpen) }
  | '..' expr                              {% withSpan $1 (Range [] Nothing (Just $2) Closed) }
  | '...' expr                             {% withSpan $1 (Range [] Nothing (Just $2) HalfOpen) }
  | continue                               {% withSpan $1 (Continue [] Nothing) }
  | continue lifetime                      {% withSpan $1 (Continue [] (Just $2)) }
  | break                                  {% withSpan $1 (Break [] Nothing Nothing) }
  | break          expr                    {% withSpan $1 (Break [] Nothing (Just $2)) }
  | break lifetime                         {% withSpan $1 (Break [] (Just $2) Nothing) }
  | break lifetime expr                    {% withSpan $1 (Break [] (Just $2) (Just $3)) }



-- Match arms usually have to be seperated by commas (with an optional comma at the end). This
-- condition is loosened (so that there is no seperator needed) if the arm ends in a safe block.
arms :: { [Arm Span] }
  : ntArm                                                     { [$1] }
  | ntArm arms                                                { $1 : $2 }
  | outer_attrs sep_by1(pat,'|') arm_guard '=>' expr_arms     { let (e,as) = $> in (Arm (toList $1) $2 $3 e mempty : as) }
  |             sep_by1(pat,'|') arm_guard '=>' expr_arms     { let (e,as) = $> in (Arm [] $1 $2 e mempty : as) }

comma_arms :: { [Arm Span] }
  : {- empty -}    { [] }
  | ','            { [] }
  | ',' arms       { $2 }

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
  | block                             comma_arms              { (BlockExpr [] $1 mempty, $2) }
  | block                                   arms              { (BlockExpr [] $1 mempty, $2) }
  
comma_nsb_arithmetic_expr(c) :: { Expr Span }
  : nsb_arithmetic_expr c                                     { $1 }

arm_guard :: { Maybe (Expr Span) }
  : {- empty -}  { Nothing }
  | if expr      { Just $2 }

if_expr :: { Expr Span }
  : if             nostruct_expr block else_expr {% withSpan $1 (If [] $2 $3 $4) }
  | if let pat '=' nostruct_expr block else_expr {% withSpan $1 (IfLet [] $3 $5 $6 $7) }

else_expr :: { Maybe (Expr Span) }
  : else block      {% Just <\$> withSpan $1 (BlockExpr [] $2) }
  | else if_expr    { Just $2 }
  | {- empty -}     { Nothing }

lambda_expr :: { Expr Span }
  : move args '->' ty block   {% withSpan $1 (Closure [] Value (FnDecl $2 (Just $4) False mempty) (BlockExpr [] $> mempty)) }
  | move args         expr    {% withSpan $1 (Closure [] Value (FnDecl $2 Nothing   False mempty) $>) }
  |      args '->' ty block   {% withSpan $1 (Closure [] Ref   (FnDecl $1 (Just $3) False mempty) (BlockExpr [] $> mempty)) }
  |      args         expr    {% withSpan $1 (Closure [] Ref   (FnDecl $1 Nothing   False mempty) $>) }

lambda_expr_nostruct :: { Expr Span }
  : move args nostruct_expr      {% withSpan $1 (Closure [] Value (FnDecl $2 Nothing   False mempty) $>) }
  |      args nostruct_expr      {% withSpan $1 (Closure [] Ref   (FnDecl $1 Nothing   False mempty) $>) }


struct_expr :: { Expr Span }
  : expr_path '{'                                  '}'   {% withSpan $1 (Struct [] $1 [] Nothing) }
  | expr_path '{'                        '..' expr '}'   {% withSpan $1 (Struct [] $1 [] (Just $4)) }
  | expr_path '{' sep_by1(field,',') ',' '..' expr '}'   {% withSpan $1 (Struct [] $1 (toList $3) (Just $6)) }
  | expr_path '{' sep_by1(field,',')               '}'   {% withSpan $1 (Struct [] $1 (toList $3) Nothing) }
  | expr_path '{' sep_by1(field,',') ','           '}'   {% withSpan $1 (Struct [] $1 (toList $3) Nothing) }

field :: { Field Span }
  : ident ':' expr  {% withSpan $1 (Field (unspan $1) $3) }
  | ident           {% withSpan $1 (Field (unspan $1) (PathExpr [] Nothing (Path False ((unspan $1, NoParameters mempty) :| [])  mempty) mempty)) }

arg :: { Arg Span }
  : ntArg       { $1 }
  | pat ':' ty  {% withSpan $1 (Arg (Just $1) $3) }
  | pat         {% withSpan $1 (Arg (Just $1) (Infer mempty)) }

args :: { [Arg Span] }
  : '|' '|'                       { [] }
  | '||'                          { [] }
  | '|' sep_by1(arg,',') '|'      { toList $2 }
  | '|' sep_by1(arg,',') ',' '|'  { toList $2 }


----------------
-- Statements --
----------------

stmt :: { Stmt Span }
  : ntStmt                                      { $1 }
  |             let pat ':' ty initializer ';'  {% withSpan $1 (Local $2 (Just $4) $5 []) }
  | outer_attrs let pat ':' ty initializer ';'  {% withSpan $1 (Local $3 (Just $5) $6 (toList $1)) }
  |             let pat        initializer ';'  {% withSpan $1 (Local $2 Nothing $3 []) } 
  | outer_attrs let pat        initializer ';'  {% withSpan $1 (Local $3 Nothing $4 (toList $1)) } 
  |             nonblock_expr ';'               {% toStmt $1 True }
  | outer_attrs nonblock_expr ';'               {% toStmt (toList $1 `addAttrs` $2) True }
  |             block_expr                      {% toStmt $1 False }
  |             block_expr ';'                  {% toStmt $1 True }
  | outer_attrs block_expr                      {% toStmt (toList $1 `addAttrs` $2) False }
  | outer_attrs block_expr ';'                  {% toStmt (toList $1 `addAttrs` $2) True }
  |                 stmt_item                   {% withSpan $1 (ItemStmt $1) }
  | outer_attrs     stmt_item                   {% withSpan $1 (ItemStmt (let Item i a n v s = $2 in Item i (toList $1 ++ a) n v s)) }
  |             pub stmt_item                   {% withSpan $1 (ItemStmt (let Item i a n _ s = $2 in Item i a n PublicV s)) }
  | outer_attrs pub stmt_item                   {% withSpan $1 (ItemStmt (let Item i a n _ s = $3 in Item i (toList $1 ++ a) n PublicV s)) }
--  | ';'                                          

-- List of statements where the last statement might be a no-semicolon statement.
stmts_possibly_no_semi :: { [Stmt Span] }
  : stmt stmts_possibly_no_semi              { $1 : $2 }
  | stmt                                     { [$1] }
  | nonblock_expr                            { [NoSemi $1 mempty] }

initializer :: { Maybe (Expr Span) }
  : '=' expr      { Just $2 }
  | {- empty -}   { Nothing }


block :: { Block Span }
  : ntBlock                        { $1 }
  | '{' '}'                        {% withSpan $1 (Block [] Normal) }
  | '{' stmts_possibly_no_semi '}' {% withSpan $1 (Block $2 Normal) }

inner_attrs_block :: { ([Attribute Span], Block Span) }
  : block                                        { ([], $1) }
  | '{' inner_attrs '}'                          { (toList $2, Block [] Normal mempty) }
  | '{' inner_attrs stmts_possibly_no_semi '}'   { (toList $2, Block $3 Normal mempty) }

-----------
-- Items --
-----------

item :: { Item Span }
  : ntItem                                       { $1 }
  | stmt_item                                    { $1 }
  | expr_path '!' ident '[' many(token_tree) ']' ';'   {% withSpan $1 (Item (unspan $3) [] (MacItem (Mac $1 $5 mempty)) InheritedV) }
  | expr_path '!'       '[' many(token_tree) ']' ';'   {% withSpan $1 (Item (mkIdent "") [] (MacItem (Mac $1 $4 mempty)) InheritedV) }
  | expr_path '!' ident '(' many(token_tree) ')' ';'   {% withSpan $1 (Item (unspan $3) [] (MacItem (Mac $1 $5 mempty)) InheritedV) }
  | expr_path '!'       '(' many(token_tree) ')' ';'   {% withSpan $1 (Item (mkIdent "") [] (MacItem (Mac $1 $4 mempty)) InheritedV) }
  | expr_path '!' ident '{' many(token_tree) '}'       {% withSpan $1 (Item (unspan $3) [] (MacItem (Mac $1 $5 mempty)) InheritedV) }
  | expr_path '!'       '{' many(token_tree) '}'       {% withSpan $1 (Item (mkIdent "") [] (MacItem (Mac $1 $4 mempty)) InheritedV) }

mod_item :: { Item Span }
  : outer_attrs vis item                         {% let Item i a n _ _ = $3 in withSpan $2 (Item i (toList $1 ++ a) n (unspan $2)) }
  |             vis item                         {% let Item i a n _ _ = $2 in withSpan $1 (Item i a n (unspan $1)) }

foreign_item :: { ForeignItem Span }
  :             vis static ident ':' ty ';'
      {% withSpan $1 (ForeignItem (unspan $3) [] (ForeignStatic $5 False) (unspan $1)) }
  | outer_attrs vis static ident ':' ty ';'
      {% withSpan $1 (ForeignItem (unspan $4) (toList $1) (ForeignStatic $6 False) (unspan $2)) }
  |             vis static mut ident ':' ty ';'
      {% withSpan $1 (ForeignItem (unspan $4) [] (ForeignStatic $6 True) (unspan $1)) }
  | outer_attrs vis static mut ident ':' ty ';'
      {% withSpan $1 (ForeignItem (unspan $5) (toList $1) (ForeignStatic $7 True) (unspan $2)) }
  |             vis fn ident generics fn_decl where_clause ';'
      {% withSpan $1 (ForeignItem (unspan $3) [] (ForeignFn $5 $4{ whereClause = $6 }) (unspan $1)) }
  | outer_attrs vis fn ident generics fn_decl where_clause ';'
      {% withSpan $1 (ForeignItem (unspan $4) (toList $1) (ForeignFn $6 $5{ whereClause = $7 }) (unspan $2)) }

-- parse_generics
-- Leaves the WhereClause empty
generics :: { Generics Span }
  : {- empty -}                                                    { Generics [] [] (WhereClause [] mempty) mempty }
  | ntGenerics                                                     { $1 }
  | '<' sep_by1(lifetime_def,',') ',' sep_by1(ty_param,',') gt '>' {% withSpan $1 (Generics (toList $2) (toList $4) (WhereClause [] mempty)) }
  | '<' sep_by1(lifetime_def,',')                           gt '>' {% withSpan $1 (Generics (toList $2) []          (WhereClause [] mempty)) }
  | '<'                               sep_by1(ty_param,',') gt '>' {% withSpan $1 (Generics []          (toList $2) (WhereClause [] mempty)) }
  | '<'                                                     gt '>' {% withSpan $1 (Generics []          []          (WhereClause [] mempty)) }

ty_param :: { TyParam Span }
  : ident                                             {% withSpan $1 (TyParam [] (unspan $1) [] Nothing) }
  | ident ':' sep_by1(ty_param_bound_mod,'+')         {% withSpan $1 (TyParam [] (unspan $1) (toList $3) Nothing) }
  | ident                                     '=' ty  {% withSpan $1 (TyParam [] (unspan $1) [] (Just $>)) }
  | ident ':' sep_by1(ty_param_bound_mod,'+') '=' ty  {% withSpan $1 (TyParam [] (unspan $1) (toList $3) (Just $>)) }

stmt_item :: { Item Span }
  : static     ident ':' ty '=' expr ';'                 {% withSpan $1 (Item (unspan $2) [] (Static $4 Immutable $6) InheritedV) }
  | static mut ident ':' ty '=' expr ';'                 {% withSpan $1 (Item (unspan $3) [] (Static $5 Mutable $7) InheritedV) }
  | const ident ':' ty '=' expr ';'                      {% withSpan $1 (Item (unspan $2) [] (ConstItem $4 $6) InheritedV) }
  | type ident generics where_clause '=' ty_sum ';'      {% withSpan $1 (Item (unspan $2) [] (TyAlias $6 $3{ whereClause = $4 }) InheritedV) }
  | use view_path ';'                                    {% withSpan $1 (Item (mkIdent "") [] (Use $2) InheritedV) }
  | extern crate ident ';'                               {% withSpan $1 (Item (unspan $3) [] (ExternCrate Nothing) InheritedV) } 
  | extern crate ident as ident ';'                      {% withSpan $1 (Item (unspan $3) [] (ExternCrate (Just (unspan $5))) InheritedV) } 
  | const safety   fn ident generics fn_decl where_clause inner_attrs_block
    {% withSpan $1 (Item (unspan $4) (fst $>) (Fn $6 $2 Const Rust $5{ whereClause = $7 } (snd $>)) InheritedV) }
  | unsafe ext_abi fn ident generics fn_decl where_clause inner_attrs_block
    {% withSpan $1 (Item (unspan $4) (fst $>) (Fn $6 Unsafe NotConst $2 $5{ whereClause = $7 } (snd $>)) InheritedV) }
  | extern     abi fn ident generics fn_decl where_clause inner_attrs_block
    {% withSpan $1 (Item (unspan $4) (fst $>) (Fn $6 Normal NotConst $2 $5{ whereClause = $7 } (snd $>)) InheritedV) }
  |                fn ident generics fn_decl where_clause inner_attrs_block
    {% withSpan $1 (Item (unspan $2) (fst $>) (Fn $4 Normal NotConst Rust $3{ whereClause = $5 } (snd $>)) InheritedV) }
  | mod ident ';'                                        {% withSpan $1 (Item (unspan $2) [] (Mod []) InheritedV) }
  | mod ident '{'             many(mod_item) '}'         {% withSpan $1 (Item (unspan $2) [] (Mod $4) InheritedV) }
  | mod ident '{' inner_attrs many(mod_item) '}'         {% withSpan $1 (Item (unspan $2) (toList $4) (Mod $5) InheritedV) }
  | extern abi '{'             many(foreign_item) '}'    {% withSpan $1 (Item (mkIdent "") [] (ForeignMod $2 $4) InheritedV) }
  | extern abi '{' inner_attrs many(foreign_item) '}'    {% withSpan $1 (Item (mkIdent "") (toList $4) (ForeignMod $2 $5) InheritedV) }
  | struct ident generics where_clause struct_decl_args  {% withSpan $1 (Item (unspan $2) [] (StructItem $5 $3{ whereClause = $4 }) InheritedV) }
  | union ident generics where_clause struct_decl_args   {% withSpan $1 (Item (unspan $2) [] (Union $5 $3{ whereClause = $4 }) InheritedV) }
  | enum ident generics where_clause enum_defs           {% withSpan $1 (Item (unspan $2) [] (Enum $5 $3{ whereClause = $4 }) InheritedV) }
  | item_impl                                            { $1 }
  | item_trait                                           { $1 }

item_trait :: { Item Span }
  : unsafe trait ident generics ':' sep_by1(ty_param_bound,'+') where_clause '{' many(trait_item) '}'
     {% withSpan $1 (Item (unspan $3) [] (Trait Unsafe $4{ whereClause = $7 } (toList $6) $9) InheritedV) }
  | unsafe trait ident generics where_clause '{' many(trait_item) '}'
     {% withSpan $1 (Item (unspan $3) [] (Trait Unsafe $4{ whereClause = $5 } [] $7) InheritedV) }
  |        trait ident generics ':' sep_by1(ty_param_bound,'+') where_clause '{' many(trait_item) '}'
     {% withSpan $1 (Item (unspan $2) [] (Trait Normal $3{ whereClause = $6 } (toList $5) $8) InheritedV) }
  |        trait ident generics where_clause '{' many(trait_item) '}'
     {% withSpan $1 (Item (unspan $2) [] (Trait Normal $3{ whereClause = $4 } [] $6) InheritedV) }

struct_decl_args :: { VariantData Span }
  : ';'                                                {% withSpan $1 (StructD []) }
  | '{' '}'                                            {% withSpan $1 (StructD []) }
  | '{' sep_by1(struct_decl_field,',') '}'             {% withSpan $1 (StructD (toList $2)) }
  | '{' sep_by1(struct_decl_field,',') ',' '}'         {% withSpan $1 (StructD (toList $2)) }
  | '(' ')' ';'                                        {% withSpan $1 (TupleD []) }
  | '(' sep_by1(tuple_decl_field,',') ')' ';'          {% withSpan $1 (TupleD (toList $2)) }
  | '(' sep_by1(tuple_decl_field,',') ',' ')' ';'      {% withSpan $1 (TupleD (toList $2)) }

struct_decl_field :: { StructField Span }
  : outer_attrs vis ident ':' ty_sum                   {% withSpan $1 (StructField (Just (unspan $3)) (unspan $2) $5 (toList $1)) }
  |             vis ident ':' ty_sum                   {% withSpan $1 (StructField (Just (unspan $2)) (unspan $1) $4 []) }

enum_defs :: { [Variant Span] }
  : '{' '}'                                            { [] }
  | '{' sep_by1(enum_def,',')     '}'                  { toList $2 }
  | '{' sep_by1(enum_def,',') ',' '}'                  { toList $2 }

tuple_decl_field :: { StructField Span }
  : many(outer_attribute) vis ty_sum                   {% withSpan $1 (StructField Nothing (unspan $2) $3 $1) }

enum_def :: { Variant Span }
  : outer_attrs ident '{' '}'                                       {% withSpan $1 (Variant (unspan $2) (toList $1) (StructD [] mempty) Nothing) }
  |             ident '{' '}'                                       {% withSpan $1 (Variant (unspan $1) [] (StructD [] mempty) Nothing) }
  | outer_attrs ident '{' sep_by1(struct_decl_field,',') '}'        {% withSpan $1 (Variant (unspan $2) (toList $1) (StructD (toList $4) mempty) Nothing) }
  |             ident '{' sep_by1(struct_decl_field,',') '}'        {% withSpan $1 (Variant (unspan $1) [] (StructD (toList $3) mempty) Nothing) }
  | outer_attrs ident '{' sep_by1(struct_decl_field,',') ',' '}'    {% withSpan $1 (Variant (unspan $2) (toList $1) (StructD (toList $4) mempty) Nothing) }
  |             ident '{' sep_by1(struct_decl_field,',') ',' '}'    {% withSpan $1 (Variant (unspan $1) [] (StructD (toList $3) mempty) Nothing) }
  | outer_attrs ident '(' ')'                                       {% withSpan $1 (Variant (unspan $2) (toList $1) (TupleD [] mempty) Nothing) }
  |             ident '(' ')'                                       {% withSpan $1 (Variant (unspan $1) [] (TupleD [] mempty) Nothing) }
  | outer_attrs ident '(' sep_by1(tuple_decl_field,',') ')'         {% withSpan $1 (Variant (unspan $2) (toList $1) (TupleD (toList $4) mempty) Nothing) }
  |             ident '(' sep_by1(tuple_decl_field,',') ')'         {% withSpan $1 (Variant (unspan $1) [] (TupleD (toList $3) mempty) Nothing) }
  | outer_attrs ident '(' sep_by1(tuple_decl_field,',') ',' ')'     {% withSpan $1 (Variant (unspan $2) (toList $1) (TupleD (toList $4) mempty) Nothing) }
  |             ident '(' sep_by1(tuple_decl_field,',') ',' ')'     {% withSpan $1 (Variant (unspan $1) [] (TupleD (toList $3) mempty) Nothing) }
  | outer_attrs ident '=' expr                                      {% withSpan $1 (Variant (unspan $2) (toList $1) (UnitD mempty) (Just $4)) }
  |             ident '=' expr                                      {% withSpan $1 (Variant (unspan $1) [] (UnitD mempty) (Just $3)) }
  | outer_attrs ident                                               {% withSpan $1 (Variant (unspan $2) (toList $1) (UnitD mempty) Nothing) }
  |             ident                                               {% withSpan $1 (Variant (unspan $1) [] (UnitD mempty) Nothing) }


-- parse_where_clause
where_clause :: { WhereClause Span }
  : {- empty -}                                        { WhereClause [] mempty }
  | ntWhereClause                                      { $1 } 
  | where sep_by1(where_predicate,',')     %prec WHERE {% withSpan $1 (WhereClause (toList $2)) }
  | where sep_by1(where_predicate,',') ',' %prec WHERE {% withSpan $1 (WhereClause (toList $2)) }

where_predicate :: { WherePredicate Span }
  : lifetime ':' sep_by(lifetime,'+')                      {% withSpan $1 (RegionPredicate $1 $3) }
  | ty_path '=' ty                                         {% withSpan $1 (EqPredicate $1 $3) }
  | no_for_ty                                              {% withSpan $1 (BoundPredicate [] $1 []) }
  | no_for_ty ':' sep_by1(ty_param_bound_mod,'+')          {% withSpan $1 (BoundPredicate [] $1 (toList $3)) }
  | for_lts no_for_ty                                      {% withSpan $1 (BoundPredicate (unspan $1) $2 []) }
  | for_lts no_for_ty ':' sep_by1(ty_param_bound_mod,'+')  {% withSpan $1 (BoundPredicate (unspan $1) $2 (toList $4)) }


item_impl :: { Item Span }
  : unsafe impl generics ty_prim_sum where_clause impl_items
    {% withSpan $1 (Item (mkIdent "") (fst $>) (Impl Unsafe Positive $3{ whereClause = $5 } Nothing $4 (snd $>)) InheritedV) } 
  |        impl generics ty_prim_sum where_clause impl_items
    {% withSpan $1 (Item (mkIdent "") (fst $>) (Impl Normal Positive $2{ whereClause = $4 } Nothing $3 (snd $>)) InheritedV) } 
  | unsafe impl generics '(' ty ')'  where_clause impl_items 
    {% withSpan $1 (Item (mkIdent "") (fst $>) (Impl Unsafe Positive $3{ whereClause = $7 } Nothing $5 (snd $>)) InheritedV) } 
  |        impl generics '(' ty ')'  where_clause impl_items
    {% withSpan $1 (Item (mkIdent "") (fst $>) (Impl Normal Positive $2{ whereClause = $6 } Nothing $4 (snd $>)) InheritedV) } 
  | unsafe impl generics '!' trait_ref for ty_sum where_clause impl_items
    {% withSpan $1 (Item (mkIdent "") (fst $>) (Impl Unsafe Negative $3{ whereClause = $8 } (Just $5) $7 (snd $>)) InheritedV) }
  | unsafe impl generics     trait_ref for ty_sum where_clause impl_items
    {% withSpan $1 (Item (mkIdent "") (fst $>) (Impl Unsafe Positive $3{ whereClause = $7 } (Just $4) $6 (snd $>)) InheritedV) }
  |        impl generics '!' trait_ref for ty_sum where_clause impl_items
    {% withSpan $1 (Item (mkIdent "") (fst $>) (Impl Normal Negative $2{ whereClause = $7 } (Just $4) $6 (snd $>)) InheritedV) }
  |        impl generics     trait_ref for ty_sum where_clause impl_items
    {% withSpan $1 (Item (mkIdent "") (fst $>) (Impl Normal Positive $2{ whereClause = $6 } (Just $3) $5 (snd $>)) InheritedV) }
  | unsafe impl generics     trait_ref for '..' '{' '}'
    {% withSpan $1 (Item (mkIdent "") [] (case $3 of { Generics [] [] _ _ -> (DefaultImpl Unsafe $4); _ -> error "todo" }) InheritedV) }
  |        impl generics     trait_ref for '..' '{' '}'
    {% withSpan $1 (Item (mkIdent "") [] (case $2 of { Generics [] [] _ _ -> (DefaultImpl Normal $3); _ -> error "todo" }) InheritedV) }

impl_items :: { ([Attribute Span], [ImplItem Span]) }
  : '{'             many(impl_item) '}'  { ([], $2) }
  | '{' inner_attrs many(impl_item) '}'  { (toList $2, $3) }

impl_item :: { ImplItem Span }
  : ntImplItem                                          { $1 }
  | outer_attrs vis def type ident '=' ty ';'           {% withSpan $1 (ImplItem (unspan $5) (unspan $2) $3 (toList $1) (TypeI $7)) }
  |             vis def type ident '=' ty ';'           {% withSpan $1 (ImplItem (unspan $4) (unspan $1) $2 [] (TypeI $6)) }
  | outer_attrs vis def const ident ':' ty '=' expr ';' {% withSpan $1 (ImplItem (unspan $5) (unspan $2) $3 (toList $1) (ConstI $7 $9)) }
  |             vis def const ident ':' ty '=' expr ';' {% withSpan $1 (ImplItem (unspan $4) (unspan $1) $2 [] (ConstI $6 $8)) }
  | outer_attrs vis def mod_mac                         {% withSpan $1 (ImplItem (mkIdent "") (unspan $2) $3 (toList $1) (MacroI $4)) }
  |             vis def mod_mac                         {% withSpan $1 (ImplItem (mkIdent "") (unspan $1) $2 [] (MacroI $3)) }
  | outer_attrs vis def const safety fn ident generics fn_decl_with_self where_clause inner_attrs_block
     {% withSpan $1 (ImplItem (unspan $7) (unspan $2) $3 (toList $1 ++ fst $>) (MethodI (MethodSig $5 Const Rust $9 $8{ whereClause = $10 }) (snd $>))) }
  |             vis def const safety fn ident generics fn_decl_with_self where_clause inner_attrs_block
     {% withSpan $1 (ImplItem (unspan $6) (unspan $1) $2 (fst $>) (MethodI (MethodSig $4 Const Rust $8 $7{ whereClause = $9 }) (snd $>))) }
  | outer_attrs vis def safety ext_abi fn ident generics fn_decl_with_self where_clause inner_attrs_block
     {% withSpan $1 (ImplItem (unspan $7) (unspan $2) $3 (toList $1 ++ fst $>) (MethodI (MethodSig $4 NotConst $5 $9 $8{ whereClause = $10 }) (snd $>))) }
  |             vis def safety ext_abi fn ident generics fn_decl_with_self where_clause inner_attrs_block
     {% withSpan $1 (ImplItem (unspan $6) (unspan $1) $2 (fst $>) (MethodI (MethodSig $3 NotConst $4 $8 $7{ whereClause = $9 }) (snd $>))) }

trait_item :: { TraitItem Span }
  : ntTraitItem                                      { $1 }
  | outer_attrs const ident ':' ty_sum  '=' expr ';' {% withSpan $1 (TraitItem (unspan $3) (toList $1) (ConstT $5 (Just $7))) }
  |             const ident ':' ty_sum  '=' expr ';' {% withSpan $1 (TraitItem (unspan $2) [] (ConstT $4 (Just $6))) }
  | outer_attrs const ident ':' ty_sum           ';' {% withSpan $1 (TraitItem (unspan $3) (toList $1) (ConstT $5 Nothing)) }
  |             const ident ':' ty_sum           ';' {% withSpan $1 (TraitItem (unspan $2) [] (ConstT $4 Nothing)) }
  | outer_attrs mod_mac                              {% withSpan $1 (TraitItem (mkIdent "") (toList $1) (MacroT $2)) }
  |             mod_mac                              {% withSpan $1 (TraitItem (mkIdent "") [] (MacroT $1)) }
  | outer_attrs type ty_param ';'                    {% let TyParam _ i b d _ = $3 in withSpan $1 (TraitItem i (toList $1) (TypeT b d)) }
  |             type ty_param ';'                    {% let TyParam _ i b d _ = $2 in withSpan $1 (TraitItem i [] (TypeT b d)) }
  | outer_attrs safety ext_abi fn ident generics fn_decl_with_self where_clause ';'
     { TraitItem (unspan $5) (toList $1) (MethodT (MethodSig $2 NotConst $3 $7 $6{ whereClause = $8 }) Nothing) mempty }
  |             safety ext_abi fn ident generics fn_decl_with_self where_clause ';'
     { TraitItem (unspan $4) [] (MethodT (MethodSig $1 NotConst $2 $6 $5{ whereClause = $7 }) Nothing) mempty }
  | outer_attrs safety ext_abi fn ident generics fn_decl_with_self where_clause block
     { TraitItem (unspan $5) (toList $1) (MethodT (MethodSig $2 NotConst $3 $7 $6{ whereClause = $8 }) (Just $>)) mempty }
  |             safety ext_abi fn ident generics fn_decl_with_self where_clause block
     { TraitItem (unspan $4) [] (MethodT (MethodSig $1 NotConst $2 $6 $5{ whereClause = $7 }) (Just $>)) mempty }


safety :: { Unsafety }
  : {- empty -}     { Normal }
  | unsafe          { Unsafe }

ext_abi :: { Abi }
  : {- empty -}     { Rust }
  | extern abi      { $2 }

vis :: { Spanned (Visibility Span) }
  : {- empty -}          { pure InheritedV }
  | pub        %prec VIS {% withSpan $1 (Spanned PublicV) }
  | pub '(' crate ')'    {% withSpan $1 (Spanned CrateV) }
  | pub '(' mod_path ')' {% withSpan $1 (Spanned (RestrictedV $3)) }

def :: { Defaultness }
  : {- empty -}      %prec mut { Final }
  | default                    { Default }

view_path :: { ViewPath Span }
  : '::' sep_by1(self_or_ident,'::')                                     {% let n = fmap unspan $2 in withSpan $1 (ViewPathSimple True (N.init n) (PathListItem (N.last n) Nothing mempty)) }
  | '::' sep_by1(self_or_ident,'::') as ident                            {% let n = fmap unspan $2 in withSpan $1 (ViewPathSimple True (N.init n) (PathListItem (N.last n) (Just (unspan $>)) mempty)) }
  | '::'                                  '*'                            {% withSpan $1 (ViewPathGlob True []) }
  | '::' sep_by1(self_or_ident,'::') '::' '*'                            {% withSpan $1 (ViewPathGlob True (fmap unspan (toList $2))) }
  | '::' sep_by1(self_or_ident,'::') '::' '{'                        '}' {% withSpan $1 (ViewPathList True (map unspan (toList $2)) []) }
  | '::' sep_by1(self_or_ident,'::') '::' '{' sep_by1(plist,',')     '}' {% withSpan $1 (ViewPathList True (map unspan (toList $2)) (toList $5)) }
  | '::' sep_by1(self_or_ident,'::') '::' '{' sep_by1(plist,',') ',' '}' {% withSpan $1 (ViewPathList True (map unspan (toList $2)) (toList $5)) }
  | '::'                                  '{'                        '}' {% withSpan $1 (ViewPathList True [] []) }
  | '::'                                  '{' sep_by1(plist,',')     '}' {% withSpan $1 (ViewPathList True [] (toList $3)) }
  | '::'                                  '{' sep_by1(plist,',') ',' '}' {% withSpan $1 (ViewPathList True [] (toList $3)) }
  |      sep_by1(self_or_ident,'::')                                     {% let n = fmap unspan $1 in withSpan $1 (ViewPathSimple False (N.init n) (PathListItem (N.last n) Nothing mempty)) }
  |      sep_by1(self_or_ident,'::') as ident                            {% let n = fmap unspan $1 in withSpan $1 (ViewPathSimple False (N.init n) (PathListItem (N.last n) (Just (unspan $>)) mempty)) }
  |                                       '*'                            {% withSpan $1 (ViewPathGlob False []) }
  |      sep_by1(self_or_ident,'::') '::' '*'                            {% withSpan $1 (ViewPathGlob False (fmap unspan (toList $1))) }
  |      sep_by1(self_or_ident,'::') '::' '{'                        '}' {% withSpan $1 (ViewPathList False (map unspan (toList $1)) []) }
  |      sep_by1(self_or_ident,'::') '::' '{' sep_by1(plist,',')     '}' {% withSpan $1 (ViewPathList False (map unspan (toList $1)) (toList $4)) }
  |      sep_by1(self_or_ident,'::') '::' '{' sep_by1(plist,',') ',' '}' {% withSpan $1 (ViewPathList False (map unspan (toList $1)) (toList $4)) }
  |                                       '{'                        '}' {% withSpan $1 (ViewPathList False [] []) }
  |                                       '{' sep_by1(plist,',')     '}' {% withSpan $1 (ViewPathList False [] (toList $2)) }
  |                                       '{' sep_by1(plist,',') ',' '}' {% withSpan $1 (ViewPathList False [] (toList $2)) }


self_or_ident :: { Spanned Ident }
  : ident                   { $1 }
  | self                    {% withSpan $1 (Spanned "self") }
  | super                   {% withSpan $1 (Spanned "super") }


plist :: { PathListItem Span }
  : self_or_ident           {% withSpan $1 (PathListItem (unspan $1) Nothing) }
  | self_or_ident as ident  {% withSpan $1 (PathListItem (unspan $1) (Just (unspan $3))) }

-------------------
-- Macro related --
-------------------

expr_mac :: { Mac Span }
  : expr_path '!' '[' many(token_tree) ']'      {% withSpan $1 (Mac $1 $4) }
  | expr_path '!' '(' many(token_tree) ')'      {% withSpan $1 (Mac $1 $4) }

ty_mac :: { Mac Span }
  : ty_path '!' '[' many(token_tree) ']'        {% withSpan $1 (Mac $1 $4) }
  | ty_path '!' '{' many(token_tree) '}'        {% withSpan $1 (Mac $1 $4) }
  | ty_path '!' '(' many(token_tree) ')'        {% withSpan $1 (Mac $1 $4) }

mod_mac :: { Mac Span }
  : mod_path '!' '[' many(token_tree) ']' ';'  {% withSpan $1 (Mac $1 $4) }
  | mod_path '!' '{' many(token_tree) '}'      {% withSpan $1 (Mac $1 $4) }
  | mod_path '!' '(' many(token_tree) ')' ';'  {% withSpan $1 (Mac $1 $4) }

token_tree :: { TokenTree }
  : ntTT                     { $1 }
  -- # Delimited
  | '(' many(token_tree) ')'                              { Delimited mempty Paren mempty $2 mempty }
  | '{' many(token_tree) '}'                              { Delimited mempty Brace mempty $2 mempty }
  | '[' many(token_tree) ']'                              { Delimited mempty Bracket mempty $2 mempty }
  -- # Sequence
  | '$' '(' many(token_tree) ')' token_not_plus_star '+'  { Sequence mempty $3 (Just (unspan $5)) OneOrMore } 
  | '$' '(' many(token_tree) ')' token_not_plus_star '*'  { Sequence mempty $3 (Just (unspan $5)) ZeroOrMore }
  | '$' '(' many(token_tree) ')' '+'                      { Sequence mempty $3 Nothing OneOrMore }
  | '$' '(' many(token_tree) ')' '*'                      { Sequence mempty $3 Nothing ZeroOrMore }
  -- # Token
  -- Expression-operator symbols. 
  | token_not_plus_star                                   { mkTokenTree $1 }
  | '+'                                                   { mkTokenTree $1 }
  | '*'                                                   { mkTokenTree $1 }

token_not_plus_star :: { Spanned Token }
  : '='        { $1 }
  | '<'        { $1 }
  | '>'        { $1 }
  | '!'        { $1 }
  | '~'        { $1 }
  | '-'        { $1 }
  | '/'        { $1 }
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
  -- Macro related
  | substNt    { $1 }
  | matchNt    { $1 }


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

-- | Parser for crates.
parseCrate :: P (Crate Span)

-- | Parser for blocks.
parseBlock :: P (Block Span)

-- | Parser for @impl@ items.
parseImplItem :: P (ImplItem Span)

-- | Parser for @trait@ items.
parseTraitItem :: P (TraitItem Span)

-- | Parser for token trees.
parseTt :: P TokenTree


-- | Try to convert an expression to a statement given information about whether there is a trailing
-- semicolon
toStmt :: Expr Span -> Bool -> P (Stmt Span)
toStmt (MacExpr a m s) hasSemi = withSpan s (MacStmt m (if hasSemi then SemicolonMac else BracesMac) a)
toStmt e hasSemi = withSpan e ((if hasSemi then Semi else NoSemi) e)

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


-- | Given a spanned token, convert it to a token tree. Basically just move the Span
mkTokenTree :: Spanned Token -> TokenTree
mkTokenTree (Spanned t s) = Token s t

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

isPathSegmentIdent :: Spanned Ident -> Bool
isPathSegmentIdent i = True

isTypePathSegmentIdent :: Spanned Ident -> Bool
isTypePathSegmentIdent i = True

-- | Check if a given string is one of the accepted ABIs
isAbi :: String -> Bool
isAbi s = s `elem` abis
  where abis :: [String]
        abis = [ "Cdecl", "Stdcall", "Fastcall", "Vectorcall", "Aapcs", "Win64", "SysV64"
               , "Rust", "C", "System", "RustIntrinsic", "RustCall", "PlatformIntrinsic"
               ]

isTraitTyParamBound TraitTyParamBound{} = True
isTraitTyParamBound _ = False

-- | The second argument is the thing you are trying to add a 'Span' to. The first argument is
-- the first constituent of the thing we are annotating - it is passed in so that we can extract the
-- start of the 'Span'. The end of the 'Span' is determined from the current parser position.
withSpan :: Located node => node -> (Span -> a) -> P a
withSpan node mkNode = do
  let Span lo _ = spanOf node
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
