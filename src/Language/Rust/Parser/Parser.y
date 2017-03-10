{
module Language.Rust.Parser.Parser (
  attributeP, typeP, literalP, patternP, expressionP, stmtP, itemP
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
%name itemP item

%tokentype { Spanned Token }

%monad { P } { >>= } { return }
%error { parseError }
%lexer { lexNonSpace >>= } { Spanned Eof _ }

-- Conflicts caused in
--  * different expressions that all start with an 'expr_path'
--  * '..' and '...' both can have an expression after them (x2)
--  * something around empty returns
--  * complex_expr_path
--  * different types that all start with 'ty_path'
--  * sep_by1(segment,'::') parts of paths
--  * field accesses are prefixes of method calls (all 3 types of exprs, so x3)
--  * deciding between expression paths and struct expressions
-- However, they are all S/R and seem to be currently doing what they should
%expect 16

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
  as             { Identifier "as" }
  box            { Identifier "box" } 
  break          { Identifier "break" } 
  const          { Identifier "const" } 
  continue       { Identifier "continue" }
  crate          { Identifier "crate" } 
  else           { Identifier "else" }
  enum           { Identifier "enum" }
  extern         { Identifier "extern" }
  false          { Identifier "false" } 
  fn             { Identifier "fn" }
  for            { Identifier "for" } 
  if             { Identifier "if" }
  impl           { Identifier "impl" }
  in             { Identifier "in" }
  let            { Identifier "let" } 
  loop           { Identifier "loop" }
  match          { Identifier "match" } 
  mod            { Identifier "mod" } 
  move           { Identifier "move" }
  mut            { Identifier "mut" } 
  pub            { Identifier "pub" } 
  ref            { Identifier "ref" } 
  return         { Identifier "return" }
  Self           { Identifier "Self" }
  self           { Identifier "self" } 
  static         { Identifier "static" }
  struct         { Identifier "struct" }
  super          { Identifier "super" } 
  trait          { Identifier "trait" } 
  true           { Identifier "true" }
  type           { Identifier "type" }
  unsafe         { Identifier "unsafe" }
  use            { Identifier "use" } 
  where          { Identifier "where" } 
  while          { Identifier "while" } 
  
  -- Keywords reserved for future use
  abstract       { Identifier "abstract" }
  alignof        { Identifier "alignof" } 
  become         { Identifier "become" }
  do             { Identifier "do" }
  final          { Identifier "final" } 
  macro          { Identifier "macro" } 
  offsetof       { Identifier "offsetof" }
  override       { Identifier "override" }
  priv           { Identifier "priv" }
  proc           { Identifier "proc" }
  pure           { Identifier "pure" }
  sizeof         { Identifier "sizeof" }
  typeof         { Identifier "typeof" }
  unsized        { Identifier "unsized" } 
  virtual        { Identifier "virtual" } 
  yield          { Identifier "yield" } 

  -- Weak keywords, have special meaning only in specific contexts.
  default        { Identifier "default" } 
  union          { Identifier "union" } 

  -- Comments
  outerDoc       { Spanned (Doc _ OuterDoc) _ }
  innerDoc       { Spanned (Doc _ InnerDoc) _ }

  -- Identifiers.
  IDENT          { Identifier _ }
  '_'            { Spanned Underscore _ }

  -- Lifetimes.
  LIFETIME       { Spanned (LifetimeTok _) _ }

  -- macro related
  substNt        { Spanned (SubstNt _ _) _ }
  matchNt        { Spanned (MatchNt _ _ _ _) _ }

  -- Interpolated
  ntItem         { Spanned (Interpolated (NtItem $$)) _ }
  ntBlock        { Spanned (Interpolated (NtBlock $$)) _ }
  ntStmt         { Spanned (Interpolated (NtStmt $$)) _ }
  ntPat          { Spanned (Interpolated (NtPat $$)) _ }
  ntExpr         { Spanned (Interpolated (NtExpr $$)) _ }
  ntTy           { Spanned (Interpolated (NtTy $$)) _ }
  ntIdent        { Spanned (Interpolated (NtIdent $$)) _ }
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
%nonassoc IDENT

%%

-- Unwraps the IdentTok into just an Ident
ident :: { Spanned Ident }
  : ntIdent                       { Spanned $1 mempty }
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
  : some_r(p)          { N.reverse $1 }
some_r(p) :: { NonEmpty a }
  : some_r(p) p        { $2 <| $1 }
  | p                  { $1 :| [] }

-- | Zero or more
many(p) :: { [a] }
  : some(p)            { toList $1 }
  | {- empty -}        { [] }


-- | One or more occurences of p, seperated by sep
-- TODO: Use the commented out implementation (and understand why it currently makes more conlifcts)
{-
sep_by1(p,sep)  : sep_by1_r(p,sep)   { N.reverse $1 }
sep_by1_r(p,s)  : sep_by1_r(p,s) s p { $3 <| $1 }
                | p                  { $1 :| [] }
-}
sep_by1(p,sep) :: { NonEmpty a }
  : sep_by1(p,sep) sep p  { $1 |> $3 }
  | p                     { $1 :| [] }


-- | Zero or more occurrences of p, separated by sep
sep_by(p,sep) :: { [a] }
  : sep_by1(p,sep)     { toList $1 }
  | {- empty -}        { [] }


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
  : ntMeta                                          { $1 }
  | ident                                           {% withSpan $1 (Word (unspan $1)) }
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
qual_path(segs) :: { Spanned (QSelf Span, Path Span) }
  : '<' qual_path_suf(segs)                                                        { $2 }
  | '<<' ty_qual_path_suf as ty_path '>' '::' segs                                 {%
      let segs = segments $4 <> unspan $7
      in withSpan $1 (Spanned (QSelf $2 (length (segments $4)), $4{ segments = segs }))
    }

-- Basically a qualified path, but ignoring the very first '>' token
qual_path_suf(segs) :: { Spanned (QSelf Span, Path Span) }
  : ty_sum '>' '::' segs                {% withSpan $1 (Spanned (QSelf $1 0, Path False (unspan $4) (posOf $4))) }
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
  | '(' sep_by(ty_sum,',') ')'                {% withSpan $1 (Parenthesized $2 Nothing) }
  | '(' sep_by1(ty_sum,',') ',' ')'           {% withSpan $1 (Parenthesized (toList $2) Nothing) }
  | '(' sep_by(ty_sum,',') ')' '->' ty        {% withSpan $1 (Parenthesized $2 (Just $>)) }
  | '(' sep_by1(ty_sum,',') ',' ')' '->' ty   {% withSpan $1 (Parenthesized (toList $2) (Just $>)) }
  | {- empty -}                               { AngleBracketed [] [] [] mempty }


-- Expression related:
-- parse_path(PathStyle::Expr)
expr_path :: { Path Span }
  : ntPath                                  { $1 }
  | path_segments_with_colons               {% withSpan $1 (Path False (unspan $1)) }
  | '::' path_segments_with_colons          {% withSpan $1 (Path True (unspan $2)) }

-- As expr_path, but disallowing one IDENT paths
-- TODO: this duplicates a bunch of stuff
-- TODO: abstract function to make path out into code block at bottom of file
complex_expr_path :: { Path Span }
  : ident '::' generic_values                                      
      {% if isPathSegmentIdent $1
           then let (lts, tys, bds) = $3
                in withSpan $1 (Path False (fromList [(unspan $1, AngleBracketed lts tys bds mempty)]))
           else fail "invalid path segment in expression path" }
  | ident '::' path_segments_with_colons                              
      {% withSpan $1 (Path False ((unspan $1, NoParameters mempty) <| unspan $3)) }
  | ident '::' generic_values '::' path_segments_with_colons
      {% let (lts, tys, bds) = $3 in withSpan $1 (Path False ((unspan $1, AngleBracketed lts tys bds mempty) <| unspan $5)) }
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
  : generic_values         { Left $1 }
  | ident                  { Right (unspan $1) }


-- Mod related:
-- parse_path(PathStyle::Mod)
mod_path :: { Path Span }
  : ntPath                                   { $1 }
  | path_segments_without_types              { $1 }

path_segments_without_types :: { Path Span  }
  : path_segment_without_types               {% withSpan $1 (Path False (unspan $1 :| [])) }
  | '::' path_segment_without_types          {% withSpan $1 (Path True (unspan $2 :| [])) }
  | mod_path '::' path_segment_without_types {% withSpan $1 (Path (global $1) (segments $1 |> unspan $3)) }

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
  : ntTy                             { $1 }
  | no_for_ty                        { $1 }
  | for_ty                           { $1 }

ty_prim :: { Ty Span }
  : no_for_ty_prim                   { $1 }
  | for_ty                           { $1 }

no_for_ty :: { Ty Span }
  : no_for_ty_prim                   { $1 }
  | '(' ')'                          {% withSpan $1 (TupTy []) }
  | '(' ty_sum ')'                   {% withSpan $1 (ParenTy $2) }
  | '(' ty_sum ',' ')'               {% withSpan $1 (TupTy [$2]) }
  | '(' ty_sum ',' sep_by1(ty_sum,',') ')' {% withSpan $1 (TupTy ($2 : toList $4)) }
  | ty_qual_path                     {% withSpan $1 (PathTy (Just (fst (unspan $1))) (snd (unspan $1))) }

no_for_ty_prim :: { Ty Span }
  : '_'                              {% withSpan $1 Infer }
  | '!'                              {% withSpan $1 Never }
  | '[' ty ']'                       {% withSpan $1 (Slice $2) }
  | '*' ty                           {% withSpan $1 (Ptr Immutable $2) }
  | '*' const ty                     {% withSpan $1 (Ptr Immutable $3) }
  | '*' mut ty                       {% withSpan $1 (Ptr Mutable $3) }
  | '&' ty                           {% withSpan $1 (Rptr Nothing Immutable $2) }
  | '&' mut ty                       {% withSpan $1 (Rptr Nothing Mutable $3) }
  | '&' lifetime ty                  {% withSpan $1 (Rptr (Just $2) Immutable $3) }
  | '&' lifetime mut ty              {% withSpan $1 (Rptr (Just $2) Mutable $4) }
  | '&&' ty                          {% withSpan $1 (Rptr Nothing Immutable (Rptr Nothing Immutable $2 mempty)) }
  | '&&' mut ty                      {% withSpan $1 (Rptr Nothing Immutable (Rptr Nothing Mutable $3 mempty)) }
  | '&&' lifetime ty                 {% withSpan $1 (Rptr Nothing Immutable (Rptr (Just $2) Immutable $3 mempty)) }
  | '&&' lifetime mut ty             {% withSpan $1 (Rptr Nothing Immutable (Rptr (Just $2) Mutable $4 mempty)) }
  | ty_path                          {% withSpan $1 (PathTy Nothing $1) }
  | ty_mac                           {% withSpan $1 (MacTy $1) } 
  | unsafe extern abi fn fn_decl     {% withSpan $1 (BareFn Unsafe $3 [] $>) }
  | unsafe fn fn_decl                {% withSpan $1 (BareFn Unsafe Rust [] $>) }
  | extern abi fn fn_decl            {% withSpan $1 (BareFn Normal $2 [] $>) }
  | fn fn_decl                       {% withSpan $1 (BareFn Normal Rust [] $>) }
  | typeof '(' expr ')'              {% withSpan $1 (Typeof $3) }
  | '[' ty ';' expr ']'              {% withSpan $1 (Array $2 $4) }
  | Self                             {% withSpan $1 ImplicitSelf }

for_ty :: { Ty Span }
  : for_lts unsafe extern abi fn fn_decl {% withSpan $1 (BareFn Unsafe $4 (unspan $1) $>) }
  | for_lts unsafe fn fn_decl            {% withSpan $1 (BareFn Unsafe Rust (unspan $1) $>) }
  | for_lts extern abi fn fn_decl        {% withSpan $1 (BareFn Normal $3 (unspan $1) $>) }
  | for_lts fn fn_decl                   {% withSpan $1 (BareFn Normal Rust (unspan $1) $>) }
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
  : ty                                        { $1 }
  | ty '+' sep_by1(ty_param_bound,'+')        {% withSpan $1 (ObjectSum $1 (toList $3)) }

ty_prim_sum :: { Ty Span }
  : ty_prim                                   { $1 }
  | ty_prim '+' sep_by1(ty_param_bound,'+')   {% withSpan $1 (ObjectSum $1 (toList $3)) }


-- The argument list and return type in a function
fn_decl :: { FnDecl Span }
  : '(' sep_by1(arg_general,',') ',' '...' ')' ret_ty  {% withSpan $1 (FnDecl (toList $2) $> True) }
  | '(' sep_by1(arg_general,',') ',' ')' ret_ty        {% withSpan $1 (FnDecl (toList $2) $> False) }
  | '(' sep_by(arg_general,',') ')' ret_ty             {% withSpan $1 (FnDecl $2 $> False) }


fn_decl_with_self :: { FnDecl Span }
  : '(' arg_self ',' sep_by1(arg_general,',') ',' ')' ret_ty   {% withSpan $1 (FnDecl ($2 : toList $4) $> False) }
  | '(' arg_self ',' sep_by1(arg_general,',') ')' ret_ty       {% withSpan $1 (FnDecl ($2 : toList $4) $> False) } 
  | '(' arg_self ',' ')' ret_ty                                {% withSpan $1 (FnDecl [$2] $> False) }
  | '(' arg_self ')' ret_ty                                    {% withSpan $1 (FnDecl [$2] $> False) }
  | fn_decl                                                    { $1 }

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
  : ty_sum                {% withSpan $1 (Arg Nothing $1) }
  | ident ':' ty_sum      {% withSpan $1 (Arg (Just (IdentP (ByValue Immutable) (unspan $1) Nothing mempty)) $3) }
  | '_'   ':' ty_sum      {% withSpan $1 (Arg (Just (WildP mempty)) $3) }

arg_self :: { Arg Span }
  : self                  {% withSpan $1 (SelfValue Immutable) }
  | mut self              {% withSpan $1 (SelfValue Mutable) }
  | '&' self              {% withSpan $1 (SelfRegion Nothing Immutable) }
  | '&' lifetime self     {% withSpan $1 (SelfRegion (Just $2) Immutable) }
  | '&' mut self          {% withSpan $1 (SelfRegion Nothing Mutable) }
  | '&' lifetime mut self {% withSpan $1 (SelfRegion (Just $2) Mutable) }
  | self ':' ty_sum       {% withSpan $1 (SelfExplicit $3 Immutable) }
  | mut self ':' ty_sum   {% withSpan $1 (SelfExplicit $4 Mutable) }


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
  : ntPat                                       { $1 }
  | '_'                                         {% withSpan $1 WildP }
  | '&' mut pat                                 {% withSpan $1 (RefP $3 Mutable) }
  | '&' pat                                     {% withSpan $1 (RefP $2 Immutable) }
  | '&&' mut pat                                {% withSpan $1 (RefP (RefP $3 Mutable mempty) Immutable) }
  | '&&' pat                                    {% withSpan $1 (RefP (RefP $2 Immutable mempty) Immutable) }
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


-- General expressions, not restrictions
expr          :: { Expr Span }
              : ntExpr                                          { $1 }
              | gen_expr                                        { $1 }
              | binary0_expr                                    { $1 }
              | lambda_expr                                     { $1 }
binary0_expr  :: { Expr Span }
              : gen_binary0_expr(binary1_expr,binary0_expr)     { $1 }
              | binary1_expr                                    { $1 }
binary1_expr  :: { Expr Span }
              : gen_binary1_expr(binary2_expr,binary1_expr)     { $1 } 
              | binary2_expr                                    { $1 } 
binary2_expr  :: { Expr Span }
              : gen_binary2_expr(binary3_expr,binary3_expr)     { $1 } 
              | binary3_expr                                    { $1 } 
binary3_expr  :: { Expr Span }
              : gen_binary3_expr(binary3_expr,binary4_expr)     { $1 } 
              | binary4_expr                                    { $1 } 
binary4_expr  :: { Expr Span }
              : gen_binary4_expr(binary4_expr,binary5_expr)     { $1 } 
              | binary5_expr                                    { $1 } 
binary5_expr  :: { Expr Span }
              : gen_binary5_expr(binary5_expr,binary6_expr)     { $1 } 
              | binary6_expr                                    { $1 } 
binary6_expr  :: { Expr Span }
              : gen_binary6_expr(binary6_expr,binary7_expr)     { $1 } 
              | binary7_expr                                    { $1 } 
binary7_expr  :: { Expr Span }
              : gen_binary7_expr(binary7_expr,binary8_expr)     { $1 } 
              | binary8_expr                                    { $1 } 
binary8_expr  :: { Expr Span }
              : gen_binary8_expr(binary8_expr,binary9_expr)     { $1 } 
              | binary9_expr                                    { $1 } 
binary9_expr  :: { Expr Span }
              : gen_binary9_expr(binary9_expr,binary10_expr)     { $1 } 
              | binary10_expr                                   { $1 } 
binary10_expr :: { Expr Span }
              : gen_binary10_expr(binary10_expr,binary11_expr)  { $1 } 
              | binary11_expr                                   { $1 } 
binary11_expr :: { Expr Span }
              : gen_binary11_expr(binary11_expr,binary12_expr)  { $1 } 
              | binary12_expr                                   { $1 } 
binary12_expr :: { Expr Span }
              : gen_binary12_expr(binary12_expr)                { $1 } 
              | prefix_expr                                     { $1 } 
prefix_expr   :: { Expr Span }
              : gen_prefix_expr(prefix_expr)                    { $1 } 
              | postfix_expr                                    { $1 } 
postfix_expr  :: { Expr Span }
              : gen_postfix_expr(postfix_expr)                  { $1 } 
              | paren_expr                                      { $1 }
              | struct_expr                                     { $1 }
              | block_expr                                      { $1 }


-- General expressions, but no structs
nostruct_expr    :: { Expr Span }
                 : ntExpr                                                { $1 }
                 | gen_expr                                              { $1 }
                 | ns_binary0_expr                                       { $1 }
                 | lambda_expr_nostruct                                  { $1 }
ns_binary0_expr  :: { Expr Span }
                 : gen_binary0_expr(ns_binary1_expr,ns_binary0_expr)     { $1 }
                 | ns_binary1_expr                                       { $1 }
ns_binary1_expr  :: { Expr Span }
                 : gen_binary1_expr(ns_binary2_expr,ns_binary1_expr)     { $1 } 
                 | ns_binary2_expr                                       { $1 } 
ns_binary2_expr  :: { Expr Span }
                 : gen_binary2_expr(ns_binary3_expr,ns_binary3_expr)     { $1 } 
                 | ns_binary3_expr                                       { $1 } 
ns_binary3_expr  :: { Expr Span }
                 : gen_binary3_expr(ns_binary3_expr,ns_binary4_expr)     { $1 } 
                 | ns_binary4_expr                                       { $1 } 
ns_binary4_expr  :: { Expr Span }
                 : gen_binary4_expr(ns_binary4_expr,ns_binary5_expr)     { $1 } 
                 | ns_binary5_expr                                       { $1 } 
ns_binary5_expr  :: { Expr Span }
                 : gen_binary5_expr(ns_binary5_expr,ns_binary6_expr)     { $1 } 
                 | ns_binary6_expr                                       { $1 } 
ns_binary6_expr  :: { Expr Span }
                 : gen_binary6_expr(ns_binary6_expr,ns_binary7_expr)     { $1 } 
                 | ns_binary7_expr                                       { $1 } 
ns_binary7_expr  :: { Expr Span }
                 : gen_binary7_expr(ns_binary7_expr,ns_binary8_expr)     { $1 } 
                 | ns_binary8_expr                                       { $1 } 
ns_binary8_expr  :: { Expr Span }
                 : gen_binary8_expr(ns_binary8_expr,ns_binary9_expr)     { $1 } 
                 | ns_binary9_expr                                       { $1 } 
ns_binary9_expr  :: { Expr Span }
                 : gen_binary9_expr(ns_binary9_expr,ns_binary10_expr)     { $1 } 
                 | ns_binary10_expr                                      { $1 } 
ns_binary10_expr :: { Expr Span }
                 : gen_binary10_expr(ns_binary10_expr,ns_binary11_expr)  { $1 } 
                 | ns_binary11_expr                                      { $1 } 
ns_binary11_expr :: { Expr Span }
                 : gen_binary11_expr(ns_binary11_expr,ns_binary12_expr)  { $1 } 
                 | ns_binary12_expr                                      { $1 } 
ns_binary12_expr :: { Expr Span }
                 : gen_binary12_expr(ns_binary12_expr)                   { $1 } 
                 | ns_prefix_expr                                        { $1 } 
ns_prefix_expr   :: { Expr Span }
                 : gen_prefix_expr(ns_prefix_expr)                       { $1 } 
                 | ns_postfix_expr                                       { $1 } 
ns_postfix_expr  :: { Expr Span }
                 : gen_postfix_expr(ns_postfix_expr)                     { $1 } 
                 | paren_expr                                            { $1 }
                 | block_expr                                            { $1 }


-- General expressions, but no blocks on the left
nonblock_expr    :: { Expr Span }
                 : ntExpr                                                { $1 }
                 | gen_expr                                              { $1 }
                 | nb_binary0_expr                                       { $1 }
                 | lambda_expr_nostruct                                  { $1 }
nb_binary0_expr  :: { Expr Span }
                 : gen_binary0_expr(nb_binary1_expr,binary0_expr)        { $1 }
                 | nb_binary1_expr                                       { $1 }
nb_binary1_expr  :: { Expr Span } 
                 : gen_binary1_expr(nb_binary2_expr,binary1_expr)        { $1 } 
                 | nb_binary2_expr                                       { $1 } 
nb_binary2_expr  :: { Expr Span }
                 : gen_binary2_expr(nb_binary3_expr,binary3_expr)        { $1 } 
                 | nb_binary3_expr                                       { $1 } 
nb_binary3_expr  :: { Expr Span }
                 : gen_binary3_expr(nb_binary3_expr,binary4_expr)        { $1 } 
                 | nb_binary4_expr                                       { $1 } 
nb_binary4_expr  :: { Expr Span }
                 : gen_binary4_expr(nb_binary4_expr,binary5_expr)        { $1 } 
                 | nb_binary5_expr                                       { $1 } 
nb_binary5_expr  :: { Expr Span }
                 : gen_binary5_expr(nb_binary5_expr,binary6_expr)        { $1 } 
                 | nb_binary6_expr                                       { $1 } 
nb_binary6_expr  :: { Expr Span }
                 : gen_binary6_expr(nb_binary6_expr,binary7_expr)        { $1 } 
                 | nb_binary7_expr                                       { $1 } 
nb_binary7_expr  :: { Expr Span }
                 : gen_binary7_expr(nb_binary7_expr,binary8_expr)        { $1 } 
                 | nb_binary8_expr                                       { $1 } 
nb_binary8_expr  :: { Expr Span }
                 : gen_binary8_expr(nb_binary8_expr,binary9_expr)        { $1 } 
                 | nb_binary9_expr                                       { $1 } 
nb_binary9_expr  :: { Expr Span }
                 : gen_binary9_expr(nb_binary9_expr,binary10_expr)        { $1 } 
                 | nb_binary10_expr                                      { $1 } 
nb_binary10_expr :: { Expr Span }
                 : gen_binary10_expr(nb_binary10_expr,binary11_expr)     { $1 } 
                 | nb_binary11_expr                                      { $1 } 
nb_binary11_expr :: { Expr Span }
                 : gen_binary11_expr(nb_binary11_expr,binary12_expr)     { $1 } 
                 | nb_binary12_expr                                      { $1 } 
nb_binary12_expr :: { Expr Span }
                 : gen_binary12_expr(nb_binary12_expr)                   { $1 } 
                 | nb_prefix_expr                                        { $1 } 
nb_prefix_expr   :: { Expr Span }
                 : gen_prefix_expr(nb_prefix_expr)                       { $1 } 
                 | nb_postfix_expr                                       { $1 } 
nb_postfix_expr  :: { Expr Span }
                 : gen_postfix_expr(nb_postfix_expr)                     { $1 }
                 | paren_expr                                            { $1 }
                 | struct_expr                                           { $1 }



-- "There is a convenience rule that allows one to omit the separating ; after if, match, loop, for, while"
block_expr :: { Expr Span }
  : if_expr                                                        { $1 }
  |              loop                            safe_block        {% withSpan $1 (Loop [] $2 Nothing) }
  | lifetime ':' loop                            safe_block        {% withSpan $1 (Loop [] $4 (Just $1)) }
  |              for pat in nostruct_expr        safe_block        {% withSpan $1 (ForLoop [] $2 $4 $5 Nothing) }
  | lifetime ':' for pat in nostruct_expr        safe_block        {% withSpan $1 (ForLoop [] $4 $6 $7 (Just $1)) }
  |              while             nostruct_expr safe_block        {% withSpan $1 (While [] $2 $3 Nothing) }
  | lifetime ':' while             nostruct_expr safe_block        {% withSpan $1 (While [] $4 $5 (Just $1)) }
  |              while let pat '=' nostruct_expr safe_block        {% withSpan $1 (WhileLet [] $3 $5 $6 Nothing) }
  | lifetime ':' while let pat '=' nostruct_expr safe_block        {% withSpan $1 (WhileLet [] $5 $7 $8 (Just $1)) }
  | match nostruct_expr '{' '}'                                    {% withSpan $1 (Match [] $2 []) }
  | match nostruct_expr '{' arms '}'                               {% withSpan $1 (Match [] $2 $4) }
  | expr_path '!' '{' many(token_tree) '}'                         {% withSpan $1 (MacExpr [] (Mac $1 $4 mempty)) }
  | block                                                          {% withSpan $1 (BlockExpr [] $1) }

paren_expr :: { Expr Span }
  : '(' ')'                                {% withSpan $1 (TupExpr [] []) }
  | '(' expr ')'                           {% withSpan $1 (ParenExpr [] $2) }
  | '(' expr ',' ')'                       {% withSpan $1 (TupExpr [] [$2]) }
  | '(' expr ',' sep_by1(expr,',') ')'     {% withSpan $1 (TupExpr [] ($2 : toList $4)) }

-- General postfix expression
gen_postfix_expr(lhs) :: { Expr Span }
  : lit_expr                                                                    { $1 }
  | expr_path                                                                   {% withSpan $1 (PathExpr [] Nothing $1) }
  | expr_qual_path                                                              {% withSpan $1 (PathExpr [] (Just (fst (unspan $1))) (snd (unspan $1))) }
  | expr_mac                                                                    {% withSpan $1 (MacExpr [] $1) }
  | lhs '[' expr ']'                                                            {% withSpan $1 (Index [] $1 $3) }
  | lhs '?'                                                                     {% withSpan $1 (Try [] $1) }
  | lhs '(' ')'                                                                 {% withSpan $1 (Call [] $1 []) }
  | lhs '(' sep_by1(expr,',') ')'                                               {% withSpan $1 (Call [] $1 (toList $3)) }
  | lhs '(' sep_by1(expr,',') ',' ')'                                           {% withSpan $1 (Call [] $1 (toList $3)) }
  | lhs '.' ident '(' ')'                                                       {% withSpan $1 (MethodCall [] (unspan $3) [] ($1 :| [])) }
  | lhs '.' ident '(' sep_by1(expr,',') ')'                                     {% withSpan $1 (MethodCall [] (unspan $3) [] ($1 <| $5)) }
  | lhs '.' ident '(' sep_by1(expr,',') ',' ')'                                 {% withSpan $1 (MethodCall [] (unspan $3) [] ($1 <| $5)) }
  | lhs '.' ident '::' '<' sep_by(ty_sum,',') '>' '(' ')'                       {% withSpan $1 (MethodCall [] (unspan $3) $6 ($1 :| [])) }
  | lhs '.' ident '::' '<' sep_by(ty_sum,',') '>' '(' sep_by1(expr,',') ')'     {% withSpan $1 (MethodCall [] (unspan $3) $6 ($1 <| $9)) }
  | lhs '.' ident '::' '<' sep_by(ty_sum,',') '>' '(' sep_by1(expr,',') ',' ')' {% withSpan $1 (MethodCall [] (unspan $3) $6 ($1 <| $9)) }
  | lhs '.' ident                                                               {% withSpan $1 (FieldAccess [] $1 (unspan $3)) }
  | lhs '.' int                                                                 {%
      case lit $3 of
        Int i Unsuffixed _ -> withSpan $1 (TupField [] $1 (fromIntegral i))
        _ -> fail "make better error message"
    }

-- General prefix expression
gen_prefix_expr(lhs) :: { Expr Span }
  : '*' lhs                                {% withSpan $1 (Unary [] Deref $2) }
  | '!' lhs                                {% withSpan $1 (Unary [] Not $2) }
  | '-' lhs                                {% withSpan $1 (Unary [] Neg $2) }
  | '&' lhs                                {% withSpan $1 (AddrOf [] Immutable $2) }
  | '&' mut lhs                            {% withSpan $1 (AddrOf [] Mutable $3) }
  | '&&' lhs                               {% withSpan $1 (AddrOf [] Immutable (AddrOf [] Immutable $2 mempty)) }
  | '&&' mut lhs                           {% withSpan $1 (AddrOf [] Immutable (AddrOf [] Mutable $3 mempty)) }
  | box lhs                                {% withSpan $1 (Box [] $2) }

-- General binary expression
-- (Although this isn't really a binary operation)
gen_binary12_expr(lhs) :: { Expr Span }
  : lhs ':' ty                             {% withSpan $1 (TypeAscription [] $1 $3) }
  | lhs as ty                              {% withSpan $1 (Cast [] $1 $3) }

-- should be left associative
gen_binary11_expr(lhs,rhs) :: { Expr Span }
  : lhs '*' rhs                            {% withSpan $1 (Binary [] MulOp $1 $3) }
  | lhs '/' rhs                            {% withSpan $1 (Binary [] DivOp $1 $3) }
  | lhs '%' rhs                            {% withSpan $1 (Binary [] RemOp $1 $3) }

-- should be left associative
gen_binary10_expr(lhs,rhs) :: { Expr Span }
  : lhs '+' rhs                            {% withSpan $1 (Binary [] AddOp $1 $3) }
  | lhs '-' rhs                            {% withSpan $1 (Binary [] SubOp $1 $3) }

-- should be left associative
gen_binary9_expr(lhs,rhs) :: { Expr Span }
  : lhs '<<' rhs                           {% withSpan $1 (Binary [] ShlOp $1 $3) }
  | lhs '>>' rhs                           {% withSpan $1 (Binary [] ShrOp $1 $3) }

-- should be left associative
gen_binary8_expr(lhs,rhs) :: { Expr Span }
  : lhs '&' rhs                            {% withSpan $1 (Binary [] BitAndOp $1 $3) }

-- should be left associative
gen_binary7_expr(lhs,rhs) :: { Expr Span }
  : lhs '^' rhs                            {% withSpan $1 (Binary [] BitXorOp $1 $3) }

-- should be left associative
gen_binary6_expr(lhs,rhs) :: { Expr Span }
  : lhs '|' rhs                            {% withSpan $1 (Binary [] BitOrOp $1 $3) }

-- should be left associative
gen_binary5_expr(lhs,rhs) :: { Expr Span }
  : lhs '==' rhs                           {% withSpan $1 (Binary [] EqOp $1 $3) }
  | lhs '!=' rhs                           {% withSpan $1 (Binary [] NeOp $1 $3) }
  | lhs '<'  rhs                           {% withSpan $1 (Binary [] LtOp $1 $3) }
  | lhs '>'  rhs                           {% withSpan $1 (Binary [] GtOp $1 $3) }
  | lhs '<=' rhs                           {% withSpan $1 (Binary [] LeOp $1 $3) }
  | lhs '>=' rhs                           {% withSpan $1 (Binary [] GeOp $1 $3) }

-- should be left associative
gen_binary4_expr(lhs,rhs) :: { Expr Span }
  : lhs '&&' rhs                           {% withSpan $1 (Binary [] AndOp $1 $3) }

-- should be left associative
gen_binary3_expr(lhs,rhs) :: { Expr Span }
  : lhs '||' rhs                           {% withSpan $1 (Binary [] OrOp $1 $3) }

-- should be nonassoc
-- TODO omitting lhs or rhs
gen_binary2_expr(lhs,rhs) :: { Expr Span }
  : lhs '..' rhs                           {% withSpan $1 (Range [] (Just $1) (Just $3) Closed) }
  | lhs '...' rhs                          {% withSpan $1 (Range [] (Just $1) (Just $3) HalfOpen) }

-- should be right associative
gen_binary1_expr(lhs,rhs) :: { Expr Span }
  : lhs '<-' rhs                           {% withSpan $1 (InPlace [] $1 $3) }

-- should be right associative
gen_binary0_expr(lhs,rhs) :: { Expr Span }
  : lhs '=' rhs                            {% withSpan $1 (Assign   [] $1 $3) }
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

-- Lowest precedence generalized expression
gen_expr :: { Expr Span }
  : return                                 {% withSpan $1 (Ret [] Nothing) }
  | return expr                            {% withSpan $1 (Ret [] (Just $2)) }
  | '..'                                   {% withSpan $1 (Range [] Nothing Nothing Closed) }
  | '...'                                  {% withSpan $1 (Range [] Nothing Nothing HalfOpen) }
  | '..' expr                              {% withSpan $1 (Range [] Nothing (Just $2) Closed) }
  | '...' expr                             {% withSpan $1 (Range [] Nothing (Just $2) HalfOpen) }
  | continue                               {% withSpan $1 (Continue [] Nothing) }
  | continue lifetime                      {% withSpan $1 (Continue [] (Just $2)) }
  | break                                  {% withSpan $1 (Break [] Nothing) }
  | break lifetime                         {% withSpan $1 (Break [] (Just $2)) }
  | '[' ']'                                {% withSpan $1 (Vec [] []) }
  | '[' sep_by1(expr,',') ']'              {% withSpan $1 (Vec [] (toList $2)) }
  | '[' sep_by1(expr,',') ',' ']'          {% withSpan $1 (Vec [] (toList $2)) }
  | '[' expr ';' expr ']'                  {% withSpan $1 (Repeat [] $2 $4) }



-- Match arms usually have to be seperated by commas (with an optional comma at the end). This
-- condition is loosened (so that there is no seperator needed) if the arm ends in a safe block.
arms :: { [Arm Span] }
  : ntArm                                                  { [$1] }
  | ntArm arms                                             { $1 : $2 }
  | sep_by1(pat,'|') arm_guard '=>' safe_block             { [Arm [] $1 $2 (BlockExpr [] $4 mempty) mempty] }
  | sep_by1(pat,'|') arm_guard '=>' safe_block    ','      { [Arm [] $1 $2 (BlockExpr [] $4 mempty) mempty] }
  | sep_by1(pat,'|') arm_guard '=>' safe_block        arms { Arm [] $1 $2 (BlockExpr [] $4 mempty) mempty : $> }
  | sep_by1(pat,'|') arm_guard '=>' safe_block    ',' arms { Arm [] $1 $2 (BlockExpr [] $4 mempty) mempty : $> }
  | sep_by1(pat,'|') arm_guard '=>' nonblock_expr          { [Arm [] $1 $2 $4 mempty] }
  | sep_by1(pat,'|') arm_guard '=>' nonblock_expr ','      { [Arm [] $1 $2 $4 mempty] }
  | sep_by1(pat,'|') arm_guard '=>' nonblock_expr ',' arms { Arm [] $1 $2 $4 mempty : $6 }

arm_guard :: { Maybe (Expr Span) }
  : {- empty -}  { Nothing }
  | if expr      { Just $2 }

if_expr :: { Expr Span }
  : if             nostruct_expr safe_block else_expr {% withSpan $1 (If [] $2 $3 $4) }
  | if let pat '=' nostruct_expr safe_block else_expr {% withSpan $1 (IfLet [] $3 $5 $6 $7) }

else_expr :: { Maybe (Expr Span) }
  : else safe_block {% Just <\$> withSpan $1 (BlockExpr [] $2) }
  | else if_expr    { Just $2 }
  | {- empty -}     { Nothing }

lambda_expr :: { Expr Span }
  : move args '->' ty safe_block {% withSpan $1 (Closure [] Value (FnDecl $2 (Just $4) False mempty) (BlockExpr [] $> mempty)) }
  | move args         expr       {% withSpan $1 (Closure [] Value (FnDecl $2 Nothing   False mempty) $>) }
  |      args '->' ty safe_block {% withSpan $1 (Closure [] Ref   (FnDecl $1 (Just $3) False mempty) (BlockExpr [] $> mempty)) }
  |      args         expr       {% withSpan $1 (Closure [] Ref   (FnDecl $1 Nothing   False mempty) $>) }

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

-- TODO: consider attributes
stmt :: { Stmt Span }
  : ntStmt                               { $1 }
  | let pat ':' ty initializer ';'       {% withSpan $1 (Local $2 (Just $4) $5 []) }
  | let pat initializer ';'              {% withSpan $1 (Local $2 Nothing $3 []) } 
  | nonblock_expr ';'                    {% withSpan $1 (Semi $1) }
  | block_expr                           {% withSpan $1 (NoSemi $1) }
  | stmt_item                            {% withSpan $1 (ItemStmt $1) }
  | pub stmt_item                        {% withSpan $1 (ItemStmt (let Item i a n _ s = $2 in Item i a n PublicV s)) }
 -- | ';'                                  {%  

-- List of statements where the last statement might be a no-semicolon statement.
stmts_possibly_no_semi :: { [Stmt Span] }
  : stmt stmts_possibly_no_semi              { $1 : $2 }
  | stmt                                     { [$1] }
  | nonblock_expr                            { [NoSemi $1 mempty] }

initializer :: { Maybe (Expr Span) }
  : '=' expr      { Just $2 }
  | {- empty -}   { Nothing }


block :: { Block Span }
  : ntBlock                               { $1 }
  | unsafe_block                          { $1 }
  | safe_block                            { $1 }

unsafe_block :: { Block Span }
  : unsafe '{' '}'                        {% withSpan $1 (Block [] (UnsafeBlock False)) }
  | unsafe '{' stmts_possibly_no_semi '}' {% withSpan $1 (Block $3 (UnsafeBlock False)) }
safe_block :: { Block Span }
  :        '{' '}'                        {% withSpan $1 (Block [] DefaultBlock) }
  |        '{' stmts_possibly_no_semi '}' {% withSpan $1 (Block $2 DefaultBlock) }


-----------
-- Items --
-----------

item :: { Item Span }
  : ntItem                                       { $1 }
  | stmt_item                                    { $1 }
  | expr_path '!' '[' many(token_tree) ']' ';'   {% withSpan $1 (Item (mkIdent "") [] (MacItem (Mac $1 $4 mempty)) InheritedV) }
  | expr_path '!' '(' many(token_tree) ')' ';'   {% withSpan $1 (Item (mkIdent "") [] (MacItem (Mac $1 $4 mempty)) InheritedV) }
  | expr_path '!' '{' many(token_tree) '}'       {% withSpan $1 (Item (mkIdent "") [] (MacItem (Mac $1 $4 mempty)) InheritedV) }

mod_item :: { Item Span }
  : vis item                                     {% let Item i a n _ _ = $2 in withSpan $1 (Item i a n (unspan $1)) }

foreign_item :: { ForeignItem Span }
  : vis static ident ':' ty ';'
      {% withSpan $1 (ForeignItem (unspan $3) [] (ForeignStatic $5 False) (unspan $1)) }
  | vis static mut ident ':' ty ';'
      {% withSpan $1 (ForeignItem (unspan $4) [] (ForeignStatic $6 True) (unspan $1)) }
  | vis fn ident generics fn_decl where_clause ';'
      {% withSpan $1 (ForeignItem (unspan $3) [] (ForeignFn $5 $4{ whereClause = $6 }) (unspan $1)) }

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
  | const safety   fn ident generics fn_decl where_clause safe_block {% withSpan $1 (Item (unspan $4) [] (Fn $6 $2 Const Rust $5{ whereClause = $7 } $8) InheritedV) }
  | unsafe ext_abi fn ident generics fn_decl where_clause safe_block {% withSpan $1 (Item (unspan $4) [] (Fn $6 Unsafe NotConst $2 $5{ whereClause = $7 } $8) InheritedV) }
  | extern     abi fn ident generics fn_decl where_clause safe_block {% withSpan $1 (Item (unspan $4) [] (Fn $6 Normal NotConst $2 $5{ whereClause = $7 } $8) InheritedV) }
  | fn ident generics fn_decl where_clause safe_block    {% withSpan $1 (Item (unspan $2) [] (Fn $4 Normal NotConst Rust $3{ whereClause = $5 } $6) InheritedV) }
  | mod ident ';'                                        {% withSpan $1 (Item (unspan $2) [] (Mod []) InheritedV) }
  | mod ident '{' many(mod_item) '}'                     {% withSpan $1 (Item (unspan $2) [] (Mod $4) InheritedV) }
  | extern abi '{' many(foreign_item) '}'                {% withSpan $1 (Item (mkIdent "") [] (ForeignMod $2 $4) InheritedV) }
  | struct ident generics where_clause struct_decl_args  {% withSpan $1 (Item (unspan $2) [] (StructItem $5 $3{ whereClause = $4 }) InheritedV) }
  | union ident generics where_clause struct_decl_args   {% withSpan $1 (Item (unspan $2) [] (Union $5 $3{ whereClause = $4 }) InheritedV) }
  | enum ident generics where_clause enum_defs           {% withSpan $1 (Item (unspan $2) [] (Enum $5 $3{ whereClause = $4 }) InheritedV) }
  | item_impl                                            { Item (mkIdent "") [] $1 InheritedV mempty }
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

struct_decl_field :: { StructField Span }
  : vis ident ':' ty_sum                               {% withSpan $1 (StructField (Just (unspan $2)) (unspan $1) $4 []) }

enum_defs :: { [Variant Span] }
  : '{' '}'                                            { [] }
  | '{' sep_by1(enum_def,',')     '}'                  { toList $2 }
  | '{' sep_by1(enum_def,',') ',' '}'                  { toList $2 }

tuple_decl_field :: { StructField Span }
  : ty_sum                                              {% withSpan $1 (StructField Nothing InheritedV $1 []) }

enum_def :: { Variant Span }
  : ident '{' '}'                                       {% withSpan $1 (Variant (unspan $1) [] (StructD [] mempty) Nothing) }
  | ident '{' sep_by1(struct_decl_field,',') '}'        {% withSpan $1 (Variant (unspan $1) [] (StructD (toList $3) mempty) Nothing) }
  | ident '{' sep_by1(struct_decl_field,',') ',' '}'    {% withSpan $1 (Variant (unspan $1) [] (StructD (toList $3) mempty) Nothing) }
  | ident '(' ')'                                       {% withSpan $1 (Variant (unspan $1) [] (TupleD [] mempty) Nothing) }
  | ident '(' sep_by1(tuple_decl_field,',') ')'         {% withSpan $1 (Variant (unspan $1) [] (TupleD (toList $3) mempty) Nothing) }
  | ident '(' sep_by1(tuple_decl_field,',') ',' ')'     {% withSpan $1 (Variant (unspan $1) [] (TupleD (toList $3) mempty) Nothing) }
  | ident '=' expr                                      {% withSpan $1 (Variant (unspan $1) [] (UnitD mempty) (Just $3)) }
  | ident                                               {% withSpan $1 (Variant (unspan $1) [] (UnitD mempty) Nothing) }


-- parse_where_clause
where_clause :: { WhereClause Span }
  : {- empty -}                        { WhereClause [] mempty }
  | ntWhereClause                      { $1 } 
  | where sep_by1(where_predicate,',') {% withSpan $1 (WhereClause (toList $2)) }

where_predicate :: { WherePredicate Span }
  : lifetime ':' sep_by(lifetime,'+')                      {% withSpan $1 (RegionPredicate $1 $3) }
  | ty_path '=' ty                                         {% withSpan $1 (EqPredicate $1 $3) }
  | no_for_ty                                              {% withSpan $1 (BoundPredicate [] $1 []) }
  | no_for_ty ':' sep_by1(ty_param_bound_mod,'+')          {% withSpan $1 (BoundPredicate [] $1 (toList $3)) }
  | for_lts no_for_ty                                      {% withSpan $1 (BoundPredicate (unspan $1) $2 []) }
  | for_lts no_for_ty ':' sep_by1(ty_param_bound_mod,'+')  {% withSpan $1 (BoundPredicate (unspan $1) $2 (toList $4)) }


item_impl :: { ItemKind Span }
  : unsafe impl generics ty_prim_sum where_clause impl_items               { Impl Unsafe Positive $3{ whereClause = $5 } Nothing $4 $> } 
  |        impl generics ty_prim_sum where_clause impl_items               { Impl Normal Positive $2{ whereClause = $4 } Nothing $3 $> }
  | unsafe impl generics '(' ty ')'  where_clause impl_items               { Impl Unsafe Positive $3{ whereClause = $7 } Nothing $5 $> } 
  |        impl generics '(' ty ')'  where_clause impl_items               { Impl Normal Positive $2{ whereClause = $6 } Nothing $4 $> }
  | unsafe impl generics '!' trait_ref for ty_sum where_clause impl_items  { Impl Unsafe Negative $3{ whereClause = $8 } (Just $5) $7 $> }
  | unsafe impl generics     trait_ref for ty_sum where_clause impl_items  { Impl Unsafe Positive $3{ whereClause = $7 } (Just $4) $6 $> }
  |        impl generics '!' trait_ref for ty_sum where_clause impl_items  { Impl Normal Negative $2{ whereClause = $7 } (Just $4) $6 $> }
  |        impl generics     trait_ref for ty_sum where_clause impl_items  { Impl Normal Positive $2{ whereClause = $6 } (Just $3) $5 $> }
  | unsafe impl generics     trait_ref for '..' '{' '}'                    {% case $3 of { Generics [] [] _ _ -> pure (DefaultImpl Unsafe $4); _ -> fail "todo" } }
  |        impl generics     trait_ref for '..' '{' '}'                    {% case $2 of { Generics [] [] _ _ -> pure (DefaultImpl Normal $3); _ -> fail "todo" } }

impl_items :: { [ImplItem Span] }
  : '{' '}'                             { [] }
  | '{' sep_by1(impl_item,',') '}'      { toList $2 }
  | '{' sep_by1(impl_item,',') ',' '}'  { toList $2 }

impl_item :: { ImplItem Span }
  : ntImplItem                                         { $1 }
  | vis def type ident '=' ty ';'                      {% withSpan $1 (ImplItem (unspan $4) (unspan $1) $2 [] (TypeI $6)) }
  | vis def const ident ':' ty '=' expr ';'            {% withSpan $1 (ImplItem (unspan $4) (unspan $1) $2 [] (ConstI $6 $8)) }
  | vis def mod_mac                                    {% withSpan $1 (ImplItem (mkIdent "") (unspan $1) $2 [] (MacroI $3)) }
  | vis def const safety fn ident generics fn_decl_with_self where_clause safe_block
     {% withSpan $1 (ImplItem (unspan $6) (unspan $1) $2 [] (MethodI (MethodSig $4 Const Rust $8 $7{ whereClause = $9 }) $>)) }
  | vis def safety ext_abi fn ident generics fn_decl_with_self where_clause safe_block
     {% withSpan $1 (ImplItem (unspan $6) (unspan $1) $2 [] (MethodI (MethodSig $3 NotConst $4 $8 $7{ whereClause = $9 }) $>)) }


trait_item :: { TraitItem Span }
  : ntTraitItem                          { $1 }
  | const ident ':' ty_sum  '=' expr ';' {% withSpan $1 (TraitItem (unspan $2) [] (ConstT $4 (Just $6))) }
  | const ident ':' ty_sum           ';' {% withSpan $1 (TraitItem (unspan $2) [] (ConstT $4 Nothing)) }
  | mod_mac                              {% withSpan $1 (TraitItem (mkIdent "") [] (MacroT $1)) }
  | type ty_param ';'                    {% let TyParam _ i b d _ = $2 in withSpan $1 (TraitItem i [] (TypeT b d)) }
  | vis safety ext_abi fn ident generics fn_decl_with_self where_clause ';'
     {% withSpan $1 (TraitItem (unspan $5) [] (MethodT (MethodSig $2 NotConst $3 $7 $6{ whereClause = $8}) Nothing)) }
  | vis safety ext_abi fn ident generics fn_decl_with_self where_clause safe_block
     {% withSpan $1 (TraitItem (unspan $5) [] (MethodT (MethodSig $2 NotConst $3 $7 $6{ whereClause = $8}) (Just $>))) }


safety :: { Unsafety }
  : {- empty -}     { Normal }
  | unsafe          { Unsafe }

ext_abi :: { Abi }
  : {- empty -}     { Rust }
  | extern abi      { $2 }

vis :: { Spanned (Visibility Span) }
  : {- empty -}          { pure InheritedV }
  | pub                  {% withSpan $1 (Spanned PublicV) }
  | pub '(' crate ')'    {% withSpan $1 (Spanned CrateV) }
  | pub '(' mod_path ')' {% withSpan $1 (Spanned (RestrictedV $3)) }

def :: { Defaultness }
  : {- empty -}          { Final }
  | default              { Default }

-- TODO: How to translate the commented out cases?
view_path :: { ViewPath Span }
  : mod_path                                     {% withSpan $1 (ViewPathSimple (fst (N.last (segments $1))) $1) }
  | mod_path as ident                            {% withSpan $1 (ViewPathSimple (unspan $3) $1) }
  | mod_path '::' '*'                            {% withSpan $1 (ViewPathGlob $1) }
  | mod_path '::' '{'                        '}' {% withSpan $1 (ViewPathList $1 []) }
  | mod_path '::' '{' sep_by1(plist,',')     '}' {% withSpan $1 (ViewPathList $1 (toList $4)) }
  | mod_path '::' '{' sep_by1(plist,',') ',' '}' {% withSpan $1 (ViewPathList $1 (toList $4)) }
-- |         '::' '{'                '}'       { $$ = mk_node("ViewPathList", 1, mk_atom("ViewPathListEmpty")); }
-- |         '::' '{' idents_or_self '}'       { $$ = mk_node("ViewPathList", 1, $3); }
-- |         '::' '{' idents_or_self ',' '}'   { $$ = mk_node("ViewPathList", 1, $3); }
-- |              '{'                '}'       { $$ = mk_atom("ViewPathListEmpty"); }
-- |              '{' idents_or_self '}'       { $$ = mk_node("ViewPathList", 1, $2); }
-- |              '{' idents_or_self ',' '}'   { $$ = mk_node("ViewPathList", 1, $2); }

plist :: { PathListItem Span }
  : ident           {% withSpan $1 (PathListItem (unspan $1) Nothing) }
  | ident as ident  {% withSpan $1 (PathListItem (unspan $1) (Just (unspan $3))) }

-------------------
-- Macro related --
-------------------

expr_mac :: { Mac Span }
  : expr_path '!' '[' many(token_tree) ']'      {% withSpan $1 (Mac $1 $4) }
  | expr_path '!' '(' many(token_tree) ')'      {% withSpan $1 (Mac $1 $4) }

ty_mac :: { Mac Span }
  : ty_path '!' '[' many(token_tree) ']'      {% withSpan $1 (Mac $1 $4) }
  | ty_path '!' '{' many(token_tree) '}'      {% withSpan $1 (Mac $1 $4) }
  | ty_path '!' '(' many(token_tree) ')'      {% withSpan $1 (Mac $1 $4) }

mod_mac :: { Mac Span }
  : mod_path '!' '[' many(token_tree) ']' ';'  {% withSpan $1 (Mac $1 $4) }
  | mod_path '!' '{' many(token_tree) '}'      {% withSpan $1 (Mac $1 $4) }
  | mod_path '!' '(' many(token_tree) ')' ';'  {% withSpan $1 (Mac $1 $4) }

token_tree :: { TokenTree }
  : ntTT                     { $1 }
  -- # Delimited
  | '(' many(token_tree) ')'                              { Delimited mempty Paren mempty $2 mempty }
  | '{' many(token_tree) ')'                              { Delimited mempty Brace mempty $2 mempty }
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
  | '$'        { $1 } 
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

-- | Given a spanned token, convert it to a token tree. Basically just move the Span
mkTokenTree :: Spanned Token -> TokenTree
mkTokenTree (Spanned t s) = Token s t

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
