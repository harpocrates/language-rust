{
module Language.Rust.Parser.Parser (attributeP, typeP, literalP) where

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
import Data.List.NonEmpty hiding (length)
}

-- <https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y>
-- <https://github.com/rust-lang/rust/blob/master/src/libsyntax/parse/parser.rs>
-- References to <https://doc.rust-lang.org/grammar.html>
-- To see conflicts: stack exec happy -- --info=happyinfo.txt -o /dev/null src/Language/Rust/Parser/Parser2.y

-- in order to document the parsers, we have to alias them
%name literalP lit
%name attributeP attribute
%name typeP ty
%name expressionP expr

%tokentype { TokenSpace Spanned }

%monad { P } { >>= } { return }
%error { parseError }
%lexer { lexRust } { Tok (Spanned Eof _) }

%expect 1

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

-- | Optional parser
opt(p)          : p                   { Just $1 }
                |                     { Nothing }

-- | One or more
some(p)         : p many(p)           { $1 :| $2 }

-- | Zero or more 
many(p)         :                     { [] }
                | p many(p)           { $1 : $2 }

-- | Zero or more occurrences of p, separated by sep
sep_by(p,sep)   : sep_by1(p,sep)      { $1 }
                | {- empty -}         { [] }

-- | One or more occurences of p, seperated by sep
sep_by1(p,sep)  : sep_by1(p,sep) sep p  { $1 ++ [$3] }
                | p                     { [$1] }

-- | Plus delimited, at least one
plus(p)         : sep_by1(p,'+')      { $1 }

-- | Comma delimited, at least one
comma(p)        : sep_by(p,',')       { $1 }

-- | One or the other, but of the same type
alt(l,r)        : l                   { $1 }
                | r                   { $1 }

-------------
-- General --
-------------


--------------------------
-- Attributes
--------------------------
-- TODO: list case of a meta_item should admit trailing commas
-- TODO: add a check that the literals in meta_item and meta_item_inner are unsuffixed

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
  : ident                                 {% withSpan $1 (Word (unspan $1)) }
  | ident '=' lit                         {% withSpan $1 (NameValue (unspan $1) $3) }
  | ident '(' comma(meta_item_inner) ')'  {% withSpan $1 (List (unspan $1) $3) }

-- parse_meta_item_inner()
meta_item_inner :: { NestedMetaItem Span }
  : lit                          {% withSpan $1 (Literal $1) }
  | meta_item                    {% withSpan $1 (MetaItem $1) } 


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


-----------
-- Paths --
-----------

-- parse_qualified_path(PathStyle::Type)
ty_qual_path :: { Ty Span }
  : '<' ty_sum '>' '::' path_segments_without_colons            {% withSpan $1 (PathTy (Just (QSelf $2 0)) (Path False (unspan $5) (posOf $5))) }
  | '<' ty_sum as ty_path '>' '::' path_segments_without_colons {% let segs = segments $4 <> unspan $7
                                                                   in withSpan $1 (PathTy (Just (QSelf $2 (length (segments $4)))) $4{ segments = segs }) }

-- parse_generic_values_after_lt()
generic_values :: { ([Lifetime Span], [Ty Span], [(Ident, Ty Span)]) }
generic_values : lifetimes_tysums_bindings { $1 }

-- TODO: can this be made left recursive?
lifetimes_tysums_bindings
  : lifetime ',' lifetimes_tysums_bindings   { let (lts, tys, bds) = $3 in ($1 : lts, tys, bds) }
  | lifetime                                 { ([$1], [], []) }
  | tysums_bindings                          { let (tys, bds) = $1 in ([], tys, bds) } 

-- TODO: can this be made left recursive?
tysums_bindings
  : ty_sum ',' tysums_bindings               { let (tys, bds) = $3 in ($1 : tys, bds) }
  | ty_sum                                   { ([$1], []) }
  | comma(binding)                           { ([], $1) }

binding : ident '=' ty                             { (unspan $1, $3) }


-- parse_path(PathStyle::Type)
ty_path :: { Path Span }
  : path_segments_without_colons            {% withSpan $1 (Path False (unspan $1)) }
  | '::' path_segments_without_colons       {% withSpan $1 (Path True (unspan $2)) }

-- parse_path_segments_without_colons()
path_segments_without_colons :: { Spanned (NonEmpty (Ident, PathParameters Span)) }
  : sep_by1(path_segment_without_colons, '::')  { sequence (fromList $1) }

-- No corresponding function - see path_segments_without_colons
path_segment_without_colons :: { Spanned (Ident, PathParameters Span) }
  : ident path_parameter 
     {% if not (isTypePathSegmentIdent $1)
          then fail "invalid path segment in type path"
          else withSpan $1 (Spanned (unspan $1, $2))
     }


-- TODO: comma(ty_sum), not comma(ty) for the second/third cases
path_parameter :: { PathParameters Span }
  : '<' generic_values '>'     {% let (lts, tys, bds) = $2 in withSpan $1 (AngleBracketed lts tys bds) }
  | '(' comma(ty) ')'          {% withSpan $1 (Parenthesized $2 Nothing) }
  | '(' comma(ty) ')' '->' ty  {% withSpan $1 (Parenthesized $2 (Just $5)) }
  | {- empty -}                { AngleBracketed [] [] [] mempty }


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
  | '(' ty ',' ')'                   {% withSpan $1 (TupTy [$2]) }
  | '(' ty ',' sep_by1(ty,',') ')'   {% withSpan $1 (TupTy ($2 : $4)) }
  | '[' ty ']'                       {% withSpan $1 (Slice $2) }
  | '*' ty                           {% withSpan $1 (Ptr Immutable $2) }
  | '*' const ty                     {% withSpan $1 (Ptr Immutable $3) }
  | '*' mut ty                       {% withSpan $1 (Ptr Mutable $3) }
  | '&' ty                           {% withSpan $1 (Rptr Nothing Immutable $2) }
  | '&' mut ty                       {% withSpan $1 (Rptr Nothing Mutable $3) }
  | '&' lifetime ty                  {% withSpan $1 (Rptr (Just $2) Immutable $3) }
  | '&' lifetime mut ty              {% withSpan $1 (Rptr (Just $2) Mutable $4) }
  | ty_path                          {% withSpan $1 (PathTy Nothing $1) }
  | ty_qual_path                     { $1 }
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
 {- | for_lts trait_ref '+' sep_by1(ty_param_bound,'+')  {%
      do poly <- withSpan $1 (PolyTraitRef (unspan $1) $2)
         withSpan $1 (PolyTraitRefTy (TraitTyParamBound poly None :| $4)) 
    } 
-}
{-
      | impl ty_param_bounds_mod         {% if (any isTraitTyParamBound (unspan $2))
                                              then pure (withSpan (ImplTrait <\$> $2 <* $1))
                                              else fail "at least one trait must be specified"
                                         }
-}




-- parse_ty_sum()
ty_sum :: { Ty Span }
  : ty                                   { $1 }
  | ty '+' sep_by1(ty_param_bound,'+')   {% withSpan $1 (ObjectSum $1 $3) }
 
fn_decl :: { FnDecl Span }
  : fn '(' comma(arg_general) opt('...') ')' ret_ty  {% withSpan $1 (FnDecl $3 $6 (isJust $4)) }

-- TODO: consider inlinging this
-- parse_ty_param_bounds(BoundParsingMode::Bare)
ty_param_bounds_bare :: { [TyParamBound Span] }
  : sep_by1(ty_param_bound,'+')   { $1 }
  
ty_param_bound :: { TyParamBound Span }
  : lifetime         { RegionTyParamBound $1 }
  | poly_trait_ref   { TraitTyParamBound $1 None }

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
  : for '<' comma(lifetime_def) '>'   {% withSpan $1 (Spanned $3) } 

-- No corresponding parse function
lifetime_def :: { LifetimeDef Span }
  : outer_attribute many(outer_attribute) lifetime ':' sep_by1(lifetime,'+') {% withSpan $1 (LifetimeDef ($1 : $2) $3 $5) }
  | outer_attribute many(outer_attribute) lifetime                           {% withSpan $1 (LifetimeDef ($1 : $2) $3 []) }
  | lifetime ':' sep_by1(lifetime,'+')                                       {% withSpan $1 (LifetimeDef [] $1 $3) }
  | lifetime                                                                 {% withSpan $1 (LifetimeDef [] $1 []) }


-----------------
-- Expressions --
-----------------

-- TODO: lit should have attributes. Also, some other cases are missing... ;)
expr :: { Expr Span }
  : lit       {% withSpan $1 (Lit [] $1) }


{

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

isAbi :: InternedString -> Bool
isAbi s = s `elem` words "Cdecl Stdcall Fastcall Vectorcall Aapcs Win64 SysV64 Rust C System RustIntrinsic RustCall PlatformIntrinsic"

isTraitTyParamBound TraitTyParamBound{} = True
isTraitTyParamBound _ = False
 
withSpan :: Located node => node -> (Span -> a) -> P a
withSpan node mkNode = do
  let Span lo _ = posOf node
  hi <- getPosition
  pure (mkNode (Span lo hi))

-- Functions related to `NonEmpty` that really should exist...
(<++) :: [a] -> NonEmpty a -> NonEmpty a
xs <++ ys = foldr (<|) ys xs

(++>) :: NonEmpty a -> [a] -> NonEmpty a
(x :| xs) ++> ys = x :| (xs ++ ys)

}
