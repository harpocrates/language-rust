{

{-# LANGUAGE PatternSynonyms #-}
module Language.Rust.Parser.Parser2 (patP, exprP, tyP) where

import Language.Rust.Data.InputStream
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
import Language.Rust.Parser.Lexer
import Language.Rust.Parser.ParseMonad
import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Constants

-- <https://github.com/rust-lang/rust/blob/master/src/grammar/parser-lalr.y>
-- References to <https://doc.rust-lang.org/grammar.html>
-- To see conflicts: stack exec happy -- --info=happyinfo.txt -o /dev/null src/Language/Rust/Parser/Parser2.y
}

-- in order to document the parsers, we have to alias them
%name patP pat
%name exprP expr
%name tyP ty

%tokentype { TokenSpace Spanned }

%monad { P } { >>= } { return }
%error { parseError }
%lexer { lexRust } { Tok (Spanned Eof _) }

-- %expect 0

%token

  -- Expression-operator symbols. 
  '='        { Tok $$@(Spanned Equal _) }
  '<'        { Tok $$@(Spanned Less _) }
  '>'        { Tok $$@(Spanned Greater _) }
  '!'        { Tok $$@(Spanned Exclamation _) }
  '~'        { Tok $$@(Spanned Tilde _) }
  
  '+'        { Tok $$@(Spanned Plus _) }
  '-'        { Tok $$@(Spanned Minus _) }
  '*'        { Tok $$@(Spanned Star _) }
  '/'        { Tok $$@(Spanned Slash _) }
  '%'        { Tok $$@(Spanned Percent _) }
  '^'        { Tok $$@(Spanned Caret _) }
  '&'        { Tok $$@(Spanned Ampersand _) }
  '|'        { Tok $$@(Spanned Pipe _) }

  EQ         { NoSpTok $$@(Spanned Equal _) }
  LT         { Tok $$@(Spanned Less _) }
  GT         { Tok $$@(Spanned Greater _) }
  NOT        { Tok $$@(Spanned Exclamation _) }
  
  PLUS       { Tok $$@(Spanned Plus _) }
  MINUS      { Tok $$@(Spanned Minus _) }
  STAR       { Tok $$@(Spanned Star _) }
  SLASH      { Tok $$@(Spanned Slash _) }
  PERCENT    { Tok $$@(Spanned Percent _) }
  CARET      { Tok $$@(Spanned Caret _) }
  AMPERSAND  { Tok $$@(Spanned Ampersand _) }
  PIPE       { Tok $$@(Spanned Pipe _) }


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
  Self       { Tok $$@(Identifier "self") }
  self       { Tok $$@(Identifier "Self") } 
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
  -- docComment { Tok $$@(Spanned (DocComment _) _) }
  -- comment    { Tok $$@(Spanned Comment _) }

  -- Types
  boolTyp    { Tok $$@(Identifier "bool") }
  charTyp    { Tok $$@(Identifier "char") }
  i8Typ      { Tok $$@(Identifier "i8") }
  i16Typ     { Tok $$@(Identifier "i16") }
  i32Typ     { Tok $$@(Identifier "i32") }
  i64Typ     { Tok $$@(Identifier "i64") }
  u8Typ      { Tok $$@(Identifier "u8") }
  u16Typ     { Tok $$@(Identifier "u16") }
  u32Typ     { Tok $$@(Identifier "u32") }
  u64Typ     { Tok $$@(Identifier "u64") }
  isizeTyp   { Tok $$@(Identifier "isize") }
  usizeTyp   { Tok $$@(Identifier "usize") }
  f32Typ     { Tok $$@(Identifier "f32") }
  f64Typ     { Tok $$@(Identifier "f64") }
  strTyp     { Tok $$@(Identifier "str") }

  -- Identifiers.
  IDENT      { Tok $$@(Identifier _) }
  '_'        { Tok $$@(Spanned Underscore _) }

  -- Lifetimes.
  LIFETIME   { Tok $$@(Spanned (LifetimeTok _) _) }

-- fake-precedence symbol to cause '|' bars in lambda context to parse
-- at low precedence, permit things like |x| foo = bar, where '=' is
-- otherwise lower-precedence than '|'. Also used for proc() to cause
-- things like proc() a + b to parse as proc() { a + b }.
%left LAMBDA   -- precedence

%left self     -- precedence

-- MUT should be lower precedence than IDENT so that in the pat rule,
-- "& MUT pat" has higher precedence than "binding_mode ident [@ pat]"
%left mut ref -- precedence

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
---------------------
-- Extended tokens --
---------------------

-- All of these have type 'Spanned ()'
'<<=' : '<' LT EQ      { () <\$ $1 <* $2 <* $3 }
'>>=' : '>' GT EQ      { () <\$ $1 <* $2 <* $3 }
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
some(p)         : rev_list1(p)        { reverse $1 }

rev_list1(p)    : p                   { [$1] }
                | rev_list1(p) p      { $2 : $1 }

-- | Zero or more 
many(p)         : rev_list(p)         { reverse $1 }

rev_list(p)     : {- Empty -}         { [] }
                | rev_list(p) p       { $2 : $1 }

-- | Zero or more occurrences of p, separated by sep
sep_by(p,sep)   : {- Empty -}         { [] }
                | sep_by1(p,sep)      { $1 }

-- | One or more occurences of p, seperated by sep
sep_by1(p,sep)  : p many(then(sep,p)) { $1 : $2 }

-- | Sequence two parsers, return the result of the second (*>)
then(a,b)       : a b                 { $2 }

-- | Plus delimited, at least one
plus(p)         : sep_by1(p,'+')      { $1 }

-- | Comma delimited, at least one
comma(p)        : sep_by1(p,',')      { $1 }

-- | Comma delimited, allow trailing
commaT(p)       : {- empty -}         { [] }
                | comma(p) opt(',')   { $1 }

-- | One or the other
or(l,r)         : l                   { Left $1 }
                | r                   { Right $1 }

-- | One or the other, but of the same type
alt(l,r)        : l                   { $1 }
                | r                   { $1 }

-- | Both
and(l,r)        : l r                 { ($1, $2) }

-------------
-- General --
-------------

-- parse_qualified_path(PathStyle::Type)
ty_qual_path :: { Spanned (QSelf Span, Path Span) }
ty_qual_path : qual_path(path_segments_without_colons)   { $1 }

-- parse_qualified_path(PathStyle::Expr)
expr_qual_path :: { Spanned (QSelf Span, Path Span) }
expr_qual_path : qual_path(path_segments_with_colons)    { $1 }

qual_path(segs)
      : '<' ty_sum opt(then(as, ty_path)) '>' '::' segs
        { do
            path1 <- maybe (pure (Path False [] mempty)) id $3
            segs' <- (segments path1 ++) <\$> $6
            qself <- QSelf <\$> $2 <*> pure (length (segments path1))
            (qself, path1{ segments = segs' }) <\$ $1
        }

-- parse_path(PathStyle::Type)
ty_path :: { Spanned (Path Span) }
ty_path 
      : path_segments_without_colons           %prec ident { withSpan (Path False <\$> $1) }
      | '::' path_segments_without_colons      %prec ident { withSpan (Path True <\$> $2 <* $1) }

-- parse_path(PathStyle::Expr)
expr_path :: { Spanned (Path Span) }
expr_path 
      : path_segments_with_colons              %prec ident { withSpan (Path False <\$> $1) }
      | '::' path_segments_with_colons         %prec ident { withSpan (Path True <\$> $2 <* $1) }

-- parse_path_segments_without_colons()
path_segments_without_colons :: { Spanned [(Ident, PathParameters Span)] }
path_segments_without_colons : sep_by1(path_segment_without_colons, '::')  { sequence $1 } 

-- No corresponding function - see parse_path_segments_without_colons
path_segment_without_colons :: { Spanned (Ident, PathParameters Span) }
path_segment_without_colons
      : ident                                
          {% if (not . isTypePathSegmentIdent . unspan \$ $1)
               then fail "invalid path segment in type path"
               else pure $ do
                       i <- $1
                       pure (i, AngleBracketed [] [] [] mempty)
          }
      | ident '<' generic_values_after_lt '>'
          {% if (not . isTypePathSegmentIdent . unspan \$ $1)
               then fail "invalid path segment in type path"
               else pure $ do
                       i <- $1
                       let (lts, tys, bds) = $3
                       ang <- withSpan (AngleBracketed <\$> lts <*> tys <*> bds <* $2 <* $4)
                       pure (i, ang)
          }
      | ident '(' commaT(ty_sum) ')' opt(then('->', ty))
          {% if (not . isTypePathSegmentIdent . unspan \$ $1)
               then fail "invalid path segment in type path"
               else pure $ do
                      i <- $1
                      args <- withSpan (Parenthesized <\$> sequence $3 <*> sequence $5 <* $2)
                      pure (i, args)
          }

-- parse_path_segments_with_colons()
path_segments_with_colons :: { Spanned [(Ident, PathParameters Span)] }
path_segments_with_colons : sep_by1(path_segment_without_colons, '::')  { sequence $1 }

-- No corresponding function - see parse_path_segments_with_colons
path_segment_with_colons :: { Spanned (Ident, PathParameters Span) }
path_segment_with_colons
      : ident
          {% if (not . isPathSegmentIdent . unspan \$ $1)
               then fail "invalid path segment in expression path"
               else pure ((,) <\$> $1 <*> pure (AngleBracketed [] [] [] mempty))
          }
      | ident '::' '<' generic_values_after_lt '>'
          {% if (not . isPathSegmentIdent . unspan \$ $1)
               then fail "invalid path segment in expression path"
               else pure $ do
                       i <- $1
                       let (lts, tys, bds) = $4
                       ang <- withSpan (AngleBracketed <\$> lts <*> tys <*> bds <* $5)
                       pure (i, ang)
          }

-- parse_generic_values_after_lt()
generic_values_after_lt :: { (Spanned [Lifetime Span], Spanned [Ty Span], Spanned [(Ident, Ty Span)]) }
generic_values_after_lt
      : comma(lifetime) ',' comma(ty_sum) ',' comma(binding) { (sequence $1, sequence $3, sequence $5) } 
      |                     comma(ty_sum) ',' comma(binding) { (pure []    , sequence $1, sequence $3) }
      | comma(lifetime) ','                   comma(binding) { (sequence $1, pure []    , sequence $3) }
      | comma(lifetime) ',' comma(ty_sum)                    { (sequence $1, sequence $3, pure []    ) }
      |                                       comma(binding) { (pure []    , pure []    , sequence $1) }
      |                     comma(ty_sum)                    { (pure []    , sequence $1, pure []    ) } 
      | comma(lifetime)                                      { (sequence $1, pure []    , pure []    )  }
      |                                                      { (pure []    , pure []    , pure []    ) }

-- parse_arg_general(false) -- does not require name
arg_general :: { Spanned (Arg Span) } 
arg_general
      : arg             { $1 }
      | ty_sum          { withSpan (Arg <\$> $1 <*> pure (IdentP (ByValue Immutable) invalidIdent Nothing mempty)) }

-- parse_arg_general(true) -- requires name
arg ::{ Spanned (Arg Span) }
arg   : pat ':' ty_sum  { withSpan (Arg <\$> $3 <*> $1) }


----------------
-- Attributes --
----------------

-- parse_outer_attributes()
outer_attributes :: { Spanned [Attribute Span] }
outer_attribtues : {- Unimplemented -}                { error "Unimplemented" }

--------------
-- Patterns --
--------------

pat :: { Spanned (Pat Span) }
pat   : '_'                                           { withSpan (WildP <\$ $1) }
      | '&' pat                                       { withSpan (RefP <\$ $1 <*> $2 <*> pure Immutable) }
      | '&' mut pat                                   { withSpan (RefP <\$ $1 <* $2 <*> $3 <*> pure Mutable) }
      | '(' pats_list_context ')'                     { withSpan (TupleP <\$> snd $2 <*> pure (fst $2)) }
      | '[' pats_list_binding ']'                     { $2 }
      | lit_expr                                      { withSpan (LitP <\$> $1) }
      | '-' lit_expr                                  { withSpan (LitP <\$> withSpan (Unary [] Neg <\$> $2)) }
      | expr_path                                     { withSpan (PathP Nothing <\$> $1) }
      | expr_qual_path                                { withSpan (PathP <\$> (Just . fst <\$> $1) <*> (snd <\$> $1)) }
      | lit_or_path '...' lit_or_path                 { withSpan (RangeP <\$> $1 <*> $3) }
      | expr_path '{' comma(pat_field) opt(',') '}'   { withSpan (StructP <\$> $1 <*> sequence $3 <*> pure False <* $5) }
      | expr_path '{' comma(pat_field) ',' '..' '}'   { withSpan (StructP <\$> $1 <*> sequence $3 <*> pure True <* $6) }
      | expr_path '{' '..' '}'                        { withSpan (StructP <\$> $1 <*> pure [] <*> pure True) }
      | expr_path '(' pats_list_context ')'           { withSpan (TupleStructP <\$> $1 <*> snd $3 <*> pure (fst $3)) }
  --  | expr_path '!' opt(ident) delimited_token_trees     { error "Unimplemented" } {- MacP (Mac a) a -}
      | binding_mode ident '@' pat                    { withSpan (IdentP <\$> $1 <*> $2 <*> (Just <\$> $4)) }
      | binding_mode ident                            { withSpan (IdentP <\$> $1 <*> $2 <*> pure Nothing) }
      | box pat                                       { withSpan (BoxP <\$ $1 <*> $2) }

pats_list_context :: { (Maybe Int, Spanned [Pat Span]) }
pats_list_context
      :                     '..'                                { (Nothing, [] <\$ $1) }
      | comma(pat) opt(',')                                     { (Nothing, sequence $1) }
      | comma(pat) opt(',') '..'                                { (Just (length $1), sequence $1 <* $3) }
      |                     '..' opt(',') comma(pat) opt(',')   { (Just 0, sequence $3 <* $1) }
      | comma(pat) opt(',') '..' opt(',') comma(pat) opt(',')   { (Just (length $1), (++) <\$> sequence $1 <*> sequence $5) }

pats_list_binding :: { Spanned (Pat Span) }
pats_list_binding
      :                     opt(pat) '..'                                { withSpan (SliceP [] <\$> sequence $1 <*> pure [] <* $2) }
      | comma(pat) opt(',')                                              { withSpan (SliceP <\$> sequence $1 <*> pure Nothing <*> pure []) }
      | comma(pat) opt(',') opt(pat) '..'                                { withSpan (SliceP <\$> sequence $1 <*> sequence $3 <*> pure []) }
      |                     opt(pat) '..' opt(',') comma(pat) opt(',')   { withSpan (SliceP [] <\$> sequence $1 <*> sequence $4) }
      | comma(pat) opt(',') opt(pat) '..' opt(',') comma(pat) opt(',')   { withSpan (SliceP <\$> sequence $1 <*> sequence $3 <*> sequence $6) }

pats_or :: { Spanned [Pat Span] }
pats_or : sep_by1(pat,'|')                                               { sequence $1 }

binding_mode :: { Spanned BindingMode }
binding_mode
      : ref mut               { ByRef Mutable <\$ $1 <* $2 }
      | ref                   { ByRef Immutable <\$ $1 }
      | mut                   { ByValue Mutable <\$ $1 }
      | {- Empty -} %prec mut { pure (ByValue Immutable) }

lit_or_path :: { Spanned (Expr Span) }
lit_or_path
      : expr_path      { withSpan (PathExpr [] Nothing <\$> $1) } 
      | expr_qual_path { withSpan (PathExpr [] <\$> (Just . fst <\$> $1) <*> (snd <\$> $1)) }
      | lit_expr       { $1 }
      | '-' lit_expr   { withSpan (Unary [] Neg <\$> $2 <* $1) }

pat_field :: { Spanned (FieldPat Span) }
pat_field
      :     binding_mode ident     { withSpan (FieldPat <\$> $2 <*> withSpan (IdentP <\$> $1 <*> $2 <*> pure Nothing) <*> pure True) }
      | box binding_mode ident     { withSpan (FieldPat <\$> $3 <*> withSpan (BoxP <\$> withSpan (IdentP <\$> $2 <*> $3 <*> pure  Nothing) <* $1) <*> pure True) }
      | binding_mode ident ':' pat   { withSpan (FieldPat <\$> $2 <*> withSpan (IdentP <\$> $1 <*> $2 <*> (Just <\$> $4)) <*> pure True) }


-----------
-- Types --
-----------

-- parse_ty()
ty :: { Spanned (Ty Span) }
ty    : '(' ty_sum ',' comma(ty_sum) ')' { withSpan (TupTy <\$> ((:) <\$> $2 <*> sequence $4) <* $1 <* $5) }
      | '(' ty_sum ',' ')'               { withSpan (TupTy <\$> (pure <\$> $2) <* $1 <* $4) }
      | '(' ')'                          { withSpan (TupTy [] <\$ $1 <* $2) }
      | '!'                              { withSpan (Never <\$ $1) }
      | '*' maybe_mut_or_const ty        { withSpan (Ptr <\$> $2 <*> $3 <* $1) }
      | '[' ty ']'                       { withSpan (Slice <\$> $2 <* $1 <* $3) }
      | '[' ty ';' expr ']'              { withSpan (Array <\$> $2 <*> $4 <* $1 <* $5) }
      | '&' opt(lifetime) maybe_mut ty   { withSpan (Rptr <\$> sequence $2 <*> $3 <*> $4 <* $1) }
      | for_in_type                      { $1 }
      | impl ty_param_bounds_mod         {% if (any isTraitTyParamBound (unspan $2))
                                              then pure (withSpan (ImplTrait <\$> $2 <* $1))
                                              else fail "at least one trait must be specified"
                                         }
      | ty_bare_fn                       { $1 (pure []) }
      | typeof '(' expr ')'              { withSpan (Typeof <\$> $3 <* $1 <* $4) }
      | ty_qual_path                     { withSpan (PathTy <\$> (Just . fst <\$> $1) <*> (snd <\$> $1)) }
      | ty_path                          { withSpan (PathTy Nothing <\$> $1) }
      | '_'                              { withSpan (Infer <\$ $1) }

-- parse_ty_sum()
ty_sum :: { Spanned (Ty Span) }
ty_sum
      : ty                             { $1 }
      | ty '+' ty_param_bounds_bare    { withSpan (ObjectSum <\$> $1 <*> $3) }

-- parse_ty_param_bounds(BoundParsingMode::Modified)
ty_param_bounds_mod :: { Spanned [TyParamBound Span] }
ty_param_bounds_mod : sep_by(or(lifetime, and(opt('?'),poly_trait_ref)),'+')
        { sequence (map (\x -> case x of
                                 Left l              -> RegionTyParamBound <\$> l
                                 Right (Nothing,bnd) -> TraitTyParamBound <\$> bnd <*> pure None
                                 Right (Just _,bnd)  -> TraitTyParamBound <\$> bnd <*> pure Maybe)
                        $1)
        }

-- parse_ty_param_bounds(BoundParsingMode::Bare)
ty_param_bounds_bare :: { Spanned [TyParamBound Span] }
ty_param_bounds_bare : sep_by(or(lifetime, poly_trait_ref),'+')
        { sequence (map (\x -> case x of
                                 Left l    -> RegionTyParamBound <\$> l
                                 Right bnd -> TraitTyParamBound <\$> bnd <*> pure None)
                        $1)
        }

-- parse_ty_bare_fn(lifetime_defs: Vec<ast::LifetimeDef>)
ty_bare_fn :: { Spanned [LifetimeDef Span] -> Spanned (Ty Span) }
ty_bare_fn
      :  opt(unsafe) opt(then(extern,abi)) fn '(' comma(arg_general) opt('...') ')' ret_ty
          { \lts -> withSpan $ do
                      lts' <- lts
                      unsafety <- maybe (pure Normal) (Unsafe <\$) $1
                      abi <- maybe (pure Rust) id $2
                      decl <- withSpan (FnDecl <\$> sequence $5 <*> $8 <*> pure (case $6 of { Nothing -> False; _ -> True }))
                      pure (BareFn unsafety abi lts' decl)
          }

-- Sort of like parse_opt_abi() -- currently doesn't handle raw string ABI
abi :: { Spanned Abi }
abi   : {- empty -}     { pure C }
      | str             {% case unspan $1 of
                             (LiteralTok (StrTok (Name s)) Nothing) | isAbi s -> pure (read s <\$ $1)
                             _ -> fail "invalid ABI"
                        }

-- parse_ret_ty
ret_ty :: { Spanned (Maybe (Ty Span)) }
ret_ty
      : {- empty -}     { pure Nothing }
      | '->' ty         { Just <\$> $2 <* $1 }

-- parse_for_in_type()
for_in_type :: { Spanned (Ty Span) }
for_in_type
      : late_bound_lifetime_defs ty_bare_fn                          { $2 $1 }
      | late_bound_lifetime_defs trait_ref
          { let poly = withSpan (PolyTraitRef <\$> $1 <*> $2)
            in withSpan (PolyTraitRefTy <\$> ((\x -> [x]) <\$> (TraitTyParamBound <\$> poly <*> pure None)))
          }
      | late_bound_lifetime_defs trait_ref '+' ty_param_bounds_bare
          { let poly = withSpan (PolyTraitRef <\$> $1 <*> $2)
            in withSpan (PolyTraitRefTy <\$> ((:) <\$> (TraitTyParamBound <\$> poly <*> pure None) <*> $4))
          }

-- no equivalent
maybe_mut :: { Spanned Mutability }
maybe_mut
      : mut                   { Mutable <\$ $1 }
      | {- empty -} %prec mut { pure Immutable }

-- no equivalent
maybe_mut_or_const :: { Spanned Mutability }
maybe_mut_or_const
      : mut                   { Mutable <\$ $1 }
      | const                 { Immutable <\$ $1 }
      | {- empty -} %prec mut { pure Immutable }

-- parse_poly_trait_ref()
poly_trait_ref :: { Spanned (PolyTraitRef Span) }
poly_trait_ref
      : late_bound_lifetime_defs trait_ref { withSpan (PolyTraitRef <\$> $1 <*> $2) }

-- parse_late_bound_lifetime_defs()
late_bound_lifetime_defs :: { Spanned [LifetimeDef Span] }
late_bound_lifetime_defs
      : {- empty -}                       { pure [] }
      | for '<' comma(lifetime_def) '>'   { $1 *> sequence $3 <* $4 } 

-- No corresponding parse function
lifetime_def :: { Spanned (LifetimeDef Span) }
lifetime_def
      : outer_attributes lifetime                           { withSpan (LifetimeDef <\$> $1 <*> $2 <*> pure []) }
      | outer_attributes lifetime ':' sep_by1(lifetime,'+') { withSpan (LifetimeDef <\$> $1 <*> $2 <*> sequence $4) }

-- parse_lifetime()
lifetime :: { Spanned (Lifetime Span) }
lifetime : LIFETIME                                        { let Spanned (LifetimeTok (Ident l _)) s = $1 in Spanned (Lifetime l s) s }

-- parse_trait_ref()
trait_ref :: { Spanned (TraitRef Span) }
trait_ref : ty_path                          %prec ident   { withSpan (TraitRef <\$> $1) }

-- no equivalent
binding :: { Spanned (Ident, Ty Span) }
binding : ident '=' ty        { (,) <\$> $1 <*> $3  }

--------------------------------
-- Statements and Expressions --
--------------------------------
{-
block :: { Spanned (Block Span) }
block
      : '{' maybe_stmts '}'                 { withSpan (Block <\$> $2 <*> pure DefaultBlock) }
      | unsafe '{' maybe_stmts '}'          { withSpan (Block <\$> $2 <*> pure UnsafeBlock) }

maybe_stmts :: { Spanned [Stmt Span] }
maybe_stmts
      : stmts               { $1 }
      | stmts nonblock_expr { do { ss <- stmts; s <- withSpan (NoSemi <\$> $2); pure (ss ++ [s]) } } 
      | nonblock_expr       { do {              s <- withSpan (NoSemi <\$> $2); pure [s]         } }
      | {- empty -}         { pure [] }

stmts :: { Spanned [Stmt Span] }
stmts : some(stmt)          { sequence $1 }

-- TODO: figure out attributes
stmt :: { Spanned (Stmt Span) }
stmt
: let pat opt(then(':',ty_sum)) opt(then('=',expr)) ';'   { withSpan (Local <\$> $2 <*> $3 <*> $4 <*> pure []) }
|                 stmt_item
|             PUB stmt_item { $$ = $2; }
| outer_attrs     stmt_item { $$ = $2; }
| outer_attrs PUB stmt_item { $$ = $3; }
| full_block_expr
| block
| nonblock_expr ';'
| ';'                   { $$ = mk_none(); }
;

maybe_exprs :: { Spanned [Expr Span] }
maybe_exprs
      : commaT(expr)      { sequence $1 }

exprs :: { Spanned [Expr Span] }
exprs : comma(expr)       { sequence $1 }
-}


--------------
-- Literals --
--------------

lit :: { Spanned (Lit Span) }
lit
      : byte              { lit $1 }
      | char              { lit $1 }
      | int               { lit $1 }
      | float             { lit $1 }
      | true              { lit $1 }
      | false             { lit $1 }
      | string            { $1 } 

string :: { Spanned (Lit Span) }
string
      : str               { lit $1 }
      | rawStr            { lit $1 }
      | byteStr           { lit $1 }
      | rawByteStr        { lit $1 }

-- TODO

lit_expr :: { Spanned (Expr Span) }
lit_expr : lit             { withSpan (Lit [] <\$> $1) }

expr :: { Spanned (Expr Span) }
expr : {- Unimplemented -}            { error "Unimplemented" }


{

lit :: Spanned Token -> Spanned (Lit Span)
lit (Spanned (IdentTok (Ident (Name "true") _)) s) = Spanned (Bool True Unsuffixed s) s
lit (Spanned (IdentTok (Ident (Name "false") _)) s) = Spanned (Bool False Unsuffixed s) s
lit (Spanned (LiteralTok litTok suffix_m) s) = Spanned (parseLit litTok suffix s) s
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

isPathSegmentIdent :: Ident -> Bool
isPathSegmentIdent i = True

isTypePathSegmentIdent :: Ident -> Bool
isTypePathSegmentIdent i = True

isAbi :: InternedString -> Bool
isAbi s = s `elem` words "Cdecl Stdcall Fastcall Vectorcall Aapcs Win64 SysV64 Rust C System RustIntrinsic RustCall PlatformIntrinsic"

isTraitTyParamBound TraitTyParamBound{} = True
isTraitTyParamBound _ = False
 
withSpan :: Spanned (Span -> a) -> Spanned a
withSpan (Spanned f s) = Spanned (f s) s

}
