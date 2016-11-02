{
module Language.Rust.Parser.Lexer (alexScanTokens) where

import Language.Rust.Data.InputStream
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
}

-- TODO:
--  apart from the couple of TODOs everywhere in this file,
--  add Spans onto everything and use %wrapper=monad
--  fix raw strings

%wrapper "basic"

-- 3.2 Identifiers
-- Technically, these use $XID_START and $XID_CONTINUE: @ident = ($xid_start | \_ $xid_continue) $xid_continue*
-- https://github.com/alainfrisch/sedlex/blob/96eb7859711295bdb5ae742f05828287f7aa2a16/src/syntax/unicode63.ml
-- For now, we make some approximations:

$xid_start = [ a-z A-Z ]
$xid_continue = [ a-z A-Z 0-9 _ ]
@ident = ($xid_start | \_ $xid_continue) $xid_continue*

-- 3.2.2 Delimiter-restricted productions

$non_null = [ \x00 ]               -- non_null is any single Unicode character aside from U+0000 (null)
$eol = \n
$non_eol = $non_null # $eol            -- non_eol is non_null restricted to exclude U+000A ('\n')
$non_single_quote = $non_null # [ \' ] -- non_single_quote is non_null restricted to exclude U+0027 (')
$non_double_quote = $non_null # [ \" ] -- non_double_quote is non_null restricted to exclude U+0022 (")

-- 3.3 Comments

@block_comment_body = [^ \* \/ ]*
@block_comment = "/*" @block_comment_body "*/"
@line_comment = "//" $non_eol*
@comment = ( @block_comment | @line_comment | $white )+

-- literals

@lit_suffix = $xid_start $xid_continue*

-- 3.5.2 Character and string literals

$hex_digit = [0-9 A-F a-f]
$oct_digit = 0-7
$dec_digit = 0-9
$nonzero_dec = 1-9

@common_escape = \\ | 'n' | 'r' | 't' | '0' | 'x' $hex_digit 2
@unicode_escape = u \{ $hex_digit{1,6} \}

@char_body = $non_single_quote | \\ ( \' | @common_escape | @unicode_escape )                                 -- "
@string_body = $non_double_quote | \x5c ( \x22 | @common_escape | @unicode_escape )

-- 3.5.2.3 Number literals

@dec_lit = [ $dec_digit '_' ]+
@exponent = ['E' 'e'] [ \- \+ ]? @dec_lit

@int_lit = $nonzero_dec [ $dec_digit '_' ]*
         | '0' (        [ $dec_digit '_' ]*
               | 'b'    [ '1' '0'    '_' ]+
               | 'o'    [ $oct_digit '_' ]+
               | 'x'    [ $hex_digit '_' ]+  )

@float_lit = $dec_digit [ $dec_digit '_' ]* ( @exponent | '.' @dec_lit @exponent? )?


-- TODO: Add SheBang and Eof by looking at the bytestring before and after tokenizing.

tokens :-

  -- Expression-operator symbols. 
  "="                             { \_ -> Eq }
  "<"                             { \_ -> Lt }
  "<="                            { \_ -> Le }
  "=="                            { \_ -> EqEq }
  "!="                            { \_ -> Ne }
  ">="                            { \_ -> Ge }
  ">"                             { \_ -> Gt }
  "&&"                            { \_ -> AndAnd }
  "||"                            { \_ -> OrOr }
  "!"                             { \_ -> Not }
  "~"                             { \_ -> Tilde }
  
  "+"                             { \_ -> BinOp Plus }
  "-"                             { \_ -> BinOp Minus }
  "*"                             { \_ -> BinOp Star }
  "/"                             { \_ -> BinOp Slash }
  "%"                             { \_ -> BinOp Percent }
  "^"                             { \_ -> BinOp Caret }
  "&"                             { \_ -> BinOp And }
  "|"                             { \_ -> BinOp Or }
  "<<"                            { \_ -> BinOp Shl }
  ">>"                            { \_ -> BinOp Shr }

  "++"                            { \_ -> BinOpEq Plus }
  "-+"                            { \_ -> BinOpEq Minus }
  "*+"                            { \_ -> BinOpEq Star }
  "/+"                            { \_ -> BinOpEq Slash }
  "%+"                            { \_ -> BinOpEq Percent }
  "^+"                            { \_ -> BinOpEq Caret }
  "&+"                            { \_ -> BinOpEq And }
  "|+"                            { \_ -> BinOpEq Or }
  "<<+"                           { \_ -> BinOpEq Shl }
  ">>+"                           { \_ -> BinOpEq Shr }

  -- Structural symbols.
  "@"                             { \_ -> At }
  "..."                           { \_ -> DotDotDot }
  ".."                            { \_ -> DotDot }
  "."                             { \_ -> Dot }
  ","                             { \_ -> Comma }
  ";"                             { \_ -> Semi }
  "::"                            { \_ -> ModSep }
  ":"                             { \_ -> Colon }
  "->"                            { \_ -> RArrow }
  "<-"                            { \_ -> LArrow }
  "=>"                            { \_ -> FatArrow }
  "#"                             { \_ -> Pound }
  "$"                             { \_ -> Dollar }
  "?"                             { \_ -> Question }

  "("                             { \_ -> OpenDelim Paren }
  "["                             { \_ -> OpenDelim Bracket }
  "{"                             { \_ -> OpenDelim Brace }
  ")"                             { \_ -> CloseDelim Paren }
  "]"                             { \_ -> CloseDelim Bracket }
  "}"                             { \_ -> CloseDelim Brace }

  -- TODO Literals. TODO ByteStr and ByteStrRaw
  'b' \' @char_body \'            { \s -> Literal (Byte (Name s)) Nothing }
  \' @char_body \'                { \s -> Literal (Char (Name s)) Nothing }
  
  \" @string_body* \"             { \s -> Literal (Str_ (Name s)) Nothing } -- "
  'b' \" @string_body* \"         { \s -> Literal (ByteStr (Name s)) Nothing } -- "

  @int_lit                        { \s -> Literal (Integer (Name s)) Nothing }
  @float_lit                      { \s -> Literal (Float (Name s)) Nothing }

  -- Strict keywords used in the language.
  "as"                            { \_ -> As }
  "box"                           { \_ -> Box }
  "break"                         { \_ -> Break }
  "const"                         { \_ -> Const }
  "continue"                      { \_ -> Continue }
  "crate"                         { \_ -> Crate }
  "else"                          { \_ -> Else }
  "enum"                          { \_ -> Enum }
  "extern"                        { \_ -> Extern }
  "false"                         { \_ -> False_ }
  "fn"                            { \_ -> Fn }
  "for"                           { \_ -> For }
  "if"                            { \_ -> If }
  "impl"                          { \_ -> Impl }
  "in"                            { \_ -> In }
  "let"                           { \_ -> Let }
  "loop"                          { \_ -> Loop }
  "match"                         { \_ -> Match }
  "mod"                           { \_ -> Mod }
  "move"                          { \_ -> Move }
  "mut"                           { \_ -> Mut }
  "pub"                           { \_ -> Pub }
  "ref"                           { \_ -> Ref }
  "return"                        { \_ -> Return }
  "self"                          { \_ -> SelfValue }
  "Self"                          { \_ -> SelfType }
  "static"                        { \_ -> Static }
  "struct"                        { \_ -> Struct }
  "super"                         { \_ -> Super }
  "trait"                         { \_ -> Trait }
  "true"                          { \_ -> True_ }
  "type"                          { \_ -> Type }
  "unsafe"                        { \_ -> Unsafe }
  "use"                           { \_ -> Use }
  "where"                         { \_ -> Where }
  "while"                         { \_ -> While }

  -- Keywords reserved for future use.
  "abstract"                      { \_ -> Abstract }
  "alignof"                       { \_ -> Alignof }
  "become"                        { \_ -> Become }
  "do"                            { \_ -> Do }
  "final"                         { \_ -> Final }
  "macro"                         { \_ -> Macro }
  "offsetof"                      { \_ -> Offsetof }
  "override"                      { \_ -> Override }
  "priv"                          { \_ -> Priv }
  "proc"                          { \_ -> Proc }
  "pure"                          { \_ -> Pure }
  "sizeof"                        { \_ -> Sizeof }
  "typeof"                        { \_ -> Typeof }
  "unsized"                       { \_ -> Unsized }
  "virtual"                       { \_ -> Virtual }
  "yield"                         { \_ -> Yield }

  -- Weak keywords, have special meaning only in specific contexts.
  "default"                       { \_ -> Default }
  "'static"                       { \_ -> StaticLifetime }
  "union"                         { \_ -> Union }

  -- Identifiers.
  @ident                          { \s -> IdentTok (mkIdent s) }
  "_"                             { \_ -> Underscore }

  -- Lifetimes. TODO check ident is _not_ keyword
  \' @ident                       { \s -> Lifetime (mkIdent s) }

  -- DocComment. TODO exclude /********/
  "/**" @block_comment_body "*/"
| "/*!" @block_comment_body "*/"
| "///" $non_eol*
| "//!" $non_eol*                 { \s -> DocComment (Name s) }

  -- Whitespace.
  $white+                         { \_ -> Whitespace }

  -- Comment.
  @comment                        { \_ -> Comment }
