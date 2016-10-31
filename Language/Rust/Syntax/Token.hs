{-# LANGUAGE DuplicateRecordFields #-}

module Language.Rust.Syntax.Token where

import Language.Rust.Syntax.Ident (Ident(..), InternedString, Name, SyntaxContext)
import Language.Rust.Data.Position

import Data.Word

------------------
-- Tokenization.
-- https://github.com/serde-rs/syntex/blob/master/syntex_syntax/src/parse/token.rs
------------------

-- https://docs.serde.rs/syntex_syntax/parse/token/enum.BinOpToken.html
data BinOpToken = Plus | Minus | Star | Slash | Percent | Caret | And | Or | Shl | Shr | LArrow deriving (Show)

-- | A delimiter token
-- https://docs.serde.rs/syntex_syntax/parse/token/enum.DelimToken.html
data DelimToken
  = Paren   -- ^ A round parenthesis: ( or )
  | Bracket -- ^ A square bracket: [ or ]
  | Brace   -- ^ A curly brace: { or }
  | NoDelim -- ^ An empty delimiter
  deriving (Eq, Enum, Bounded, Show)

-- https://docs.serde.rs/syntex_syntax/parse/token/enum.Lit.html
data Lit
  = Byte Name
  | Char Name
  | Integer Name
  | Float Name
  | Str_ Name
  | StrRaw Name Word64     -- ^ raw str delimited by n hash symbols
  | ByteStr Name
  | ByteStrRaw Name Word64 -- ^ raw byte str delimited by n hash symbols
  deriving (Show)

-- Based loosely on <https://docs.serde.rs/syntex_syntax/parse/token/enum.Token.html>
data Token
  -- Expression-operator symbols.
  = Eq | Lt | Le | EqEq | Ne | Ge | Gt | AndAnd | OrOr | Not | Tilde | BinOp BinOpToken | BinOpEq BinOpToken
  -- Structural symbols
  | At | Dot | DotDot | DotDotDot | Comma | Semi | Colon | ModSep | RArrow | FatArrow | Pound | Dollar | Question
  | OpenDelim DelimToken                -- ^ An opening delimiter, eg. `{`
  | CloseDelim DelimToken               -- ^ A closing delimiter, eg. `}`
  -- Literals
  | Literal Lit (Maybe Name)
  -- Strict Keywords (added)
  | As | Box | Break | Const | Continue | Crate | Else | Enum | Extern | False_ | Fn | For | If
  | Impl | In | Let | Loop | Match | Mod | Move | Mut | Pub | Ref | Return | SelfValue | SelfType | Static
  | Struct | Super | Trait | True_ | Type | Unsafe | Use | Where | While
  -- Keywords reserved for future use. (added)
  | Abstract | Alignof | Become | Do | Final | Macro | Offsetof | Override | Priv | Proc | Pure | Sizeof
  | Typeof | Unsized | Virtual | Yield
  -- Weak keywords, have special meaning only in specific contexts (added)
  | Default | StaticLifetime | Union
  -- Name components
  | IdentTok (Ident ())
  | Underscore
  | Lifetime (Ident ())
  -- NOT NEEDED IN TOKENIZATION!!
  -- For interpolation
  -- | Interpolated Nonterminal            -- ^ Can be expanded into several tokens.
  -- Doc comment
  | DocComment Name
  -- NOT NEEDED IN TOKENIZATION!!
  -- In left-hand-sides of MBE macros:
  -- | MatchNt Ident Ident                 -- ^ Parse a nonterminal (name to bind, name of NT)
  -- NOT NEEDED IN TOKENIZATION!!
  -- In right-hand-sides of MBE macros:
  -- | SubstNt Ident                       -- ^ A syntactic variable that will be filled in by macro expansion.
  -- | SpecialVarNt                        -- ^ A macro variable with special meaning.
  -- Junk. These carry no data because we don't really care about the data
  -- they *would* carry, and don't really want to allocate a new ident for
  -- them. Instead, users could extract that from the associated span.
  | Whitespace                          -- ^ Whitespace
  | Comment                             -- ^ Comment
  | Shebang Name
  | Eof
  deriving (Show)


canBeginExpr :: Token -> Bool
canBeginExpr OpenDelim{}   = True
canBeginExpr IdentTok{}    = True
canBeginExpr Underscore    = True
canBeginExpr Tilde         = True
canBeginExpr Literal{}     = True
canBeginExpr Not           = True
canBeginExpr (BinOp Minus) = True
canBeginExpr (BinOp Star)  = True
canBeginExpr (BinOp And)   = True
canBeginExpr (BinOp Or)    = True -- in lambda syntax
canBeginExpr OrOr          = True -- in lambda syntax
canBeginExpr AndAnd        = True -- double borrow
canBeginExpr DotDot        = True
canBeginExpr DotDotDot     = True -- range notation
canBeginExpr ModSep        = True
canBeginExpr Pound         = True -- for expression attributes
-- canBeginExpr (Interpolated NtExpr{}) = True
-- canBeginExpr (Interpolated NtIdent{}) = True
-- canBeginExpr (Interpolated NtBlock{}) = True
-- canBeginExpr (Interpolated NtPath{}) = True
canBeginExpr _ = False

