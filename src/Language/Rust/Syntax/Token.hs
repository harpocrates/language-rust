{-|
Module      : Language.Rust.Syntax.Token
Description : Token definitions
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Language.Rust.Syntax.Token (
  -- Contains roughly the same stuff as @syntax::parse::token@ - data definitions for tokens.
  Token(..), DocType(..), Space(..), Delim(..), LitTok(..)
) where

import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)

import Language.Rust.Syntax.Ident (Ident, Name)
import Language.Rust.Data.Position (Span)
import Language.Rust.Syntax.AST (Nonterminal)

-- | A general token (based on @syntax::parse::token::Token@).
--
-- Unlike its @libsyntax@ counterpart, 'Token' has folded in @syntax::parse::token::BinOpToken@
-- and @syntax::parse::token::BinOpEqToken@ as regular tokens.
data Token
  -- Single character expression-operator symbols.
  = Equal                 -- ^ @=@ token 
  | Less                  -- ^ @<@ token 
  | Greater               -- ^ @>@ token 
  | Ampersand             -- ^ @&@ token 
  | Pipe                  -- ^ @|@ token 
  | Exclamation           -- ^ @!@ token 
  | Tilde                 -- ^ @~@ token 
  | Plus                  -- ^ @+@ token 
  | Minus                 -- ^ @-@ token 
  | Star                  -- ^ @*@ token 
  | Slash                 -- ^ @/@ token 
  | Percent               -- ^ @%@ token 
  | Caret                 -- ^ @^@ token 
  
  -- Multi character expression-operator symbols
  | GreaterEqual          -- ^ @>=@ token
  | GreaterGreaterEqual   -- ^ @>>=@ token
  | AmpersandAmpersand    -- ^ @&&@ token
  | PipePipe              -- ^ @||@ token
  | LessLess              -- ^ @<<@ token
  | GreaterGreater        -- ^ @>>@ token
  | EqualEqual            -- ^ @==@ token
  | NotEqual              -- ^ @!=@ token
  | LessEqual             -- ^ @<=@ token
  | LessLessEqual         -- ^ @<<=@ token
  | MinusEqual            -- ^ @-=@ token
  | AmpersandEqual        -- ^ @&=@ token
  | PipeEqual             -- ^ @|=@ token
  | PlusEqual             -- ^ @+=@ token
  | StarEqual             -- ^ @*=@ token
  | SlashEqual            -- ^ @/=@ token
  | CaretEqual            -- ^ @^=@ token
  | PercentEqual          -- ^ @%=@ token
  
  -- Structural symbols  
  | At                    -- ^ @\@@ token
  | Dot                   -- ^ @.@ token
  | DotDot                -- ^ @..@ token
  | DotDotDot             -- ^ @...@ token
  | Comma                 -- ^ @,@ token
  | Semicolon             -- ^ @;@ token
  | Colon                 -- ^ @:@ token
  | ModSep                -- ^ @::@ token
  | RArrow                -- ^ @->@ token
  | LArrow                -- ^ @<-@ token
  | FatArrow              -- ^ @=>@ token
  | Pound                 -- ^ @#@ token
  | Dollar                -- ^ @$@ token
  | Question              -- ^ @?@ token
  
  -- Delimiters
  | OpenDelim !Delim      -- ^ One of @(@, @[@, @{@
  | CloseDelim !Delim     -- ^ One of @)@, @]@, @}@
  
  -- Literals
  | LiteralTok LitTok (Maybe Name) -- ^ a literal token with an optional suffix (something like @i32@)
  
  -- Name components
  | IdentTok Ident        -- ^ an arbitrary identifier (something like @x@ or @foo@ or @and_then@)
  | Underscore            -- ^ @_@ token
  | LifetimeTok Ident     -- ^ a lifetime (something like @\'a@ or @\'static@)
  | Space Space Name      -- ^ whitespace
  | Doc String !DocType   -- ^ doc comment with its contents and whether it is outer/inner
  | Shebang               -- ^ @#!@ shebang token
  | Eof                   -- ^ end of file token
  
  -- NOT PRODUCED IN TOKENIZATION!!
  | Interpolated (Nonterminal Span) -- ^ can be expanded into several tokens in macro-expansion
  deriving (Eq, Data, Typeable, Generic)


-- | Possible styles of doc comments:
--
-- @
-- /// Full line outer doc comment
-- //! Full line inner doc comment
-- \/** Inline outer doc comment *\/
-- \/*! Inline inner doc comment *\/
-- @
--
data DocType
  = OuterDoc -- ^ comment refers to element that follows immediately after
  | InnerDoc -- ^ comment refers to the closest enclosing element
  deriving (Eq, Show, Enum, Bounded, Data, Typeable, Generic)

-- | Rust is whitespace independent. Short of providing space between tokens, whitespace is all the
-- same to the parser.
data Space
  = Whitespace  -- ^ usual white space: @[\\ \\t\\n\\f\\v\\r]+@
  | Comment     -- ^ comment (either inline or not, see 'DocType')
  deriving (Eq, Show, Enum, Bounded, Data, Typeable, Generic)

-- | A delimiter token (@syntax::parse::token::DelimToken@)
data Delim
  = Paren   -- ^ round parenthesis: @(@ or @)@
  | Bracket -- ^ square bracket: @[@ or @]@
  | Brace   -- ^ curly brace: @{@ or @}@
  | NoDelim -- ^ empty delimiter
  deriving (Eq, Enum, Bounded, Show, Data, Typeable, Generic)

-- | A literal token (@syntax::parse::token::Lit@)
data LitTok
  = ByteTok Name            -- ^ byte
  | CharTok Name            -- ^ character
  | IntegerTok Name         -- ^ integral literal (could have type @i32@, @int@, @u128@, etc.)
  | FloatTok Name           -- ^ floating point literal (could have type @f32@, @f64@, etc.)
  | StrTok Name             -- ^ string literal
  | StrRawTok Name !Int     -- ^ raw string literal and the number of @#@ marks around it
  | ByteStrTok Name         -- ^ byte string literal
  | ByteStrRawTok Name !Int -- ^ raw byte string literal and the number of @#@ marks around it
  deriving (Eq, Show, Data, Typeable, Generic)


-- | This instance is only for error messages and debugging purposes.
instance Show Token where
  -- Single character expression-operator symbols.
  show Equal = "="
  show Less = "<"
  show Greater = ">"
  show Ampersand = "&"
  show Pipe = "|"
  show Exclamation = "!"
  show Tilde = "~"
  show Plus = "+"
  show Minus = "-"
  show Star = "*"
  show Slash = "/"
  show Percent = "%"
  -- Multi character eexpression-operator symbols
  show GreaterEqual = ">="
  show GreaterGreaterEqual = ">>="
  show AmpersandAmpersand = "&&"
  show PipePipe = "||"
  show LessLess = "<<"
  show GreaterGreater = ">>"
  show EqualEqual = "=="
  show NotEqual = "!="
  show LessEqual = "<="
  show LessLessEqual = "<<="
  show MinusEqual = "-="
  show AmpersandEqual = "&="
  show PipeEqual = "|="
  show PlusEqual = "+="
  show StarEqual = "*="
  show SlashEqual = "/="
  show CaretEqual = "^="
  show PercentEqual = "%="
  show Caret = "^"
  -- Structural symbols
  show At = "@"
  show Dot = "."
  show DotDot = ".."
  show DotDotDot = "..."
  show Comma = ","
  show Semicolon = ";"
  show Colon = ":"
  show ModSep = "::"
  show RArrow = "<-"
  show LArrow = "->"
  show FatArrow = "=>"
  show Pound = "#"
  show Dollar = "$"
  show Question = "?"
  -- Delimiters, eg. @{@, @]@, @(@
  show (OpenDelim Paren) = "("
  show (OpenDelim Bracket) = "["
  show (OpenDelim Brace) = "{"
  show (OpenDelim NoDelim) = ""
  show (CloseDelim Paren) = ")"
  show (CloseDelim Bracket) = "]"
  show (CloseDelim Brace) = "}"
  show (CloseDelim NoDelim) = ""
  -- Literals
  show (LiteralTok (ByteTok n) s) = "b'" ++ show n ++ "'" ++ maybe "" show s
  show (LiteralTok (CharTok n) s) = "'"  ++ show n ++ "'" ++ maybe "" show s
  show (LiteralTok (IntegerTok n) s) = show n ++ maybe "" show s
  show (LiteralTok (FloatTok n) s) = show n ++ maybe "" show s
  show (LiteralTok (StrTok n) s) = "\"" ++ show n ++ "\"" ++ maybe "" show s
  show (LiteralTok (StrRawTok n i) s) = "r" ++ replicate i '#' ++ "\"" ++ show n ++ "\"" ++ replicate i '#' ++ maybe "" show s
  show (LiteralTok (ByteStrTok n) s) = "b\"" ++ show n ++ "\"" ++ maybe "" show s
  show (LiteralTok (ByteStrRawTok n i) s) = "br" ++ replicate i '#' ++ "\"" ++ show n ++ "\"" ++ replicate i '#' ++ maybe "" show s
  -- Name components
  show (IdentTok i) = show i
  show Underscore = "_"
  show (LifetimeTok l) = "'" ++ show l
  show (Space Whitespace _) = "<whitespace>"
  show (Space Comment n) = "/*" ++ show n ++ " */"
  show (Doc d InnerDoc) = "/*!" ++ d ++ "*/"
  show (Doc d OuterDoc) = "/**" ++ d ++ "*/"
  show Shebang = "#!"
  show Eof = "<EOF>"
  -- Macro related 
  show Interpolated{} = "<Interpolated>"

