{-|
Module      : Language.Rust.Syntax.Token
Description : Token definitions
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}

module Language.Rust.Syntax.Token (
  -- Contains roughly the same stuff as @syntax::parse::token@ - data definitions for tokens.
  Token(..), spaceNeeded, Space(..), Delim(..), LitTok(..), AttrStyle(..)
) where

import GHC.Generics (Generic)
import Data.Maybe (fromMaybe)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData)

import Language.Rust.Syntax.Ident (Ident, Name)
import Language.Rust.Data.Position (Span)
import Language.Rust.Syntax.AST (Nonterminal, AttrStyle(..))

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
  -- ^ doc comment with its contents, whether it is outer/inner, and whether it is inline or not
  | Doc String !AttrStyle !Bool
  | Shebang               -- ^ @#!@ shebang token
  | Eof                   -- ^ end of file token
  
  -- NOT PRODUCED IN TOKENIZATION!!
  | Interpolated (Nonterminal Span) -- ^ can be expanded into several tokens in macro-expansion
  deriving (Eq, Data, Typeable, Generic, NFData)

-- | Rust is whitespace independent. Short of providing space between tokens, whitespace is all the
-- same to the parser.
data Space
  = Whitespace  -- ^ usual white space: @[\\ \\t\\n\\f\\v\\r]+@
  | Comment     -- ^ comment (either inline or not)
  deriving (Eq, Show, Enum, Bounded, Data, Typeable, Generic, NFData)

-- | A delimiter token (@syntax::parse::token::DelimToken@)
data Delim
  = Paren   -- ^ round parenthesis: @(@ or @)@
  | Bracket -- ^ square bracket: @[@ or @]@
  | Brace   -- ^ curly brace: @{@ or @}@
  | NoDelim -- ^ empty delimiter             -- TODO: BANISH! (or rather: distinguish DelimToken from Delim, as rustc does)
  deriving (Eq, Enum, Bounded, Show, Data, Typeable, Generic, NFData)

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
  deriving (Eq, Show, Data, Typeable, Generic, NFData)


-- | Check whether a space is needed between two tokens to avoid confusion
spaceNeeded :: Token -> Token -> Bool
-- conflicts with 'GreaterEqual'
spaceNeeded Greater Equal = True
spaceNeeded Greater EqualEqual = True
spaceNeeded Greater FatArrow = True

-- conflicts with 'GreaterGreaterEqual'
spaceNeeded Greater GreaterEqual = True
spaceNeeded GreaterGreater Equal = True
spaceNeeded GreaterGreater EqualEqual = True 
spaceNeeded GreaterGreater FatArrow = True

-- conflicts with 'AmpersandAmpersand'
spaceNeeded Ampersand Ampersand = True
spaceNeeded Ampersand AmpersandAmpersand = True
spaceNeeded Ampersand AmpersandEqual = True

-- conflicts with 'PipePipe'
spaceNeeded Pipe Pipe = True
spaceNeeded Pipe PipePipe = True
spaceNeeded Pipe PipeEqual = True

-- conflicts with 'LessLess'
spaceNeeded Less Less = True
spaceNeeded Less LessLess = True
spaceNeeded Less LessLessEqual = True
spaceNeeded Less LArrow = True

-- conflicts with 'GreaterGreater'
spaceNeeded Greater Greater = True
spaceNeeded Greater GreaterGreater = True
spaceNeeded Greater GreaterGreaterEqual = True

-- conflicts with 'EqualEqual'
spaceNeeded Equal Equal = True
spaceNeeded Equal EqualEqual = True
spaceNeeded Equal FatArrow = True

-- conflicts with 'NotEqual'
spaceNeeded Exclamation Equal = True
spaceNeeded Exclamation EqualEqual = True
spaceNeeded Exclamation FatArrow = True

-- conflicts with 'LessEqual'
spaceNeeded Less Equal = True
spaceNeeded Less EqualEqual = True
spaceNeeded Less FatArrow = True

-- conflicts with 'LessLessEqual'
spaceNeeded Less LessEqual = True
spaceNeeded LessLess Equal = True
spaceNeeded LessLess EqualEqual = True
spaceNeeded LessLess FatArrow = True

-- conflicts with 'MinusEqual'
spaceNeeded Minus Equal = True
spaceNeeded Minus EqualEqual = True
spaceNeeded Minus FatArrow = True

-- conflicts with 'AmpersandEqual'
spaceNeeded Ampersand Equal = True
spaceNeeded Ampersand EqualEqual = True
spaceNeeded Ampersand FatArrow = True

-- conflicts with 'PipeEqual'
spaceNeeded Pipe Equal = True
spaceNeeded Pipe EqualEqual = True
spaceNeeded Pipe FatArrow = True

-- conflicts with 'PlusEqual'
spaceNeeded Plus Equal = True
spaceNeeded Plus EqualEqual = True
spaceNeeded Plus FatArrow = True

-- conflicts with 'StarEqual'
spaceNeeded Star Equal = True
spaceNeeded Star EqualEqual = True
spaceNeeded Star FatArrow = True

-- conflicts with 'SlashEqual'
spaceNeeded Slash Equal = True
spaceNeeded Slash EqualEqual = True
spaceNeeded Slash FatArrow = True

-- conflicts with 'CaretEqual'
spaceNeeded Caret Equal = True
spaceNeeded Caret EqualEqual = True
spaceNeeded Caret FatArrow = True

-- conflicts with 'PercentEqual'
spaceNeeded Percent Equal = True
spaceNeeded Percent EqualEqual = True
spaceNeeded Percent FatArrow = True

-- conflicts with 'DotDot'
spaceNeeded Dot Dot = True
spaceNeeded Dot DotDot = True
spaceNeeded Dot DotDotDot = True

-- conflicts with 'DotDotDot'
spaceNeeded DotDot Dot = True
spaceNeeded DotDot DotDot = True
spaceNeeded DotDot DotDotDot = True

-- conflicts with 'ModSep'
spaceNeeded Colon Colon = True
spaceNeeded Colon ModSep = True

-- conflicts with 'RArrow'
spaceNeeded Minus Greater = True
spaceNeeded Minus GreaterGreater = True
spaceNeeded Minus GreaterEqual = True
spaceNeeded Minus GreaterGreaterEqual = True

-- conflicts with 'LArrow'
spaceNeeded Less Minus = True
spaceNeeded Less MinusEqual = True
spaceNeeded Less RArrow = True

-- conflicts with 'FatArrow'
spaceNeeded Equal Greater = True
spaceNeeded Equal GreaterGreater = True
spaceNeeded Equal GreaterEqual = True
spaceNeeded Equal GreaterGreaterEqual = True

-- conflicts with 'LiteralTok'
spaceNeeded LiteralTok{} IdentTok{} = True 
spaceNeeded LiteralTok{} Underscore = True

-- conflicts with 'IdentTok'
spaceNeeded IdentTok{} IdentTok{} = True
spaceNeeded IdentTok{} Underscore = True

-- conflicts with 'Shebang'
spaceNeeded Pound Exclamation = True
spaceNeeded Pound NotEqual = True

-- there are no other conflicts
spaceNeeded _ _ = False


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
  show RArrow = "->"
  show LArrow = "<-"
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
  show (LiteralTok (ByteTok n) s) = "b'" ++ n ++ "'" ++ fromMaybe "" s
  show (LiteralTok (CharTok n) s) = "'"  ++ n ++ "'" ++ fromMaybe "" s
  show (LiteralTok (IntegerTok n) s) = n ++ fromMaybe "" s
  show (LiteralTok (FloatTok n) s) = n ++ fromMaybe "" s
  show (LiteralTok (StrTok n) s) = "\"" ++ n ++ "\"" ++ fromMaybe "" s
  show (LiteralTok (StrRawTok n i) s) = "r" ++ replicate i '#' ++ "\"" ++ n ++ "\"" ++ replicate i '#' ++ fromMaybe "" s
  show (LiteralTok (ByteStrTok n) s) = "b\"" ++ n ++ "\"" ++ fromMaybe "" s
  show (LiteralTok (ByteStrRawTok n i) s) = "br" ++ replicate i '#' ++ "\"" ++ n ++ "\"" ++ replicate i '#' ++ fromMaybe "" s
  -- Name components
  show (IdentTok i) = show i
  show Underscore = "_"
  show (LifetimeTok l) = "'" ++ show l
  show (Space Whitespace _) = "<whitespace>"
  show (Space Comment n) = "/*" ++ show n ++ " */"
  show (Doc d Inner True) = "/*!" ++ d ++ "*/"
  show (Doc d Outer True) = "/**" ++ d ++ "*/"
  show (Doc d Inner False) = "//!" ++ d
  show (Doc d Outer False) = "///" ++ d
  show Shebang = "#!"
  show Eof = "<EOF>"
  -- Macro related 
  show Interpolated{} = "<Interpolated>"

