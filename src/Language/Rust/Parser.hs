{-|
Module      : Language.Rust.Parser
Description : Parsing and lexing
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Selecting the right parser may require adding an annotation to avoid an 'Ambiguous type variable'
error.

>>> :set -XTypeApplications
>>> import Language.Rust.Syntax
>>> import Language.Rust.Parser
>>> inp <- readInputStream "hello_world.rs"
inp :: InputStream
>>> Right sourceFile = parse @(SourceFile Span) inp
sourceFile :: SourceFile Span

-}
{-# LANGUAGE FlexibleInstances, DeriveDataTypeable #-}

module Language.Rust.Parser (
  -- * Parsing
  parse, parse', Parse(..), P, execParser, initPos, Span,
  -- * Lexing
  lexToken, lexNonSpace, lexTokens, translateLit,
  -- * Input stream
  readInputStream, inputStreamToString, inputStreamFromString,
  -- * Error reporting
  lexicalError, parseError, ParseFail,
) where

import Language.Rust.Syntax.AST
import Language.Rust.Data.InputStream (InputStream, readInputStream, inputStreamToString, inputStreamFromString)
import Language.Rust.Data.Position (Position, Span, initPos, prettyPosition)
import Language.Rust.Parser.Internal
import Language.Rust.Parser.Lexer (lexToken, lexNonSpace, lexTokens, lexicalError)
import Language.Rust.Parser.Literals (translateLit)
import Language.Rust.Parser.ParseMonad (P, execParser, parseError)

import Data.Typeable (Typeable)
import Control.Exception (Exception, throw) 

-- | Parse something from an input stream (it is assumed the initial position is 'initPos')
parse :: Parse a => InputStream -> Either (Position,String) a
parse is = execParser parser is initPos

-- | Same as 'parse', but throws a 'ParseFail' exception if it cannot parse. This function is
-- intended for situations in which you are already stuck catching exceptions - otherwise you should
-- prefer 'parse'.
parse' :: Parse a => InputStream -> a
parse' is = case execParser parser is initPos of
              Left (pos, msg) -> throw (ParseFail pos msg)
              Right x -> x

-- | Exceptions that occur during parsing
data ParseFail = ParseFail Position String deriving (Eq, Typeable)

instance Show ParseFail where
  show (ParseFail pos msg) = unwords [ "parse failure at", prettyPosition pos, "(" ++ msg ++ ")" ]

instance Exception ParseFail
  

-- | Describes things that can be parsed
class Parse a where
  parser :: P a

instance Parse (Lit Span) where parser = parseLit
instance Parse (Attribute Span) where parser = parseAttr
instance Parse (Ty Span) where parser = parseTy 
instance Parse (Pat Span) where parser = parsePat
instance Parse (Expr Span) where parser = parseExpr
instance Parse (Stmt Span) where parser = parseStmt
instance Parse (Item Span) where parser = parseItem
instance Parse (SourceFile Span) where parser = parseSourceFile
instance Parse TokenTree where parser = parseTt
instance Parse (Block Span) where parser = parseBlock
instance Parse (ImplItem Span) where parser = parseImplItem 
instance Parse (TraitItem Span) where parser = parseTraitItem

