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
  parse, parse', readSourceFile, readTokens, Parse(..), P, execParser, execParserTokens, initPos, Span,
  -- * Lexing
  lexToken, lexNonSpace, lexTokens, translateLit,
  -- * Input stream
  readInputStream, inputStreamToString, inputStreamFromString,
  -- * Error reporting
  lexicalError, parseError, ParseFail,
) where

import Language.Rust.Syntax
import Language.Rust.Data.InputStream (InputStream, readInputStream, inputStreamToString, inputStreamFromString)
import Language.Rust.Data.Position (Position, Span, Spanned, initPos, prettyPosition)
import Language.Rust.Parser.Internal
import Language.Rust.Parser.Lexer (lexToken, lexNonSpace, lexTokens, lexicalError)
import Language.Rust.Parser.Literals (translateLit)
import Language.Rust.Parser.ParseMonad (P, execParser, parseError, pushToken)

import Data.Typeable (Typeable)
import Control.Exception (Exception, throw) 
import Data.Foldable (traverse_)

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

-- | Same as 'execParser', but working from a list of tokens instead of an 'InputStream'.
execParserTokens :: P a -> [Spanned Token] -> Position -> Either (Position,String) a
execParserTokens p toks = execParser (pushTokens toks *> p) (inputStreamFromString "")
  where pushTokens = traverse_ pushToken . reverse

-- | Given a path pointing to a Rust source file, read that file and parse it into a 'SourceFile'
readSourceFile :: FilePath -> IO (SourceFile Span)
readSourceFile fileName = parse' <$> readInputStream fileName

-- | Given a path pointing to a Rust source file, read that file and lex it (ignoring whitespace)
readTokens :: FilePath -> IO [Spanned Token]
readTokens fileName = do
  inp <- readInputStream fileName
  case execParser (lexTokens lexNonSpace) inp initPos of
    Left (pos, msg) -> throw (ParseFail pos msg)
    Right x -> pure x

-- | Exceptions that occur during parsing
data ParseFail = ParseFail Position String deriving (Eq, Typeable)

instance Show ParseFail where
  show (ParseFail pos msg) = unwords [ "parse failure at", prettyPosition pos, "(" ++ msg ++ ")" ]

instance Exception ParseFail
  

-- | Describes things that can be parsed
class Parse a where
  -- | Complete parser (fails if not all of the input is consumed)
  parser :: P a

instance Parse (Lit Span) where
  parser = parseLit

instance Parse (Attribute Span) where
  parser = parseAttr

instance Parse (Ty Span) where
  parser = parseTy 

instance Parse (Pat Span) where
  parser = parsePat

instance Parse (Expr Span) where
  parser = parseExpr

instance Parse (Stmt Span) where
  parser = parseStmt

instance Parse (Item Span) where
  parser = parseItem

instance Parse (SourceFile Span) where
  parser = parseSourceFile

instance Parse TokenTree where
  parser = parseTt

instance Parse TokenStream where
  parser = parseTokenStream

instance Parse (Block Span) where
  parser = parseBlock

instance Parse (ImplItem Span) where
  parser = parseImplItem 

instance Parse (TraitItem Span) where
  parser = parseTraitItem

instance Parse (TyParam Span) where
  parser = parseTyParam

instance Parse (LifetimeDef Span) where
  parser = parseLifetimeDef

instance Parse (Generics Span) where
  parser = parseGenerics

instance Parse (WhereClause Span) where
  parser = parseWhereClause

