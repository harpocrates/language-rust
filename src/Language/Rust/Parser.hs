{-|
Module      : Language.Rust.Parser
Description : Parsing and lexing
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

Selecting the right parser may require adding an annotation or using @-XTypeApplications@ to avoid
an "Ambiguous type variable" error.

Using 'Control.Monad.void' (as in the examples below) exploits the fact that most AST nodes are
instances of 'Functor' to discard the 'Span' annotation that is attached to most parsed AST nodes.
Conversely, if you wish to extract the 'Span' annotation, the 'Language.Rust.Syntax.AST.Located'
typeclass provides a 'Language.Rust.Syntax.AST.spanOf' method.

The examples below assume the following GHCi flags and imports:

>>> :set -XTypeApplications -XOverloadedStrings
>>> import Language.Rust.Syntax.AST
>>> import Control.Monad ( void )
>>> import System.IO

-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Rust.Parser (
  -- * Parsing
  parse,
  parse',
  readSourceFile,
  readTokens,
  Parse(..),
  P,
  execParser,
  execParserTokens,
  initPos,
  Span,

  -- * Lexing
  lexToken,
  lexNonSpace,
  lexTokens,
  translateLit,

  -- * Input stream
  readInputStream,
  hReadInputStream,
  inputStreamToString,
  inputStreamFromString,

  -- * Error reporting
  lexicalError,
  parseError,
  ParseFail(..),
) where

import Language.Rust.Syntax

import Language.Rust.Data.InputStream
import Language.Rust.Data.Position     ( Position, Span, Spanned, initPos )

import Language.Rust.Parser.Internal
import Language.Rust.Parser.Lexer      ( lexToken, lexNonSpace, lexTokens, lexicalError )
import Language.Rust.Parser.Literals   ( translateLit )
import Language.Rust.Parser.ParseMonad ( P, execParser, parseError, pushToken, ParseFail(..) )

import Control.Exception               ( throw )
import Data.Foldable                   ( traverse_ )
import System.IO                       ( Handle )

-- | Parse something from an input stream (it is assumed the initial position is 'initPos').
--
-- >>> fmap void $ parse @(Expr Span) "x + 1"
-- Right (Binary [] AddOp (PathExpr [] Nothing (Path False [PathSegment "x" Nothing ()] ()) ())
--                        (Lit [] (Int Dec 1 Unsuffixed ()) ())
--                        ())
--
-- >>> fmap void $ parse @(Expr Span) "x + "
-- Left (parse failure at 1:4 (Syntax error: unexpected `<EOF>' (expected an expression)))
--
parse :: Parse a => InputStream -> Either ParseFail a
parse is = execParser parser is initPos

-- | Same as 'parse', but throws a 'ParseFail' exception if it cannot parse. This function is
-- intended for situations in which you are already stuck catching exceptions - otherwise you should
-- prefer 'parse'.
--
-- >>> void $ parse' @(Expr Span) "x + 1"
-- Binary [] AddOp (PathExpr [] Nothing (Path False [PathSegment "x" Nothing ()] ()) ())
--                 (Lit [] (Int Dec 1 Unsuffixed ()) ())
--                 ()
--
-- >>> void $ parse' @(Expr Span) "x + "
-- *** Exception: parse failure at 1:4 (Syntax error: unexpected `<EOF>' (expected an expression))
--
parse' :: Parse a => InputStream -> a
parse' = either throw id . parse

-- | Same as 'execParser', but working from a list of tokens instead of an 'InputStream'.
execParserTokens :: P a -> [Spanned Token] -> Position -> Either ParseFail a
execParserTokens p toks = execParser (pushTokens toks *> p) (inputStreamFromString "")
  where pushTokens = traverse_ pushToken . reverse

-- | Given a handle to a Rust source file, read that file and parse it into a 'SourceFile'
--
-- >>> writeFile "empty_main.rs" "fn main() { }"
-- >>> fmap void $ withFile "empty_main.rs" ReadMode readSourceFile
-- SourceFile Nothing [] [Fn [] InheritedV "main"
--                           (FnDecl [] Nothing False ())
--                           Normal NotConst Rust
--                           (Generics [] [] (WhereClause [] ()) ())
--                           (Block [] Normal ()) ()]
--
readSourceFile :: Handle -> IO (SourceFile Span)
readSourceFile hdl = parse' <$> hReadInputStream hdl

-- | Given a path pointing to a Rust source file, read that file and lex it (ignoring whitespace)
--
-- >>> writeFile "empty_main.rs" "fn main() { }"
-- >>> withFile "empty_main.rs" ReadMode readTokens
-- [fn,main,(,),{,}]
--
readTokens :: Handle -> IO [Spanned Token]
readTokens hdl = do
  inp <- hReadInputStream hdl
  case execParser (lexTokens lexNonSpace) inp initPos of
    Left pf -> throw pf
    Right x -> pure x

-- | Describes things that can be parsed
class Parse a where
  -- | Complete parser (fails if not all of the input is consumed)
  parser :: P a

instance Parse (Lit Span)         where parser = parseLit
instance Parse (Attribute Span)   where parser = parseAttr
instance Parse (Ty Span)          where parser = parseTy 
instance Parse (Pat Span)         where parser = parsePat
instance Parse (Expr Span)        where parser = parseExpr
instance Parse (Stmt Span)        where parser = parseStmt
instance Parse (Item Span)        where parser = parseItem
instance Parse (SourceFile Span)  where parser = parseSourceFile
instance Parse TokenTree          where parser = parseTt
instance Parse TokenStream        where parser = parseTokenStream
instance Parse (Block Span)       where parser = parseBlock
instance Parse (ImplItem Span)    where parser = parseImplItem 
instance Parse (TraitItem Span)   where parser = parseTraitItem
instance Parse (TyParam Span)     where parser = parseTyParam
instance Parse (LifetimeDef Span) where parser = parseLifetimeDef
instance Parse (Generics Span)    where parser = parseGenerics
instance Parse (WhereClause Span) where parser = parseWhereClause
