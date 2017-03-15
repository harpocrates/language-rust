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

@
ghci> import Language.Rust.Syntax.AST
ghci> import Language.Data.Position (Span)
ghci> import Language.Rust.Parser
ghci> inp <- readInputStream "hello_world.rs"
inp :: InputStream
ghci> Right crateAst = execParser (parser :: P (Crate Span)) inp initPos
crateAst :: Crate Span
@
-}

{-# LANGUAGE FlexibleInstances #-}

module Language.Rust.Parser (
  -- * Parsing
  Parse(..), P, execParser, initPos,
  -- * Lexing
  lexToken, lexNonSpace, lexTokens,
  -- * Input stream
  readInputStream, inputStreamToString, inputStreamFromString,
  -- * Error reporting
  lexicalError, parseError,
) where

import Language.Rust.Syntax.AST
import Language.Rust.Parser.ParseMonad (P, execParser, parseError)
import Language.Rust.Data.Position (Span, initPos)
import Language.Rust.Parser.Lexer (lexToken, lexNonSpace, lexTokens, lexicalError)
import Language.Rust.Data.InputStream (readInputStream, inputStreamToString, inputStreamFromString)
import Language.Rust.Parser.Internal

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
instance Parse (Crate Span) where parser = parseCrate
instance Parse TokenTree where parser = parseTt
instance Parse (Block Span) where parser = parseBlock
instance Parse (ImplItem Span) where parser = parseImplItem 
instance Parse (TraitItem Span) where parser = parseTraitItem

