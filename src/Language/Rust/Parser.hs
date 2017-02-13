{-# LANGUAGE FlexibleInstances #-}

module Language.Rust.Parser (
  -- * Parsing
  Parse(..), P, execParser
) where

import Language.Rust.Syntax.AST
import Language.Rust.Data.Position
import Language.Rust.Parser.ParseMonad
import Language.Rust.Parser.Parser

-- | Describes things that can be parsed
class Parse a where
  -- | Parser that can be run using 'execParser'
  parser :: P a

instance Parse (Lit Span) where parser = literalP
instance Parse (Attribute Span) where parser = attributeP
instance Parse (Ty Span) where parser = typeP
instance Parse (Pat Span) where parser = patternP
instance Parse (Expr Span) where parser = expressionP

