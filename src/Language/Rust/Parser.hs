{-# LANGUAGE FlexibleInstances #-}

module Language.Rust.Parser (Parse(..), P, execParser) where

import Language.Rust.Syntax.AST
import Language.Rust.Data.Position
import Language.Rust.Parser.ParseMonad
import Language.Rust.Parser.Parser

class Parse a where parser :: P a
instance Parse (Lit Span) where parser = literalP
instance Parse (Attribute Span) where parser = attributeP
instance Parse (Ty Span) where parser = typeP
instance Parse (Pat Span) where parser = patternP
instance Parse (Expr Span) where parser = expressionP


