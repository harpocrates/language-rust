{-# LANGUAGE KindSignatures #-}

module Language.Rust.Syntax.Token where

data DelimToken
data LitTok
data TokenSpace (s :: * -> *)
data Token

data DocType
data Space
data IdentStyle

canBeginExpr :: Token -> Bool

