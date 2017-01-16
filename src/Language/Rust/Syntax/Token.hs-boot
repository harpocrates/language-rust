{-# LANGUAGE KindSignatures #-}

module Language.Rust.Syntax.Token where

data LitTok
data TokenSpace (s :: * -> *)

data Token
instance Eq Token
instance Show Token

data DelimToken
instance Eq DelimToken
instance Show DelimToken

data DocType
data Space
data IdentStyle

canBeginExpr :: Token -> Bool

