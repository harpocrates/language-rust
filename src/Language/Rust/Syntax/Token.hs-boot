module Language.Rust.Syntax.Token where

data LitTok

data Token
instance Eq Token
instance Show Token

data Delim
instance Eq Delim
instance Show Delim

data DocType
data Space

