 -- .hs-boot files play badly with type families like 'Rep'
{-# OPTIONS_GHC -w #-}

module Language.Rust.Syntax.Token where

import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)

data LitTok

data Token
instance Eq Token
instance Show Token
instance Data Token
-- instance Typeable Token
instance Generic Token

data Delim
instance Eq Delim
instance Data Delim
-- instance Typeable Delim
instance Generic Delim
instance Show Delim

data DocType
data Space

