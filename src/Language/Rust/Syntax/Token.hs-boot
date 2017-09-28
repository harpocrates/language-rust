-- .hs-boot files play badly with associated type families like 'Rep'
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-methods #-}
#endif

module Language.Rust.Syntax.Token where

import GHC.Generics (Generic)
import Data.Data (Data)
-- import Data.Typeable (Typeable)
import Control.DeepSeq (NFData)

data LitTok

data Token
instance Eq Token
instance Ord Token
instance Show Token
instance Data Token
-- instance Typeable Token
instance Generic Token
instance NFData Token

data Delim
instance Eq Delim
instance Ord Delim
instance Data Delim
-- instance Typeable Delim
instance Generic Delim
instance Show Delim
instance NFData Delim

data Space
instance NFData Space
instance Eq Space
instance Ord Space

