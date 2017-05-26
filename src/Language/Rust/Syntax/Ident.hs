{-|
Module      : Language.Rust.Syntax.Ident
Description : Identifiers
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Data structure behind identifiers.
-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveAnyClass #-}

module Language.Rust.Syntax.Ident (Ident(..), mkIdent, invalidIdent, Name) where

import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)
import Control.DeepSeq (NFData)

import Data.List (foldl')
import Data.Char (ord)
import Data.String (IsString(..))
import Data.Semigroup

-- | An identifier
data Ident
  = Ident { name :: Name  -- ^ payload of the identifier
          , hash :: !Int  -- ^ hash for quick comparision
          } deriving (Data, Typeable, Generic, NFData)

instance Show Ident where
  show = name

instance IsString Ident where
  fromString = mkIdent

-- | Uses 'hash' to short-circuit
instance Eq Ident where
  i1 == i2 = hash i1 == hash i2 && name i1 == name i2
  i1 /= i2 = hash i1 /= hash i2 || name i1 /= name i2

instance Monoid Ident where
  Ident n1 _ `mappend` Ident n2 _ = mkIdent (n1 `mappend` n2)
  mempty = invalidIdent

instance Semigroup Ident

-- | Smart constructor for making an 'Ident'.
mkIdent :: String -> Ident
mkIdent s = Ident s (hashString s)

-- | Hash a string into an 'Int'
hashString :: String -> Int
hashString = foldl' f golden
   where f m c = fromIntegral (ord c) * magic + m
         magic = 0xdeadbeef
         golden = 1013904242

-- | The empty identifier is invalid
invalidIdent :: Ident
invalidIdent = mkIdent ""

-- | TODO: backpack
type Name = String

