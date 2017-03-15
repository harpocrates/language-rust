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

module Language.Rust.Syntax.Ident (Ident(..), mkIdent, invalidIdent, Name) where

import Data.List (foldl')
import Data.Char (ord)
import Data.String (IsString(..))

-- | An identifier
data Ident
  = Ident { name :: Name  -- ^ payload of the identifier
          , hash :: !Int  -- ^ hash for quick comparision
          }

instance Show Ident where
  show = show . name

instance IsString Ident where
  fromString = mkIdent

-- | Uses 'hash' to short-circuit
instance Eq Ident where
  i1 == i2 = hash i1 == hash i2 && name i1 == name i2
  i1 /= i2 = hash i1 /= hash i2 || name i1 /= name i2

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

