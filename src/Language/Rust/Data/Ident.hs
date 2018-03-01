{-|
Module      : Language.Rust.Data.Ident
Description : Identifiers
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Data structure behind identifiers.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Rust.Data.Ident (Ident(..), mkIdent, Name) where

import GHC.Generics       ( Generic )

import Control.DeepSeq    ( NFData )
import Data.Data          ( Data )
import Data.Typeable      ( Typeable )

import Data.List          ( foldl' )
import Data.Char          ( ord )
import Data.String        ( IsString(..) )
import Data.Semigroup     ( Semigroup(..) )

-- | An identifier
data Ident
  = Ident { name :: Name                 -- ^ payload of the identifier
          , hash :: {-# UNPACK #-} !Int  -- ^ hash for quick comparision
          } deriving (Data, Typeable, Generic, NFData)

instance Show Ident where
  show = name

instance IsString Ident where
  fromString = mkIdent

-- | Uses 'hash' to short-circuit
instance Eq Ident where
  i1 == i2 = hash i1 == hash i2 && name i1 == name i2
  i1 /= i2 = hash i1 /= hash i2 || name i1 /= name i2

-- | Uses 'hash' to short-circuit
instance Ord Ident where
  compare i1 i2 = case compare i1 i2 of
                    EQ -> compare (name i1) (name i2)
                    rt -> rt

instance Monoid Ident where
  mappend = (<>)
  mempty = mkIdent ""

instance Semigroup Ident where
  Ident n1 _ <> Ident n2 _ = mkIdent (n1 <> n2)


-- | Smart constructor for making an 'Ident'.
mkIdent :: String -> Ident
mkIdent s = Ident s (hashString s)

-- | Hash a string into an 'Int'
hashString :: String -> Int
hashString = foldl' f golden
   where f m c = fromIntegral (ord c) * magic + m
         magic = 0xdeadbeef
         golden = 1013904242

-- | The payload of an identifier
type Name = String

