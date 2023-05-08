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
import Data.Semigroup as Sem

-- | An identifier.
-- Note that the order of the fields is important, so the
-- when we derive `Eq` and `Ord` we use the hash first.
data Ident
  = Ident { hash :: {-# UNPACK #-} !Int  -- ^ hash for quick comparision
          , name :: Name                 -- ^ payload of the identifier
          , raw :: Bool                  -- ^ whether the identifier is raw
          } deriving (Data, Typeable, Generic, NFData, Eq, Ord)

-- | Shows the identifier as a string (for use with @-XOverloadedStrings@)
instance Show Ident where
  show = show . name

instance IsString Ident where
  fromString = mkIdent

-- | "Forgets" about whether either argument was raw
instance Monoid Ident where
  mappend = (<>)
  mempty = mkIdent ""

-- | "Forgets" about whether either argument was raw
instance Sem.Semigroup Ident where
  i1 <> i2 = mkIdent (name i1 <> name i2)


-- | Smart constructor for making an 'Ident'.
mkIdent :: String -> Ident
mkIdent s = Ident { hash = hashString s
                  , name = s
                  , raw = False
                  }

-- | Hash a string into an 'Int'
hashString :: String -> Int
hashString = foldl' f golden
   where f m c = fromIntegral (ord c) * magic + m
         magic = 0xdeadbeef
         golden = 1013904242

-- | The payload of an identifier
type Name = String

