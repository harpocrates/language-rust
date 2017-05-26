{-|
Module      : Language.Rust.Parser.Reversed
Description : Parsing literals
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Datatypes wrapping lists and non-empty lists designed for fast append (as opposed to prepend) 
along with the usual class instances.
-}
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}

module Language.Rust.Parser.Reversed (
  Reversed(..), toNonEmpty
) where

import Data.Foldable
import qualified Data.List.NonEmpty as N
import qualified GHC.Exts as G

-- | Wrap a data type where all the operations are reversed
newtype Reversed f a = Reversed (f a)

instance Foldable (Reversed []) where
  foldMap f (Reversed xs) = foldMap f (reverse xs)
  toList (Reversed xs) = reverse xs

instance Foldable (Reversed N.NonEmpty) where
  foldMap f (Reversed xs) = foldMap f (N.reverse xs)
  toList (Reversed xs) = reverse (toList xs)

instance Monoid (f a) => Monoid (Reversed f a) where
  mempty = Reversed mempty
  mappend (Reversed xs) (Reversed ys) = Reversed (mappend ys xs)

instance G.IsList (f a) => G.IsList (Reversed f a) where
  type Item (Reversed f a) = G.Item (f a)
  fromList xs = Reversed (G.fromList (reverse xs))
  toList (Reversed xs) = reverse (G.toList xs)

-- | Convert a reversed 'N.NonEmpty' back into a normal one.
toNonEmpty :: Reversed N.NonEmpty a -> N.NonEmpty a
toNonEmpty (Reversed xs) = N.reverse xs
  


