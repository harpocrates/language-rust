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
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, TypeFamilies #-}
#if __GLASGOW_HASKELL__ < 800
{-# LANGUAGE FlexibleContexts #-}
#endif

module Language.Rust.Parser.Reversed (
  Reversed(..), toNonEmpty, unsnoc
) where

import Data.Foldable
import Data.Semigroup
import qualified Data.List.NonEmpty as N
import qualified GHC.Exts as G

import Language.Rust.Data.Position

-- | Wrap a data type where all the operations are reversed
newtype Reversed f a = Reversed (f a)

instance Functor f => Functor (Reversed f) where
  fmap f (Reversed xs) = Reversed (fmap f xs)

instance Foldable (Reversed []) where
  foldMap f (Reversed xs) = foldMap f (reverse xs)
  toList (Reversed xs) = reverse xs

instance Foldable (Reversed N.NonEmpty) where
  foldMap f (Reversed xs) = foldMap f (N.reverse xs)
  toList (Reversed xs) = reverse (toList xs)

instance Semigroup (f a) => Semigroup (Reversed f a) where
  Reversed xs <> Reversed ys = Reversed (ys <> xs)

instance Monoid (f a) => Monoid (Reversed f a) where
  mempty = Reversed mempty
  mappend (Reversed xs) (Reversed ys) = Reversed (mappend ys xs)

instance G.IsList (f a) => G.IsList (Reversed f a) where
  type Item (Reversed f a) = G.Item (f a)
  fromList xs = Reversed (G.fromList (reverse xs))
  toList (Reversed xs) = reverse (G.toList xs)

instance Located (f a) => Located (Reversed f a) where
  spanOf (Reversed xs) = spanOf xs

-- | Convert a reversed 'N.NonEmpty' back into a normal one.
{-# INLINE toNonEmpty #-}
toNonEmpty :: Reversed N.NonEmpty a -> N.NonEmpty a
toNonEmpty (Reversed xs) = N.reverse xs

-- | Remove an element from the end of a non-empty reversed sequence
{-# INLINE unsnoc #-}
unsnoc :: Reversed N.NonEmpty a -> (Reversed [] a, a)
unsnoc (Reversed (x N.:| xs)) = (Reversed xs, x)
