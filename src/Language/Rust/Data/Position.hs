{-|
Module      : Language.Rust.Data.Position
Description : Positions and spans in files
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Everything to do with describing a position or a contiguous region in a file.
-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Language.Rust.Data.Position (
  -- * Positions in files
  Position(..), maxPos, minPos, initPos, incPos, retPos, incOffset,
  -- * Spans in files
  Span(..), subsetOf, Spanned(..), Located(..),
) where

import GHC.Generics (Generic)
import Data.Data (Data)
import Data.Typeable (Typeable)

import Data.Ord (comparing)
import Data.List (maximumBy, minimumBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))


-- | A position in a source file. The row and column information is kept only for its convenience
-- and human-readability. Analogous to the information encoded in a cursor.
data Position = Position {
    absoluteOffset :: {-# UNPACK #-} !Int, -- ^ absolute offset the source file.
    row :: {-# UNPACK #-} !Int,            -- ^ row (line) in the source file.
    col :: {-# UNPACK #-} !Int             -- ^ column in the source file.
  }
  | NoPosition
  deriving (Eq, Data, Typeable, Generic)

instance Show Position where
  show NoPosition = "$"
  show (Position _ r c) = show r ++ ":" ++ show c

-- | Maximum of two positions, bias for actual positions.
--
-- >>> maxPos (Position 30 5 8) (Position 37 5 15)
-- Position 37 5 15
--
-- >>> maxPos NoPosition (Position 30 5 8)
-- Position 30 5 8
--
maxPos :: Position -> Position -> Position
maxPos NoPosition p2 = p2
maxPos p1 NoPosition = p1
maxPos p1 p2 = maximumBy (comparing absoluteOffset) [p1,p2]

-- | Maximum and minimum positions, bias for actual positions.
--
-- >>> minPos (Position 30 5 8) (Position 37 5 15)
-- Position 30 5 8
--
-- >>> minPos NoPosition (Position 30 5 8)
-- Position 30 5 8
--
minPos :: Position -> Position -> Position
minPos NoPosition p2 = p2
minPos p1 NoPosition = p1
minPos p1 p2 = minimumBy (comparing absoluteOffset) [p1,p2]

-- | Starting position in a file.
initPos :: Position
initPos = Position 0 1 0

-- | Advance columnn a certain number of times.
incPos :: Position -> Int -> Position
incPos NoPosition _ = NoPosition
incPos p@Position{ absoluteOffset = a, col = c } offset = p { absoluteOffset = a + offset, col = c + offset }

-- | Advance to the next line.
retPos :: Position -> Position
retPos NoPosition = NoPosition
retPos (Position a r _) = Position { absoluteOffset = a + 1, row = r + 1, col = 1 }

-- | Advance only the absolute offset, not the row and column information. Only use this if you
-- know what you are doing!
incOffset :: Position -> Int -> Position
incOffset NoPosition _ = NoPosition
incOffset p@Position{ absoluteOffset = a } offset = p { absoluteOffset = a + offset }


-- | Spans represent a contiguous region of code, delimited by two 'Position's. The endpoints are
-- inclusive. Analogous to the information encoded in a selection.
data Span = Span { lo :: Position, hi :: Position } deriving (Eq, Data, Typeable, Generic)

-- | Check if a span is a subset of another span
subsetOf :: Span -> Span -> Bool
Span l1 h1 `subsetOf` Span l2 h2 = minPos l1 l2 == l1 && maxPos h1 h2 == h2

-- | smallest covering 'Span'
instance Semigroup Span where
  Span l1 h1 <> Span l2 h2 = Span (minPos l1 l2) (maxPos h1 h2)

instance Monoid Span where
  mempty = Span NoPosition NoPosition
  mappend = (<>) 

instance Show Span where
  show (Span lo' hi') = show lo' ++ " - " ++ show hi'

-- | A "tagging" of something with a 'Span' that describes its extent.
data Spanned a = Spanned { unspan :: a, span :: Span } deriving (Data, Typeable, Generic)

instance Functor Spanned where
  fmap f (Spanned x s) = Spanned (f x) s

instance Applicative Spanned where
  pure x = Spanned x mempty
  Spanned f s1 <*> Spanned x s2 = Spanned (f x) (s1 <> s2)

instance Monad Spanned where
  return = pure
  Spanned x s1 >>= f = let Spanned y s2 = f x in Spanned y (s1 <> s2) 

instance Show a => Show (Spanned a) where
  show (Spanned p s) = "at " ++ show p ++ ": " ++ show s


-- | Describes nodes that can be located - their span can be extracted from them. In general, we
-- expect that for a value constructed as @Con x y z@ where @Con@ is an arbitrary constructor
--
-- prop> (spanOf x <> spanOf y <> spanOf z) `subsetOf` spanOf (Con x y z) == True
--
class Located a where
  spanOf :: a -> Span

instance Located Span where
  spanOf = id

instance Located (Spanned a) where
  spanOf (Spanned _ s) = s

instance Located a => Located [a] where
  spanOf = foldMap spanOf

instance Located a => Located (NonEmpty a) where
  spanOf = foldMap spanOf

