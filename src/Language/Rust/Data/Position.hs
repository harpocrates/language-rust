{-|
Module      : Language.Rust.Data.Position
Description : Positions and spans in files
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

Everything to do with describing a position or a contiguous region in a file.
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Language.Rust.Data.Position (
  -- * Positions in files
  Position(..),
  prettyPosition,
  maxPos,
  minPos,
  initPos,
  incPos,
  retPos,
  incOffset,

  -- * Spans in files
  Span(..),
  unspan,
  prettySpan,
  subsetOf,
  (#),
  Spanned(..),
  Located(..),
) where

import GHC.Generics       ( Generic )

import Control.DeepSeq    ( NFData )
import Data.Data          ( Data )
import Data.Typeable      ( Typeable )

import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Monoid        ( Monoid(..) )
import Data.Semigroup     ( Semigroup(..) )


-- | A position in a source file. The row and column information is kept only for its convenience
-- and human-readability. Analogous to the information encoded in a cursor.
data Position = Position {
    absoluteOffset :: {-# UNPACK #-} !Int, -- ^ absolute offset the source file.
    row :: {-# UNPACK #-} !Int,            -- ^ row (line) in the source file.
    col :: {-# UNPACK #-} !Int             -- ^ column in the source file.
  }
  | NoPosition
  deriving (Eq, Ord, Show, Data, Typeable, Generic, NFData)

-- | Pretty print a 'Position'
prettyPosition :: Position -> String
prettyPosition NoPosition = "???"
prettyPosition (Position _ r c) = show r ++ ":" ++ show c

-- | Maximum of two positions, bias for actual positions.
--
-- >>> maxPos (Position 30 5 8) (Position 37 5 15)
-- Position 37 5 15
--
-- >>> maxPos NoPosition (Position 30 5 8)
-- Position 30 5 8
--
{-# INLINE maxPos #-}
maxPos :: Position -> Position -> Position
maxPos NoPosition p2 = p2
maxPos p1 NoPosition = p1
maxPos p1@(Position a1 _ _) p2@(Position a2 _ _) = if a1 > a2 then p1 else p2

-- | Maximum and minimum positions, bias for actual positions.
--
-- >>> minPos (Position 30 5 8) (Position 37 5 15)
-- Position 30 5 8
--
-- >>> minPos NoPosition (Position 30 5 8)
-- Position 30 5 8
--
{-# INLINE minPos #-}
minPos :: Position -> Position -> Position
minPos NoPosition p2 = p2
minPos p1 NoPosition = p1
minPos p1@(Position a1 _ _) p2@(Position a2 _ _) = if a1 < a2 then p1 else p2

-- | Starting position in a file.
{-# INLINE initPos #-}
initPos :: Position
initPos = Position 0 1 0

-- | Advance columnn a certain number of times.
{-# INLINE incPos #-}
incPos :: Position -> Int -> Position
incPos NoPosition _ = NoPosition
incPos p@Position{ absoluteOffset = a, col = c } offset = p { absoluteOffset = a + offset, col = c + offset }

-- | Advance to the next line.
{-# INLINE retPos #-}
retPos :: Position -> Position
retPos NoPosition = NoPosition
retPos (Position a r _) = Position { absoluteOffset = a + 1, row = r + 1, col = 1 }

-- | Advance only the absolute offset, not the row and column information. Only use this if you
-- know what you are doing!
{-# INLINE incOffset #-}
incOffset :: Position -> Int -> Position
incOffset NoPosition _ = NoPosition
incOffset p@Position{ absoluteOffset = a } offset = p { absoluteOffset = a + offset }

-- | Spans represent a contiguous region of code, delimited by two 'Position's. The endpoints are
-- inclusive. Analogous to the information encoded in a selection.
data Span = Span { lo, hi :: !Position }
  deriving (Eq, Ord, Show, Data, Typeable, Generic, NFData)

-- | Check if a span is a subset of another span
subsetOf :: Span -> Span -> Bool
Span l1 h1 `subsetOf` Span l2 h2 = minPos l1 l2 == l1 && maxPos h1 h2 == h2

-- | Convenience function lifting '<>' to work on all 'Located' things
{-# INLINE (#) #-}
(#) :: (Located a, Located b) => a -> b -> Span
left # right = spanOf left <> spanOf right

-- | smallest covering 'Span'
instance Semigroup Span where
  {-# INLINE (<>) #-}
  Span l1 h1 <> Span l2 h2 = Span (l1 `minPos` l2) (h1 `maxPos` h2)

instance Monoid Span where
  {-# INLINE mempty #-}
  mempty = Span NoPosition NoPosition

  {-# INLINE mappend #-}
  mappend = (<>) 

-- | Pretty print a 'Span'
prettySpan :: Span -> String
prettySpan (Span lo' hi') = show lo' ++ " - " ++ show hi'

-- | A "tagging" of something with a 'Span' that describes its extent.
data Spanned a = Spanned a {-# UNPACK #-} !Span
  deriving (Eq, Ord, Data, Typeable, Generic, NFData)

-- | Extract the wrapped value from 'Spanned'
{-# INLINE unspan #-}
unspan :: Spanned a -> a
unspan (Spanned x _) = x

instance Functor Spanned where
  {-# INLINE fmap #-}
  fmap f (Spanned x s) = Spanned (f x) s

instance Applicative Spanned where
  {-# INLINE pure #-}
  pure x = Spanned x mempty
  
  {-# INLINE (<*>) #-}
  Spanned f s1 <*> Spanned x s2 = Spanned (f x) (s1 <> s2)

instance Monad Spanned where
  return = pure
  Spanned x s1 >>= f = let Spanned y s2 = f x in Spanned y (s1 <> s2) 

instance Show a => Show (Spanned a) where
  show = show . unspan


-- | Describes nodes that can be located - their span can be extracted from them. In general, we
-- expect that for a value constructed as @Con x y z@ where @Con@ is an arbitrary constructor
--
-- prop> (spanOf x <> spanOf y <> spanOf z) `subsetOf` spanOf (Con x y z) == True
--
class Located a where
  spanOf :: a -> Span

instance Located Span where
  {-# INLINE spanOf #-}
  spanOf = id

instance Located (Spanned a) where
  {-# INLINE spanOf #-}
  spanOf (Spanned _ s) = s

instance Located a => Located (Maybe a) where
  {-# INLINE spanOf #-}
  spanOf = foldMap spanOf

-- | /O(n)/ time complexity
instance Located a => Located [a] where
  {-# INLINE spanOf #-}
  spanOf = foldMap spanOf

-- | /O(n)/ time complexity
instance Located a => Located (NonEmpty a) where
  {-# INLINE spanOf #-}
  spanOf = foldMap spanOf


