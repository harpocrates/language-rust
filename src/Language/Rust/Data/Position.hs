{-# LANGUAGE DeriveFunctor #-}

module Language.Rust.Data.Position where

import Data.Ord (comparing)
import Data.List (maximumBy, minimumBy)
import Data.Monoid (Monoid, mappend, mempty)

-- Taken and abbreviated from
-- | A position in a source file. The row and column information is kept only
-- for its convenience and human-readability.
-- https://hackage.haskell.org/package/language-c-0.5.0/docs/src/Language-C-Data-Position.html#Position
data Position = Position {
    absoluteOffset :: {-# UNPACK #-} !Int, -- ^ absolute offset the source file.
    row :: {-# UNPACK #-} !Int,            -- ^ row (line) in the source file.
    col :: {-# UNPACK #-} !Int             -- ^ column in the source file.
  }
  | NoPosition
  deriving (Eq, Ord)

-- | Maximum and minimum positions, bias for actual positions in either case
maxPos, minPos :: Position -> Position -> Position

maxPos NoPosition p2 = p2
maxPos p1 NoPosition = p1
maxPos p1 p2 = maximumBy (comparing absoluteOffset) [p1,p2]

minPos NoPosition p2 = p2
minPos p1 NoPosition = p1
minPos p1 p2 = minimumBy (comparing absoluteOffset) [p1,p2]

-- | starting position in a file
initPos :: Position
initPos = Position 0 1 1

-- | advance column
incPos :: Position -> Int -> Position
incPos NoPosition _ = NoPosition
incPos p@Position{ absoluteOffset = a, row = r } offset = p { absoluteOffset = a + offset, row = r + offset }

-- | advance to the next line
retPos :: Position -> Position
retPos NoPosition = NoPosition
retPos (Position a r _) = Position { absoluteOffset = a + 1, row = r + 1, col = 1 }

-- | advance just the offset
incOffset :: Position -> Int -> Position
incOffset NoPosition _ = NoPosition
incOffset p@Position{ absoluteOffset = a } offset = p { absoluteOffset = a + offset }


instance Show Position where
  show NoPosition = "$"
  show (Position _ r c) = show r ++ ":" ++ show c


type ExpnId = Int -- https://docs.serde.rs/syntex_pos/struct.ExpnId.html


-- | Spans represent a region of code, used for error reporting. Positions in
-- spans are absolute positions from the beginning of the codemap, not
-- positions relative to FileMaps. Methods on the CodeMap can be used to relate
-- spans back to the original source. You must be careful if the span crosses
-- more than one file - you will not be able to use many of the functions on 
-- spans in codemap and you cannot assume that the length of the span = hi - lo;
-- there may be space in the BytePos range between files.
-- https://docs.serde.rs/syntex_syntax/ext/quote/rt/struct.Span.html
data Span
  = Span {
    lo :: Position,
    hi :: Position --,
    -- expnId :: ExpnId
  } deriving (Eq)

-- Spans are merged by taking the smallest span that covers both arguments
instance Monoid Span where
  mempty = Span NoPosition NoPosition
  s1 `mappend` s2 = Span (lo s1 `minPos` lo s2) (hi s1 `maxPos` hi s2)

instance Show Span where
  show (Span lo' hi') = show lo' ++ " - " ++ show hi'

-- | A "tagging" of something with a 'Span' that describes its extent
data Spanned a = Spanned { unspan :: a, span :: Span } deriving (Functor)

instance Applicative Spanned where
  pure x = Spanned x mempty
  Spanned f s1 <*> Spanned x s2 = Spanned (f x) (s1 `mappend` s2)

instance Monad Spanned where
  return = pure
  Spanned x s1 >>= f = let Spanned y s2 = f x in Spanned y (s1 `mappend` s2) 

instance Show a => Show (Spanned a) where
  show (Spanned p s) = "at " ++ show p ++ ": " ++ show s

