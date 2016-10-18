module Language.Rust.Data.Position where

-- Taken and abbreviated from
-- https://hackage.haskell.org/package/language-c-0.5.0/docs/src/Language-C-Data-Position.html#Position
data Position
  = Position {
      posOffset :: {-# UNPACK #-} !Int, -- ^ absolute offset in the preprocessed file
      posRow :: {-# UNPACK #-} !Int,    -- ^ row (line) in the original file
      posColumn :: {-# UNPACK #-} !Int  -- ^ column in the original file
    }
  | NoPosition
  deriving (Eq, Ord)

type ExpnId = Int -- https://docs.serde.rs/syntex_pos/struct.ExpnId.html


-- | Spans represent a region of code, used for error reporting. Positions in spans are absolute positions from the
-- beginning of the codemap, not positions relative to FileMaps. Methods on the CodeMap can be used to relate spans
-- back to the original source. You must be careful if the span crosses more than one file - you will not be able to
-- use many of the functions on spans in codemap and you cannot assume that the length of the span = hi - lo; there may
-- be space in the BytePos range between files.
-- https://docs.serde.rs/syntex_syntax/ext/quote/rt/struct.Span.html
data Span
  = Span {
    lo :: Position,
    hi :: Position,
    expnId :: ExpnId
  }

data Spanned a = Spanned { node :: a, span :: Span }

