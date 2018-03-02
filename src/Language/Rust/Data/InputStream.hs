{-|
Module      : Language.Rust.Data.InputStream
Description : Interface to the underlying input of parsing
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

These are the only functions that need to be implemented in order to use the parser. Whether this
wraps 'BS.ByteString' or 'String' depends on whether the @useByteStrings@ option is on or not (it is
by default). Using 'BS.ByteString' means better handling of weird characters ('takeByte' for plain
'String' fails badly if you try to take a byte that doesn't fall on a character boundary), but it
means incurring a dependency on the [utf8-string](https://hackage.haskell.org/package/utf8-string)
package.
-}
{-# LANGUAGE CPP #-}

module Language.Rust.Data.InputStream (
  -- * InputStream type
  InputStream,
  countLines,
  inputStreamEmpty,
  
  -- * Introduction forms
  readInputStream,
  hReadInputStream,
  inputStreamFromString,
  
  -- * Elimination forms
  inputStreamToString,
  takeByte,
  takeChar,
  peekChars,
) where

import Data.Word   ( Word8 )
import Data.Coerce ( coerce )
import Data.String ( IsString(..) )
import System.IO

#ifdef USE_BYTESTRING
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BE
#else
import qualified Data.Char as Char
#endif

-- | Read an encoded file into an 'InputStream'
readInputStream :: FilePath -> IO InputStream
{-# INLINE readInputStream #-}

-- | Read an 'InputStream' from a 'Handle'
hReadInputStream :: Handle -> IO InputStream
{-# INLINE hReadInputStream #-}

-- | Convert 'InputStream' to 'String'.
inputStreamToString :: InputStream -> String
{-# INLINE inputStreamToString #-}

-- | Convert a 'String' to an 'InputStream'.
inputStreamFromString :: String -> InputStream
{-# INLINE inputStreamFromString #-}

-- | Uses 'inputStreamFromString'
instance IsString InputStream where fromString = inputStreamFromString

-- | Read the first byte from an 'InputStream' and return that byte with what remains of the
-- 'InputStream'. Behaviour is undefined when 'inputStreamEmpty' returns 'True'.
--
-- >>> takeByte "foo bar"
-- (102, "oo bar")
--
-- >>> takeByte "Ĥăƨĸëļļ"
-- (196, "\ETX\168\&8\235<<")
--
takeByte :: InputStream -> (Word8, InputStream)
{-# INLINE takeByte #-}

-- | Read the first character from an 'InputStream' and return that 'Char' with what remains of the
-- 'InputStream'. Behaviour is undefined when 'inputStreamEmpty' returns 'True'.
--
-- >>> takeChar "foo bar"
-- ('f', "oo bar")
--
-- >>> takeChar "Ĥăƨĸëļļ"
-- ('Ĥ', "ăƨĸëļļ")
--
takeChar :: InputStream -> (Char, InputStream)
{-# INLINE takeChar #-}

-- | Return @True@ if the given input stream is empty.
--
-- >>> inputStreamEmpty ""
-- True
--
-- >>> inputStreamEmpty "foo"
-- False
--
inputStreamEmpty :: InputStream -> Bool
{-# INLINE inputStreamEmpty #-}

-- | Returns the first @n@ characters of the given input stream, without removing them.
--
-- >>> peekChars 5 "foo bar"
-- "foo ba"
--
-- >>> peekChars 5 "foo"
-- "foo"
--
-- >>> peekChars 3 "Ĥăƨĸëļļ"
-- "Ĥăƨ"
--
peekChars :: Int -> InputStream -> String
{-# INLINE peekChars #-}

-- | Returns the number of text lines in the given 'InputStream'
--
-- >>> countLines ""
-- 0
--
-- >>> countLines "foo"
-- 1
--
-- >>> countLines "foo\n\nbar"
-- 3
--
-- >>> countLines "foo\n\nbar\n"
-- 3
--
countLines :: InputStream -> Int
{-# INLINE countLines #-}

#ifdef USE_BYTESTRING

-- | Opaque input type.
newtype InputStream = IS BS.ByteString deriving (Eq, Ord)
takeByte bs = (BS.head (coerce bs), coerce (BS.tail (coerce bs)))
takeChar bs = maybe (error "takeChar: no char left") coerce (BE.uncons (coerce bs))
inputStreamEmpty = BS.null . coerce
peekChars n = BE.toString . BE.take n . coerce
readInputStream f = coerce <$> BS.readFile f
hReadInputStream h = coerce <$> BS.hGetContents h
inputStreamToString = BE.toString . coerce
inputStreamFromString = IS . BE.fromString 
countLines = length . BE.lines . coerce

instance Show InputStream where
  show (IS bs) = show bs

#else

-- | Opaque input type.
newtype InputStream = IS String deriving (Eq, Ord)
takeByte (IS ~(c:str))
  | Char.isLatin1 c = let b = fromIntegral (Char.ord c) in b `seq` (b, IS str)
  | otherwise       = error "takeByte: not a latin-1 character"
takeChar (IS ~(c:str)) = (c, IS str)
inputStreamEmpty (IS str) = null str
peekChars n (IS str) = take n str
readInputStream f = IS <$> readFile f
hReadInputStream h = IS <$> hGetContents h
inputStreamToString = coerce
inputStreamFromString = IS
countLines (IS str) = length . lines $ str

instance Show InputStream where
  show (IS bs) = show bs

#endif
