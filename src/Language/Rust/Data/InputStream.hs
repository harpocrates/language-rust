{-|
Module      : Language.Rust.Data.InputStream
Description : Interface to the underlying input of parsing
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

These are the only functions that need to be implemented in order to use the parser. Whether this wraps 'ByteString' or 'String' depends on whether the @useByteStrings@ option is on or not (it is by default). Using 'ByteString' means better handling of weird characters ('takeByte' fails badly if you try to take a byte that doesn't fall on a character boundary), but it means incurring a dependency on the [utf8-string](https://hackage.haskell.org/package/utf8-string) package.
-}
{-# LANGUAGE CPP #-}

module Language.Rust.Data.InputStream (
  -- * InputStream type
  InputStream, countLines, inputStreamEmpty,
  -- * Introduction forms
  readInputStream, hReadInputStream, inputStreamFromString,
  -- * Elimination forms
  inputStreamToString, takeByte, takeChar, takeChars,
) where

import Data.Word (Word8)
import Data.Coerce (coerce)
import Data.String (IsString(..))
import System.IO

#ifndef NO_BYTESTRING
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BE
#else
import qualified Data.Char as Char
#endif

-- TODO: backpack this when GHC 8.2 is released

-- | Read a file into an 'InputStream'
readInputStream :: FilePath -> IO InputStream

-- | Read an 'InputStream' from a 'Handle'
hReadInputStream :: Handle -> IO InputStream

-- | Convert 'InputStream' to 'String'
inputStreamToString :: InputStream -> String
{-# INLINE inputStreamToString #-}

-- | Convert a 'String' to an 'InputStream'
inputStreamFromString :: String -> InputStream

instance IsString InputStream where fromString = inputStreamFromString

-- | Read the first byte from an 'InputStream' and return that byte with what remains of the
-- 'InputStream'. Should only be called when 'inputStreamEmpty' returns 'False'.
takeByte :: InputStream -> (Word8, InputStream)
{-# INLINE takeByte #-}

-- | Read the first character from an 'InputStream' and return that 'Char' with what remains of the
-- 'InputStream'. Should only be called when 'inputStreamEmpty' returns 'False'.
takeChar :: InputStream -> (Char, InputStream)
{-# INLINE takeChar #-}

-- | Return @True@ if the given input stream is empty.
inputStreamEmpty :: InputStream -> Bool
{-# INLINE inputStreamEmpty #-}

-- | Returns the first @n@characters of the given input stream, without removing them.
takeChars :: Int -> InputStream -> String
{-# INLINE takeChars #-}

-- | Returns the number of text lines in the given 'InputStream'
countLines :: InputStream -> Int

#ifndef NO_BYTESTRING

-- | Opaque input type.
newtype InputStream = IS BS.ByteString
takeByte bs = (BS.head (coerce bs), coerce (BS.tail (coerce bs)))
takeChar bs = let Just res = BE.uncons (coerce bs) in coerce res
inputStreamEmpty = BS.null . coerce
takeChars n = BE.toString . BE.take n . coerce
readInputStream f = coerce <$> BS.readFile f
hReadInputStream h = coerce <$> BS.hGetContents h
inputStreamToString = BE.toString . coerce
inputStreamFromString = IS . BE.fromString 
countLines = length . BE.lines . coerce

#else

-- | Opaque input type.
newtype InputStream = IS String
takeByte (IS ~(c:str))
  | Char.isLatin1 c = let b = fromIntegral (Char.ord c) in b `seq` (b, IS str)
  | otherwise       = error "takeByte: not a latin-1 character"
takeChar (IS ~(c:str)) = (c, IS str)
inputStreamEmpty (IS str) = null str
takeChars n (IS str) = take n str
readInputStream f = IS <$> readFile f
hReadInputStream h = IS <$> hGetContents h
inputStreamToString = coerce
inputStreamFromString = IS
countLines (IS str) = length . lines $ str

#endif
