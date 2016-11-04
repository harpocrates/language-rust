{-# LANGUAGE CPP #-}
module Language.Rust.Data.InputStream (
    InputStream, readInputStream,inputStreamToString,inputStreamFromString,
    takeByte, takeChar, inputStreamEmpty, takeChars,
    countLines,
) where

import Data.Word (Word8)

#ifndef NO_BYTESTRING
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BE
#else
import qualified Data.Char as Char
#endif

-- Things to review:
--   * backpack this when GHC 8.2 is released

-- | read a file into an 'InputStream'
readInputStream :: FilePath -> IO InputStream

-- | convert 'InputStream' to 'String'
inputStreamToString :: InputStream -> String
{-# INLINE inputStreamToString #-}

-- | convert a 'String' to an 'InputStream'
inputStreamFromString :: String -> InputStream

-- | @(b,is') = takeByte is@ reads and removes
-- the first byte @b@ from the 'InputStream' @is@
takeByte :: InputStream -> (Word8, InputStream)
{-# INLINE takeByte #-}

-- | @(c,is') = takeChar is@ reads and removes
-- the first character @c@ from the 'InputStream' @is@
takeChar :: InputStream -> (Char, InputStream)
{-# INLINE takeChar #-}

-- | return @True@ if the given input stream is empty
inputStreamEmpty :: InputStream -> Bool
{-# INLINE inputStreamEmpty #-}

-- | @str = takeChars n is@ returns the first @n@ characters
-- of the given input stream, without removing them
takeChars :: Int -> InputStream -> [Char]
{-# INLINE takeChars #-}

-- | @countLines@ returns the number of text lines  in the
-- given 'InputStream'
countLines :: InputStream -> Int


#ifndef NO_BYTESTRING

type InputStream = BS.ByteString
takeByte bs = (BS.head bs, BS.tail bs)
takeChar bs = let Just res = BE.uncons bs in res
inputStreamEmpty = BS.null
takeChars n = BE.toString . BE.take n
readInputStream = BS.readFile
inputStreamToString = BE.toString  
inputStreamFromString = BE.fromString 
countLines = length . BE.lines

#else

type InputStream = String
takeByte bs
  | Char.isLatin1 c = let b = fromIntegral (Char.ord c) in b `seq` (b, tail bs)
  | otherwise       = error "takeByte: not a latin-1 character"
  where c = head bs
takeChar bs = (head bs, tail bs)
inputStreamEmpty = null
takeChars n str = take n str
readInputStream = readFile
inputStreamToString = id
inputStreamFromString = id
countLines = length . lines

#endif
