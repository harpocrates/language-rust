{-# LANGUAGE CPP #-}
module Language.Rust.Data.InputStream (
    InputStream, readInputStream,inputStreamToString,inputStreamFromString,
    takeByte, takeChar, inputStreamEmpty, takeChars,
    countLines,
) where

import Data.Word

#ifndef NO_BYTESTRING
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
#else
import qualified Data.Char as Char
#endif

-- TODO: this is copied from Language.C.Data.InputStream. Backpack this (or use CPP) for bytestring

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
takeChar bs = let txt = decodeUtf8With lenientDecode bs in (T.head txt, encodeUtf8 (T.tail txt))
inputStreamEmpty = BS.null
takeChars n = T.unpack . T.take (fromIntegral n) . decodeUtf8With lenientDecode
readInputStream = BS.readFile
inputStreamToString = T.unpack . decodeUtf8With lenientDecode
inputStreamFromString = encodeUtf8 . T.pack
countLines = length . T.lines . decodeUtf8With lenientDecode

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
