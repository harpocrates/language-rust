{-|
Module      : Language.Rust.Parser.Literals
Description : Parsing literals
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Functions for parsing literals from valid literal tokens. Note the functions in this module fail
badly is fed invalid 'LitTok's; it is expected their input is coming from Alex and is correct.
-}

module Language.Rust.Parser.Literals (
  translateLit
) where

import Language.Rust.Syntax.Token ( LitTok(..) )
import Language.Rust.Syntax.AST   ( IntRep(..), Lit(..), StrStyle(..), Suffix(..) )

import Data.Char                  ( chr, digitToInt, ord, isHexDigit, isSpace )
import Data.List                  ( unfoldr )
import Data.Maybe                 ( fromMaybe )
import Data.Word                  ( Word8 )

import Text.Read                  ( readMaybe )

-- | Parse a valid 'LitTok' into a 'Lit'.
translateLit :: LitTok -> Suffix -> a -> Lit a
translateLit (ByteTok s)         = Byte (unescapeByte' s)
translateLit (CharTok s)         = Char (unescapeChar' s)
translateLit (FloatTok s)        = Float (unescapeFloat s) 
translateLit (StrTok s)          = Str (unfoldr (unescapeChar True) s) Cooked
translateLit (StrRawTok s n)     = Str s (Raw n)
translateLit (ByteStrTok s)      = ByteStr (unfoldr (unescapeByte True) s) Cooked
translateLit (ByteStrRawTok s n) = ByteStr (map (fromIntegral . ord) s) (Raw n) 
translateLit (IntegerTok s)      = \suf -> case (suf, unescapeInteger s) of
                                             (F32, (Dec, n)) -> Float (fromInteger n) F32
                                             (F64, (Dec, n)) -> Float (fromInteger n) F64 
                                             (_,   (rep, n)) -> Int rep n suf
  
-- | Given a string of characters read from a Rust source, extract the next underlying char taking
-- into account escapes and unicode.
unescapeChar :: Bool                    -- ^ multi-line strings allowed
             -> String                  -- ^ input string
             -> Maybe (Char, String)
unescapeChar multiline ('\\':c:cs) = case c of
  'n'  -> pure ('\n', cs)
  'r'  -> pure ('\r', cs)
  't'  -> pure ('\t', cs)
  '\\' -> pure ('\\', cs)
  '\'' -> pure ('\'', cs)
  '"'  -> pure ('"',  cs)
  '0'  -> pure ('\0', cs)
  'x'  -> do (h,cs') <- readHex 2 cs; pure (chr h, cs')
  'X'  -> do (h,cs') <- readHex 2 cs; pure (chr h, cs')
  'U'  -> do (h,cs') <- readHex 8 cs; pure (chr h, cs')
  'u'  -> case cs of
    '{':x1:'}':cs'                -> do (h,_)   <- readHex 1 [x1];                pure (chr h, cs')
    '{':x1:x2:'}':cs'             -> do (h,_)   <- readHex 2 [x1,x2];             pure (chr h, cs')
    '{':x1:x2:x3:'}':cs'          -> do (h,_)   <- readHex 3 [x1,x2,x3];          pure (chr h, cs')
    '{':x1:x2:x3:x4:'}':cs'       -> do (h,_)   <- readHex 4 [x1,x2,x3,x4];       pure (chr h, cs')
    '{':x1:x2:x3:x4:x5:'}':cs'    -> do (h,_)   <- readHex 5 [x1,x2,x3,x4,x5];    pure (chr h, cs')
    '{':x1:x2:x3:x4:x5:x6:'}':cs' -> do (h,_)   <- readHex 6 [x1,x2,x3,x4,x5,x6]; pure (chr h, cs')
    _                             -> do (h,cs') <- readHex 4 cs;                  pure (chr h, cs')
  '\n' | multiline -> unescapeChar multiline $ dropWhile isSpace cs
  _ -> error "unescape char: bad escape sequence"
unescapeChar _ (c:cs) = Just (c, cs)
unescapeChar _ [] = fail "unescape char: empty string"

-- | Given a string of characters read from a Rust source, extract the next underlying byte taking
-- into account escapes.
unescapeByte :: Bool                    -- ^ multi-line strings allowed
             -> String                  -- ^ input string
             -> Maybe (Word8, String)
unescapeByte multiline ('\\':c:cs) = case c of
       'n'  -> pure (toEnum $ fromEnum '\n', cs)
       'r'  -> pure (toEnum $ fromEnum '\r', cs)
       't'  -> pure (toEnum $ fromEnum '\t', cs)
       '\\' -> pure (toEnum $ fromEnum '\\', cs)
       '\'' -> pure (toEnum $ fromEnum '\'', cs)
       '"'  -> pure (toEnum $ fromEnum '"',  cs)
       '0'  -> pure (toEnum $ fromEnum '\0', cs)
       'x'  -> do (h,cs') <- readHex 2 cs; pure (h, cs')
       'X'  -> do (h,cs') <- readHex 2 cs; pure (h, cs')
       '\n' | multiline -> unescapeByte multiline $ dropWhile isSpace cs
       _    -> error "unescape byte: bad escape sequence"
unescapeByte _ (c:cs) = Just (toEnum $ fromEnum c, cs)
unescapeByte _ [] = fail "unescape byte: empty string"

-- | Given a string Rust representation of a character, parse it into a character
unescapeChar' :: String -> Char
unescapeChar' s = case unescapeChar False s of
                    Just (c, "") -> c
                    _ -> error "unescape char: bad character literal"

-- | Given a string Rust representation of a byte, parse it into a byte
unescapeByte' :: String -> Word8
unescapeByte' s = case unescapeByte False s of
                    Just (w8, "") -> w8
                    _ -> error "unescape byte: bad byte literal"

-- | Given a string Rust representation of an integer, parse it into a number
unescapeInteger :: Num a => String -> (IntRep,a)
unescapeInteger ('0':'b':cs@(_:_)) | all (`elem` "_01") cs = (Bin, numBase 2 (filter (/= '_') cs))
unescapeInteger ('0':'o':cs@(_:_)) | all (`elem` "_01234567") cs = (Oct, numBase 8 (filter (/= '_') cs))
unescapeInteger ('0':'x':cs@(_:_)) | all (`elem` "_0123456789abcdefABCDEF") cs = (Hex, numBase 16 (filter (/= '_') cs))
unescapeInteger cs@(_:_)           | all (`elem` "_0123456789") cs = (Dec, numBase 10 (filter (/= '_') cs))
unescapeInteger _ = error "unescape integer: bad decimal literal"

-- | Given a string Rust representation of a float, parse it into a float.
-- NOTE: this is a bit hacky. Eventually, we might not do this and change the internal
-- representation of a float to a string (what language-c has opted to do).
unescapeFloat :: String -> Double
unescapeFloat cs = fromMaybe (error $ "unescape float: cannot parse float " ++ cs') (readMaybe cs')
  where cs' = filter (/= '_') (if last cs == '.' then cs ++ "0" else cs)

-- | Try to read a hexadecimal number of the specified length off of the front of the string
-- provided. If there are not enough characters to do this, or the characters don't fall in the
-- right range, this fails with 'Nothing'.
readHex :: Num a => Int -> String -> Maybe (a, String)
readHex n cs = let digits = take n cs
               in if length digits == n && all isHexDigit digits
                    then Just (numBase 16 digits, drop n cs)
                    else Nothing

-- | Convert a list of characters to the number they represent.
numBase :: Num a => a -> String -> a
numBase b = foldl (\n x -> fromIntegral (digitToInt x) + b * n) 0

