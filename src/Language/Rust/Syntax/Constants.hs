module Language.Rust.Syntax.Constants where

import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Syntax.AST

import Data.Char (chr, isHexDigit, digitToInt)
import Data.Word (Word8)
import qualified Data.ByteString.Char8 as BSW (pack)
import qualified Data.ByteString as BS (pack)
import Data.List (unfoldr)

-- TODO make this have proper error handling
parseLit :: LitTok -> Suffix -> a -> Lit a
parseLit (ByteTok s)         = let Just (w8,"") = unescapeByte s in Byte w8
parseLit (CharTok s)         = let Just (c,"")  = unescapeChar s in Char c
parseLit (IntegerTok s)      = Int (unescapeInteger s)  
parseLit (FloatTok s)        = Float (unescapeFloat s) 
parseLit (StrTok s)          = Str (unfoldr unescapeChar s) Cooked
parseLit (StrRawTok s n)     = Str s (Raw n)
parseLit (ByteStrTok s)      = ByteStr (BS.pack (unfoldr unescapeByte s)) Cooked
parseLit (ByteStrRawTok s n) = ByteStr (BSW.pack s) (Raw n) 
  
-- | Given a string of characters read from a Rust source, extract the next underlying char taking
-- into account escapes and unicode.
unescapeChar :: String -> Maybe (Char, String)
unescapeChar ('\\':c:cs) = case c of
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
       _    -> error "unescape char: bad escape sequence"
unescapeChar (c:cs) = Just (c, cs)
unescapeChar [] = fail "unescape char: empty string"

-- | Given a string of characters read from a Rust source, extract the next underlying byte taking
-- into account escapes.
unescapeByte :: String -> Maybe (Word8, String)
unescapeByte ('\\':c:cs) = case c of
       'n'  -> pure (toEnum $ fromEnum '\n', cs)
       'r'  -> pure (toEnum $ fromEnum '\r', cs)
       't'  -> pure (toEnum $ fromEnum '\t', cs)
       '\\' -> pure (toEnum $ fromEnum '\\', cs)
       '\'' -> pure (toEnum $ fromEnum '\'', cs)
       '"'  -> pure (toEnum $ fromEnum '"',  cs)
       '0'  -> pure (toEnum $ fromEnum '\0', cs)
       'x'  -> do (h,cs') <- readHex 2 cs; pure (h, cs')
       'X'  -> do (h,cs') <- readHex 2 cs; pure (h, cs')
       _    -> error "unescape byte: bad escape sequence"
unescapeByte (c:cs) = Just (toEnum $ fromEnum c, cs)
unescapeByte [] = fail "unescape byte: empty string"

-- | Given a string Rust representation of an integer, parse it into a number
unescapeInteger :: Num a => String -> a
unescapeInteger ('0':'b':cs@(_:_)) | all (`elem` "_01") cs = numBase 2 (filter (/= '_') cs)
unescapeInteger ('0':'o':cs@(_:_)) | all (`elem` "_01234567") cs = numBase 8 (filter (/= '_') cs)
unescapeInteger ('0':'x':cs@(_:_)) | all (`elem` "_0123456789abcdefABCDEF") cs = numBase 16 (filter (/= '_') cs)
unescapeInteger cs@(_:_)           | all (`elem` "_0123456789") cs = numBase 10 (filter (/= '_') cs)
unescapeInteger _ = error "unescape integer: bad decimal literal"

-- | Given a string Rust representation of a float, parse it into a float.
-- NOTE: this is a bit hacky. Eventually, we might not do this and change the internal
-- representation of a float to a string (what language-c has opted to do).
unescapeFloat :: String -> Double
unescapeFloat cs | last cs == '.' = read (cs ++ "0")
                 | otherwise = read cs

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

