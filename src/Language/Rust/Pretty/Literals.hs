{-|
Module      : Language.Rust.Pretty.Literals
Description : Parsing literals
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Functions for pretty-printing literals.
-}
{-# OPTIONS_HADDOCK hide, not-home #-}
{-# LANGUAGE OverloadedStrings  #-}

module Language.Rust.Pretty.Literals (
  printLit
) where

import Language.Rust.Syntax.AST
import Language.Rust.Pretty.Util

import Data.Text.Prettyprint.Doc (hcat, annotate, (<>), Doc, pretty, hardline)

import Data.Char (intToDigit, ord, chr)
import Data.Word (Word8)

-- | Print a literal (@print_literal@)
printLit :: Lit a -> Doc a
printLit lit = noIndent $ case lit of
    (Str     str Cooked  s x) -> annotate x (hcat [ "\"", foldMap escapeChar str, "\"", suf s ])
    (Str     str (Raw m) s x) -> annotate x (hcat [ "r", pad m, "\"", string hardline str, "\"", pad m, suf s ])
    (ByteStr str Cooked  s x) -> annotate x (hcat [ "b\"", foldMap escapeByte str, "\"", suf s ])
    (ByteStr str (Raw m) s x) -> annotate x (hcat [ "br", pad m, "\"", string hardline (map byte2Char str), "\"", pad m, suf s ])
    (Char c s x)              -> annotate x (hcat [ "'",  escapeChar c, "'", suf s ])
    (Byte b s x)              -> annotate x (hcat [ "b'", escapeByte b, "'", suf s ])
    (Int b i s x)             -> annotate x (hcat [ printIntLit i b, suf s ])
    (Float d s x)             -> annotate x (hcat [ pretty d,  suf s ])
    (Bool True s x)           -> annotate x (hcat [ "true",  suf s ])
    (Bool False s x)          -> annotate x (hcat [ "false", suf s ])
  where
  pad :: Int -> Doc a
  pad m = pretty (replicate m '#')

  suf :: Suffix -> Doc a
  suf = pretty . show

-- | Print an integer literal
printIntLit :: Integer -> IntRep -> Doc a
printIntLit i r | i < 0     = "-" <> baseRep r <> toNBase (abs i) (baseVal r)
                | i == 0    =        baseRep r <> "0"
                | otherwise =        baseRep r <> toNBase (abs i) (baseVal r)
  where
  baseRep :: IntRep -> Doc a
  baseRep Bin = "0b"
  baseRep Oct = "0o"
  baseRep Dec = mempty
  baseRep Hex = "0x"

  baseVal :: IntRep -> Integer
  baseVal Bin = 2
  baseVal Oct = 8
  baseVal Dec = 10
  baseVal Hex = 16

  toDigit :: Integer -> Char
  toDigit l = "0123456789ABCDEF" !! fromIntegral l

  toNBase :: Integer -> Integer -> Doc a
  l `toNBase` b | l < b = pretty (toDigit l)
                | otherwise = let ~(d,e) = l `quotRem` b in toNBase d b <> pretty (toDigit e)


-- | Extend a byte into a unicode character
byte2Char :: Word8 -> Char
byte2Char = chr . fromIntegral 
  
-- | Constrain a unicode character to a byte
-- This assumes the character is in the right range already
char2Byte :: Char -> Word8
char2Byte = fromIntegral . ord

-- | Escape a byte. Based on @std::ascii::escape_default@
escapeByte :: Word8 -> Doc a
escapeByte w8 = case byte2Char w8 of
  '\t' -> "\\t" 
  '\r' -> "\\r"
  '\n' -> "\\n"
  '\\' -> "\\\\" 
  '\'' -> "\\'"
  '"'  -> "\\\""
  c | 0x20 <= w8 && w8 <= 0x7e -> pretty c
  _ -> "\\x" <> padHex 2 w8

-- | Escape a unicode character. Based on @std::ascii::escape_default@
escapeChar :: Char -> Doc a
escapeChar c | c <= '\x7f'   = escapeByte (char2Byte c)
             | c <= '\xffff' = "\\u{" <> padHex 4 (ord c) <> "}"
             | otherwise     = "\\u{" <> padHex 6 (ord c) <> "}"
 
-- | Convert a number to its padded hexadecimal form
padHex :: Integral a => Int -> a -> Doc b
padHex i 0 = pretty (replicate i '0')
padHex i m = let (m',r) = m `divMod` 0x10
             in padHex (i-1) m' <> pretty (intToDigit (fromIntegral r))

