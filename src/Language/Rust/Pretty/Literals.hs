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

import Data.Text.Prettyprint.Doc (hcat, annotate, (<>), Doc, pretty, group, hardline, flatAlt)

import Data.Char (intToDigit, ord, chr)
import Data.Word (Word8)

-- | Print a literal (@print_literal@)
printLit :: Lit a -> Doc a
printLit lit = noIndent $ case lit of
    (Str     str Cooked  s x) -> annotate x (hcat [ "\"", group (foldMap (escapeChar True) str), "\"", suf s ])
    (Str     str (Raw n) s x) -> annotate x (hcat [ "r", pad n, "\"", string hardline str, "\"", pad n, suf s ])
    (ByteStr str Cooked  s x) -> annotate x (hcat [ "b\"", group (foldMap (escapeByte True) str), "\"", suf s ])
    (ByteStr str (Raw n) s x) -> annotate x (hcat [ "br", pad n, "\"", string hardline (map byte2Char str), "\"", pad n, suf s ])
    (Char c s x)              -> annotate x (hcat [ "'",  escapeChar False c, "'", suf s ])
    (Byte b s x)              -> annotate x (hcat [ "b'", escapeByte False b, "'", suf s ])
    (Int b i s x)             -> annotate x (hcat [ printIntLit i b, suf s ])
    (Float d s x)             -> annotate x (hcat [ pretty d,  suf s ])
    (Bool True s x)           -> annotate x (hcat [ "true",  suf s ])
    (Bool False s x)          -> annotate x (hcat [ "false", suf s ])
  where
  pad :: Int -> Doc a
  pad n = pretty (replicate n '#')

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

-- | Escape a byte. Based on @std::ascii::escape_default@.
--
-- If the first argument is true, newlines may become a literal newline characters if the string is
-- too long.
escapeByte :: Bool -> Word8 -> Doc a
escapeByte nl w8 = case byte2Char w8 of
  '\t' -> "\\t" 
  '\r' -> "\\r"
  '\\' -> "\\\\" 
  '\'' -> "\\'"
  '"'  -> "\\\""
  '\n'| nl        -> flatAlt hardline "\\n"
      | otherwise -> "\\n"
  c | 0x20 <= w8 && w8 <= 0x7e -> pretty c
  _ -> "\\x" <> padHex 2 w8

-- | Escape a unicode character. Based on @std::ascii::escape_default@.
--
-- If the first argument is true, newlines may become a literal newline characters if the string is
-- too long.
escapeChar :: Bool -> Char -> Doc a
escapeChar nl c | c <= '\x7f'   = escapeByte nl (char2Byte c)
                | c <= '\xffff' = "\\u{" <> padHex 4 (ord c) <> "}"
                | otherwise     = "\\u{" <> padHex 6 (ord c) <> "}"
 
-- | Convert a number to its padded hexadecimal form
padHex :: Integral a => Int -> a -> Doc b
padHex n 0 = pretty (replicate n '0')
padHex n m = let (m',r) = m `divMod` 0x10
             in padHex (n-1) m' <> pretty (intToDigit (fromIntegral r))

