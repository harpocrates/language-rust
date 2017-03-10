{-# LANGUAGE DuplicateRecordFields, OverloadedStrings #-}

module Language.Rust.Syntax.Ident (Ident(..), name, hash, mkIdent, invalidIdent, Name(..), InternedString) where

import Data.List (foldl')
import Data.Char (ord)
import Data.String

-- | An identifier contains a Name (index into the interner table) and a SyntaxContext to track renaming
-- and macro expansion per Flatt et al., "Macros That Work Together"
-- https://docs.serde.rs/syntex_syntax/ast/struct.Ident.html
data Ident
  = Ident {
      name :: Name,
      hash :: !Int
      -- ctxt :: SyntaxContext,
      -- nodeInfo :: a
    }

instance Show Ident where
  show = show . name

instance IsString Ident where
  fromString = mkIdent

instance Eq Ident where
  i1 == i2 = hash i1 == hash i2 && name i1 == name i2

mkIdent :: String -> Ident
mkIdent s = Ident (Name s) (hashString s) -- 0 ()

hashString :: String -> Int
hashString = foldl' f golden
   where f m c = fromIntegral (ord c) * magic + m
         magic = 0xdeadbeef
         golden = 1013904242

invalidIdent :: Ident
invalidIdent = mkIdent ""

-- TODO: backpack
type InternedString = String

-- | A name is a part of an identifier, representing a string or gensym. It's the result of interning.
-- https://docs.serde.rs/syntex_syntax/ast/struct.Name.html
data Name = Name InternedString deriving (Eq) -- TODO, not quite

instance Show Name where
  show (Name s) = s

-- | A SyntaxContext represents a chain of macro expansions (represented by marks).
-- https://docs.serde.rs/syntex_syntax/ext/hygiene/struct.SyntaxContext.html
--type SyntaxContext = Int
