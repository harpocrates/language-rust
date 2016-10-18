{-# LANGUAGE DuplicateRecordFields #-}

module Language.Rust.Syntax.Ident where

-- | An identifier contains a Name (index into the interner table) and a SyntaxContext to track renaming
-- and macro expansion per Flatt et al., "Macros That Work Together"
-- https://docs.serde.rs/syntex_syntax/ast/struct.Ident.html
data Ident a
  = Ident {
      name :: Name,
      ctxt :: SyntaxContext,
      nodeInfo :: a
    } deriving (Show)

mkIdent :: String -> Ident ()
mkIdent s = Ident (Name s) 0 ()

-- TODO: backpack
type InternedString = String

-- | A name is a part of an identifier, representing a string or gensym. It's the result of interning.
-- https://docs.serde.rs/syntex_syntax/ast/struct.Name.html
data Name = Name InternedString deriving (Show) -- TODO, not quite

-- | A SyntaxContext represents a chain of macro expansions (represented by marks).
-- https://docs.serde.rs/syntex_syntax/ext/hygiene/struct.SyntaxContext.html
type SyntaxContext = Int
