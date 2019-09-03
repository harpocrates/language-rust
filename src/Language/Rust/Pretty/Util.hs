{-|
Module      : Language.Rust.Pretty.Util
Description : pretty printing utilities
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

This module contains a variety of utility functions for pretty printing. Most of these require
inspecting the internal structure of 'Doc'.

Wadler's take on pretty printing is super useful because it allows us to print thinks like blocks
much more nicely (see [this](http://stackoverflow.com/a/41424946/3072788)) that Hughes'.
Unfortunately, unlike Hughes', Wadler does not have 'mempty' as the identity of @<+>@ - the
space between the arguments of @<+>@ does not go away even if either argument is empty. The
same problem shows up for @hsep@, @<#>@, @vsep@, @</>@, etc.

My solution has been to redefine my versions of these functions which _do_ treat 'mempty' as a
neutral element for @<+>@, @hsep@, @<#>@, @vsep@, and @</>@.
-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Rust.Pretty.Util where

import Data.Monoid as M

import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Internal.Type ( Doc(..) )

import Language.Rust.Syntax.Token ( Delim(..) )

-- | Indentation level
n :: Int
n = 2

-- | Similar to 'maybe', but where the 'Nothing' case is an empty 'Doc'
emptyElim :: Doc a             -- ^ result if scrutinee is empty
          -> (Doc a -> Doc a)  -- ^ how to process scrutinee if it is not empty
          -> Doc a             -- ^ scrutinee 'Doc'
          -> Doc a
emptyElim a _ Empty = a
emptyElim _ f doc   = f doc

-- | Vertically concatenate two 'Doc's with a collapsible line between them
(<##>) :: Doc a -> Doc a -> Doc a
d1 <##> d2 = d1 M.<> PP.line' M.<> d2

-- | Flatten a 'Doc'
flatten :: Doc a -> Doc a
flatten d@Fail{}          = d
flatten d@Empty{}         = d 
flatten d@Char{}          = d 
flatten d@Text{}          = d 
flatten d@Line{}          = d 
flatten (FlatAlt _ d)     = d
flatten (Cat d1 d2)       = Cat (flatten d1) (flatten d2)
flatten (Nest i d)        = Nest i (flatten d)
flatten (Union d _)       = flatten d
flatten (Column f)        = Column (flatten . f)
flatten (WithPageWidth f) = WithPageWidth (flatten . f)
flatten (Nesting f)       = Nesting (flatten . f)
flatten (Annotated a d)   = Annotated a (flatten d)

-- | Map the list of items into 'Doc's using the provided function and add comma punctuation
commas :: [a] -> (a -> Doc b) -> Doc b
commas xs f = PP.hsep (PP.punctuate "," (map f xs))

-- | Take a binary operation on docs and lift it to one that has (left and right) identity 'mempty'
liftOp :: (Doc a -> Doc a -> Doc a) -> Doc a -> Doc a -> Doc a
liftOp _ Empty d = d
liftOp _ d Empty = d
liftOp (#) d d' = d # d'

-- | Lifted version of Wadler's @<+>@
(<+>) :: Doc a -> Doc a -> Doc a
(<+>) = liftOp (PP.<+>)

-- | Lifted version of Wadler's @hsep@
hsep :: Foldable f => f (Doc a) -> Doc a
hsep = foldr (<+>) mempty

-- | Lifted version of Wadler's @<#>@
(<#>) :: Doc a -> Doc a -> Doc a
(<#>) = liftOp (\x y -> x <> PP.line <> y)

-- | Lifted version of Wadler's @vsep@
vsep :: Foldable f => f (Doc a) -> Doc a
vsep = foldr (<#>) mempty

-- | Lifted version of Wadler's @</>@
(</>) :: Doc a -> Doc a -> Doc a
(</>) = liftOp (\x y -> x <> PP.softline <> y)

-- | Unless the condition holds, print the document
unless :: Bool -> Doc a -> Doc a
unless b = when (not b)

-- | When the condition holds, print the document
when :: Bool -> Doc a -> Doc a
when cond d = if cond then d else mempty

-- | Apply a printing function to an optional value. If the value is 'Nothing', 'perhaps' returns
-- the empty 'Doc'.
perhaps :: (a -> Doc b) -> Maybe a -> Doc b
perhaps = maybe mempty

-- | Indent the given 'Doc', but only if multi-line
indent :: Int -> Doc a -> Doc a
indent m doc = PP.flatAlt (PP.indent m doc) (flatten doc)

-- | Undo what group does. This function is pretty dangerous...
ungroup :: Doc a -> Doc a
ungroup (Union _ x) = x
ungroup y = y

-- | Remove all indent
noIndent :: Doc a -> Doc a
noIndent d = PP.nesting (\i -> PP.nest (negate i) d)

-- | Translate '\n' in a string using the provided 'Doc' instead of 'line'
string :: Doc a -> String -> Doc a
string new = foldMap (\c -> case c of { '\n' -> new; _ -> Char c })

-- | This is the most general function for printing blocks. It operates with any delimiter, any
-- seperator, an optional leading attribute doc (which isn't followed by a seperator), and wraps a
-- list of entries. It has been tweaked to look Just Right (TM) for the usual cases.
--
-- Note that this will try to fit things on one line when possible, so if you want a block that is
-- sure /not/ to be condensed on one line (e.g. for a function), you have to construct it manually.
block :: Delim           -- ^ outer delimiters
       -> Bool           -- ^ prefer to be on one line (as opposed to multiline)? 
       -> Doc a          -- ^ seperator
       -> Doc a          -- ^ attributes doc, after which no seperator will (use 'mempty' to ignore)
       -> [Doc a]        -- ^ entries
       -> Doc a
block delim p s as xs = group' (lDel # PP.vsep (as' ++ ys) # rDel)
  where
  group' = if p || null (as' ++ ys) then PP.group else id

  -- left and right delimiter lists
  (lDel,rDel) = case delim of
                  Paren ->   ("(", ")")
                  Bracket -> ("[", "]")
                  Brace ->   ("{", "}")
                  NoDelim -> (mempty, mempty)

  -- method of contenating delimiters with the rest of the body
  (#) = case delim of { Paren -> (<##>); _ -> (<#>) }

  -- attributes
  as' = case as of
          Empty -> []
          _ -> [ PP.flatAlt (PP.indent n as) (flatten as) ]

  -- list of entries
  ys = go xs where go [] = []
                   go [z] = [ PP.flatAlt (PP.indent n z <> s) (flatten z) ]
                   go (z:zs) = PP.flatAlt (PP.indent n z <> s) (flatten z <> s) : go zs
  

