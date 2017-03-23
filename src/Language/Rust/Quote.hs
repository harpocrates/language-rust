{-|
Module      : Language.Rust.Quote
Description : Quasiquotes for Rust AST
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Quasiquoters for converting Rust code into the equivalent Haskell patterns and expressions. For
now, this doesn't do anything fancy. In the future, we will try to do something similar to Rust
macros to extract/inject ASTs out/into the quasiquotes.

Eventually, one should be able to just import this module for code generation. The following
interaction is what should eventually work:

>>> import Language.Rust.Quote
>>> :set -XQuasiQuotes
>>> let one = [lit| 1i32 |]
>>> [expr| |x: i32| -> $ret:ty $body:expr |] = [expr| |x: i32| -> i32 { x + $one } |]
ret :: Ty Span
body :: Expr Span
>>> import Language.Rust.Pretty
>>> pretty ret
i32
>>> pretty body
{ x + 1 }
-}

module Language.Rust.Quote (
  lit, attr, ty, pat, stmt, expr, item, crate, implItem, traitItem, tokenTree,
) where


{-
 - Strategy moving forward:
 -
 -   * Find an (unsafe) way to get a 'Q (String -> Maybe Type)' function (instead of 'String -> Q Type')
 -     and use that to make the swapping function. Alternately, lex twice and extract all of the
 -     interesting tokens, look them up, and pass them in as a function in the second lexing pass.
 -   * Swap in 'NonTerminal's containing 'error <variable-name>' - but of the right type!
 -   * Make view patterns for the parser (instead of pattern matching directly) and make those
 -     patterns check for errors using something atrocious like
 -
 -       import Control.Exception
 -       import System.IO.Unsafe
 -       import Unsafe.Coerce
 -
 -       isError :: a -> Bool
 -       isError x = unsafePerformIO $ catch ((unsafeCoerce x :: IO ()) >> return False) (const $ return True :: SomeException -> IO Bool)
 -
 -     Check this function is cheap - if it isn't, we may have a problem (although I can think of
 -     more typeclass based hacks with specialization)
 -   * In the quoters below in 'dataToPatQ' and 'dataToExpQ', whenever something is of the right
 -     type, check if it is defined. If so, do nothing, otherwise, use the error message (ewwwwww!)
 -     to lookup variable or make an identifier pattern.
 -}


import Language.Rust.Parser
import Language.Rust.Parser.Internal
import Language.Rust.Data.InputStream (inputStreamFromString)
import Language.Rust.Data.Position (Position(..), Span)

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Data (cast, Data)

-- | Given a parser and an input string, turn it into the corresponding Haskell expression
expQuoter :: Data a => P a -> String -> Q Exp
expQuoter p inp = do
  Loc{ loc_start = (r,c) } <- qLocation
  case execParser p (inputStreamFromString inp) (Position 0 r c) of
    Left (_,msg) -> fail msg
    Right x -> liftData x

-- | Given a parser and an input string, turn it into the corresponding Haskell pattern
patQuoter :: Data a => P a -> String -> Q Pat
patQuoter p inp = do
  Loc{ loc_start = (r,c) } <- qLocation
  case execParser p (inputStreamFromString inp) (Position 0 r c) of
    Left (_,msg) -> fail msg
    Right x -> dataToPatQ (\y -> pure WildP <$ (cast y :: Maybe Span)) x

-- | Given a parser, convert it into a quasiquoter
quoter :: Data a => P a -> QuasiQuoter
quoter p = QuasiQuoter
             { quoteExp = expQuoter p
             , quotePat = patQuoter p
             , quoteDec = error "this quasiquoter does not support declarations"
             , quoteType = error "this quasiquoter does not support types"
             }

-- | Quasiquoter for literals (see 'Lit')
lit :: QuasiQuoter
lit = quoter parseLit

-- | Quasiquoter for attributes (see 'Attribute')
attr :: QuasiQuoter
attr = quoter parseAttr

-- | Quasiquoter for types (see 'Ty')
ty :: QuasiQuoter
ty = quoter parseTy

-- | Quasiquoter for pattersn (see 'Pat')
pat :: QuasiQuoter
pat = quoter parsePat

-- | Quasiquoter for statements (see 'Stmt')
stmt :: QuasiQuoter
stmt = quoter parseStmt

-- | Quasiquoter for expressions (see 'Expr')
expr :: QuasiQuoter
expr = quoter parseExpr

-- | Quasiquoter for items (see 'Item')
item :: QuasiQuoter
item = quoter parseItem

-- | Quasiquoter for crates (see 'Crate')
crate :: QuasiQuoter
crate = quoter parseCrate

-- | Quasiquoter for blocks (see 'Block')
block :: QuasiQuoter
block = quoter parseBlock

-- | Quasiquoter for impl items (see 'ImplItem')
implItem :: QuasiQuoter
implItem = quoter parseImplItem

-- | Quasiquoter for trait item (see 'TraitItem')
traitItem :: QuasiQuoter
traitItem = quoter parseTraitItem

-- | Quasiquoter for token trees (see 'TokenTree')
tokenTree :: QuasiQuoter
tokenTree = quoter parseTt

