{-|
Module      : Language.Rust.Quote
Description : Quasiquotes for Rust AST
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

Quasiquoters for converting Rust code into the equivalent Haskell patterns and expressions. In the
future, we will try to do something similar to Rust macros to extract or inject ASTs out or into
the quasiquotes.

Eventually, one should be able to just import this module for code generation. The following
interaction is what should eventually work.

>>> import qualified Language.Rust.Quote as Q
>>> :set -XQuasiQuotes +t
>>> let one = [Q.expr| 1i32 |]
one :: Expr Span
>>> [Q.expr| |x: i32| -> $retTy:ty $body:block |] = [Q.expr| |x: i32| -> i32 { ($one) + x } |]
retTy :: Ty Span
body :: Block Span
>>> import Language.Rust.Pretty
>>> pretty retTy
i32
>>> pretty body
{ (1i32) + x }

For now, however, you cannot use '$x' or '$x:ty' meta variables.
-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Rust.Quote (
  lit, attr, ty, pat, stmt, expr, item, sourceFile, implItem, traitItem, tokenTree, block,
) where


import Language.Rust.Parser.ParseMonad
import Language.Rust.Parser.Internal
import Language.Rust.Data.InputStream (inputStreamFromString)
import Language.Rust.Data.Position (Position(..))

import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ, dataToPatQ)

import Data.Data (Data)

-- | Given a parser, convert it into a quasiquoter
quoter :: Data a => P a -> QuasiQuoter
quoter p = QuasiQuoter
             { quoteExp = go (dataToExpQ (const Nothing))
             , quotePat = go (dataToPatQ (const Nothing))
             , quoteDec = error "this quasiquoter does not support declarations"
             , quoteType = error "this quasiquoter does not support types"
             }
  where
  -- | Given a parser and an input string, turn it into the corresponding Haskell expression/pattern.
  go func inp = do
    Loc{ loc_start = (r,c) } <- location
  
    -- Run the parser
    case execParser p (inputStreamFromString inp) (Position 0 r c) of
      Left (_,msg) -> fail msg
      Right x -> func x



-- | Quasiquoter for literals (see 'Language.Rust.Syntax.Lit')
lit :: QuasiQuoter
lit = quoter parseLit

-- | Quasiquoter for attributes (see 'Language.Rust.Syntax.Attribute')
attr :: QuasiQuoter
attr = quoter parseAttr

-- | Quasiquoter for types (see 'Language.Rust.Syntax.Ty')
ty :: QuasiQuoter
ty = quoter parseTy

-- | Quasiquoter for pattersn (see 'Language.Rust.Syntax.Pat')
pat :: QuasiQuoter
pat = quoter parsePat

-- | Quasiquoter for statements (see 'Language.Rust.Syntax.Stmt')
stmt :: QuasiQuoter
stmt = quoter parseStmt

-- | Quasiquoter for expressions (see 'Language.Rust.Syntax.Expr')
expr :: QuasiQuoter
expr = quoter parseExpr

-- | Quasiquoter for items (see 'Language.Rust.Syntax.Item')
item :: QuasiQuoter
item = quoter parseItem

-- | Quasiquoter for crates (see 'Language.Rust.Syntax.SourceFile')
sourceFile :: QuasiQuoter
sourceFile = quoter parseSourceFile

-- | Quasiquoter for blocks (see 'Language.Rust.Syntax.Block')
block :: QuasiQuoter
block = quoter parseBlock

-- | Quasiquoter for impl items (see 'Language.Rust.Syntax.ImplItem')
implItem :: QuasiQuoter
implItem = quoter parseImplItem

-- | Quasiquoter for trait item (see 'Language.Rust.Syntax.TraitItem')
traitItem :: QuasiQuoter
traitItem = quoter parseTraitItem

-- | Quasiquoter for token trees (see 'Language.Rust.Syntax.TokenTree')
tokenTree :: QuasiQuoter
tokenTree = quoter parseTt

