{-|
Module      : Language.Rust.Quote
Description : Quasiquotes for Rust AST
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

Quasiquoters for converting Rust code into the equivalent Haskell patterns and expressions.
These are just convenience wrappers over 'dataToExpQ' and 'dataToPatQ'. These quasiquoters
only work as expressions and patterns, not as declarations or types. The pattern quasiquoters
ignore the 'Span' and 'Position' fields.
-}


module Language.Rust.Quote (
  lit, attr, ty, pat, stmt, expr, item, sourceFile, implItem, traitItem, tokenTree, block
) where

{-
In the future, we may try to do something similar to Rust macros to extract or inject ASTs out or
into the quasiquotes.

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

For now, however, you cannot use @$x@ or @$x:ty@ meta variables.
-}

import Language.Rust.Parser.ParseMonad
import Language.Rust.Parser.Internal
import Language.Rust.Data.InputStream   ( inputStreamFromString )
import Language.Rust.Data.Position      ( Position(..), Span )

import Language.Haskell.TH
import Language.Haskell.TH.Quote        ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

import Control.Applicative              ( (<|>) )
import Control.Monad                    ( (>=>) )
import Data.Functor                     ( ($>) )
import Data.Typeable                    ( cast, Typeable )
import Data.Data                        ( Data )

-- | Given a parser, convert it into a quasiquoter. The quasiquoter produced does not support
-- declarations and types. For patterns, it replaces any 'Span' and 'Position' field with a
-- wild pattern.
quoter :: Data a => P a -> QuasiQuoter
quoter p = QuasiQuoter
             { quoteExp = parse >=> dataToExpQ (const Nothing)
             , quotePat = parse >=> dataToPatQ wildSpanPos
             , quoteDec = error "this quasiquoter does not support declarations"
             , quoteType = error "this quasiquoter does not support types"
             }
  where
  -- | Given a parser and an input string, turn it into the corresponding Haskell expression/pattern.
  parse inp = do
    Loc{ loc_start = (r,c) } <- location
  
    -- Run the parser
    case execParser p (inputStreamFromString inp) (Position 0 r c) of
      Left (ParseFail _ msg) -> fail msg
      Right x -> pure x

  -- | Replace 'Span' and 'Position' with wild patterns
  wildSpanPos :: Typeable b => b -> Maybe (Q Pat)
  wildSpanPos x = ((cast x :: Maybe Span) $> wildP) <|> ((cast x :: Maybe Position) $> wildP)


-- | Quasiquoter for literals (see 'Language.Rust.Syntax.Lit').
--
-- >>> :set -XQuasiQuotes
-- >>> void [lit| 1.4e29f64 |]
-- Float 1.4e29 f64 ()
--
lit :: QuasiQuoter
lit = quoter parseLit

-- | Quasiquoter for attributes (see 'Language.Rust.Syntax.Attribute')
--
-- >>> :set -XQuasiQuotes
-- >>> void [attr| #[no_mangle] |]
-- Attribute Outer (Path False [PathSegment no_mangle Nothing ()] ()) (Stream []) ()
--
attr :: QuasiQuoter
attr = quoter parseAttr

-- | Quasiquoter for types (see 'Language.Rust.Syntax.Ty')
--
-- >>> :set -XQuasiQuotes
-- >>> void [ty| &(_,_) |]
-- Rptr Nothing Immutable (TupTy [Infer (),Infer ()] ()) ()
--
ty :: QuasiQuoter
ty = quoter parseTy

-- | Quasiquoter for patterns (see 'Language.Rust.Syntax.Pat')
--
-- >>> :set -XQuasiQuotes
-- >>> void [pat| x @ 1...5 |]
-- IdentP (ByValue Immutable) x (Just (RangeP (Lit [] (Int Dec 1  ()) ())
--                                            (Lit [] (Int Dec 5  ()) ()) ())) ()
--
pat :: QuasiQuoter
pat = quoter parsePat

-- | Quasiquoter for statements (see 'Language.Rust.Syntax.Stmt')
--
-- >>> :set -XQuasiQuotes
-- >>> void [stmt| let x = 4; |]
-- Local (IdentP (ByValue Immutable) x Nothing ()) Nothing (Just (Lit [] (Int Dec 4  ()) ())) [] ()
--
stmt :: QuasiQuoter
stmt = quoter parseStmt

-- | Quasiquoter for expressions (see 'Language.Rust.Syntax.Expr')
--
-- >>> :set -XQuasiQuotes
-- >>> void [expr| (x,) |]
-- TupExpr [] [PathExpr [] Nothing (Path False [PathSegment x Nothing ()] ()) ()] ()
--
expr :: QuasiQuoter
expr = quoter parseExpr

-- | Quasiquoter for items (see 'Language.Rust.Syntax.Item')
--
-- >>> :set -XQuasiQuotes
-- >>> void [item| type Unit = (); |]
-- TyAlias [] InheritedV Unit (TupTy [] ()) (Generics [] [] (WhereClause [] ()) ()) ()
--
item :: QuasiQuoter
item = quoter parseItem

-- | Quasiquoter for a whole source file (see 'Language.Rust.Syntax.SourceFile')
sourceFile :: QuasiQuoter
sourceFile = quoter parseSourceFile

-- | Quasiquoter for blocks (see 'Language.Rust.Syntax.Block')
--
-- >>> :set -XQuasiQuotes
-- >>> void [block| unsafe { 1i32 } |]
-- Block [NoSemi (Lit [] (Int Dec 1 i32 ()) ()) ()] Unsafe ()
--
block :: QuasiQuoter
block = quoter parseBlock

-- | Quasiquoter for impl items (see 'Language.Rust.Syntax.ImplItem')
--
-- >>> :set -XQuasiQuotes
-- >>> void [implItem| type Item = (); |]
-- TypeI [] InheritedV Final Item (TupTy [] ()) ()
--
implItem :: QuasiQuoter
implItem = quoter parseImplItem

-- | Quasiquoter for trait item (see 'Language.Rust.Syntax.TraitItem')
--
-- >>> :set -XQuasiQuotes
-- >>> void [traitItem| type Item; |]
-- TypeT [] Item [] Nothing ()
--
traitItem :: QuasiQuoter
traitItem = quoter parseTraitItem

-- | Quasiquoter for token trees (see 'Language.Rust.Syntax.TokenTree')
--
-- >>> :set -XQuasiQuotes
-- >>> void [tokenTree| fn |]
-- Token (Span (Position 1 1 14) (Position 3 1 16)) fn
--
tokenTree :: QuasiQuoter
tokenTree = quoter parseTt

