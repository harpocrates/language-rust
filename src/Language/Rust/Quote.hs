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
-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Rust.Quote (
  lit, attr, ty, pat, stmt, expr, item, crate, implItem, traitItem, tokenTree, block,
) where


import Language.Rust.Parser.ParseMonad
import Language.Rust.Parser.Internal
import Language.Rust.Parser.Lexer
import qualified Language.Rust.Syntax.AST as AST
import qualified Language.Rust.Syntax.Ident as AST
import Language.Rust.Data.InputStream (inputStreamFromString)
import Language.Rust.Data.Position (Position(..), Span, Spanned(..))

import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ, dataToPatQ)

import Data.Data (cast, Data)
import Data.Maybe (fromMaybe)
import Data.Traversable (for)

import Control.Exception (evaluate, throw, catch, SomeException, Exception)
import System.IO.Unsafe (unsafePerformIO)

-- You have wandered into the source code for quasiquotation of Rust AST. If you did this in hopes
-- of understanding how this works, prepare to be disgusted by the hack this relies on.
--
-- # How it works!
--
-- ## Expressions:
--
--   1. Lex the quasiquote string to get a list of tokens. Extract the 'SubstNt' tokens and lookup
--      in the 'Q' monad the types of the variables they refer to.
--
--   2. Using this mapping, make a function that when given a variable name from a 'SubstNt' returns
--      a 'Nonterminal' with the right constructor (determined from the type of the variables looked
--      up in step 1), but with the field of that constructor that is 'throw (Subst <variable name>)'.
--
--   3. Lex and parse again the quoted string, using the 'swapToken' function from step 2.
--
--   4. Take the AST Happy produces and scan it for the thrown 'Subst' errors. Theoretically, this
--      shouldn't be possible, but practically, there are unsafe workarounds. Catch those errors and
--      use the string error message to construct a 'VarE <variable name> :: Exp'.
--
-- ## Patterns:
--
--   1. Make a function that when given a 'MatchNt' returns a 'Nonterminal' with the right
--      constructor (determined from the second part of the 'MatchNt'),i but with the field of that
--      constructor that is 'throw (Subst <variable name>)'.
--
--   2. Lex and parse the quoted string, using the 'swapToken' function from step 1.
--
--   3. Take the AST Happy produces and scan it for the thrown 'Subst' errors. Catch those errors and
--      use the string error message to construct a 'VarP <variable name> :: Pat'. Also, convert any
--      'Span' components into wild patterns 'WildP :: Pat'.
--
-- # Why this?
--
-- This is a really, really, _really_ bad hack. So why do it? Because the main goal of
-- 'language-rust' is to be fast and correct, quasiquotes are just a nice to have. In order to make
-- this work cleanly, I would have to:
--
--   * Make the parsing monad polymorphic over another monad, which means turning on
--     'NoMonomorphismRestriction' in the Alex and Happy generated files, which almost certainly
--     kills a lot of optimizations.
--
--   * Parametrize the ASTs so that their fields are in 'm'. For example,
--
--         data Expr m a
--           = Box [m (Attribute m a)] (m (Expr m a)) a
--           | ...
--
--     The 'usual' AST is just 'Expr Identity a'.
--
-- Both of these things mean that code is a lot more polymorphic, which means inlining is tougher,
-- and dictionary arguments may be passed around at runtime. On top of that, the 'Syntax' module,
-- which is where most people will spend their time, will look overly complex.
--
-- Finally, since quasiquotes are "run" at compile-time, their brittleness only affects programs at
-- compile time - the generated code is safe.
--
-- That said, if you can think of a better solution, please submit a PR or email me at
-- <alec.theriault@gmail.com>.


-- | Do the unthinkable: check if evaluating a value to WNHF causes an error and return either the
-- error message or the value. (So much for parametricity...)
fromError :: a -> Either String a
fromError x = unsafePerformIO $ catch (Right x <$ evaluate x) getMessage
  where
    getMessage :: SomeException -> IO (Either String a)
    getMessage = pure . Left . show

-- | Given a parser and an input string, turn it into the corresponding Haskell expression. More
-- details on how this works are given in the long comment (above) after the imports.
expQuoter :: Data a => P a -> String -> Q Exp
expQuoter p inp = do
  Loc{ loc_start = (r,c) } <- location
 
  -- Run the tokenizer once to get all of the 'SubstNt' tokens out
  let inp' = inputStreamFromString inp
      Right toks = execParser (lexTokens lexNonSpace) inp' initPos
      substs = [ n | (Spanned (SubstNt (AST.Ident n _)) _) <- toks ]

  -- Lookup all the strings and associate them with their type constructors 
  subs <- for substs $ \subst -> do
            Just n <- lookupValueName subst
            VarI _ varTy Nothing <- reify n
            let Just conName = outerCon varTy
            pure (subst, conName)

  -- Define the swap function for the tokenizer to use
  let swap :: Token -> Token
      swap tok@(SubstNt (AST.Ident sub _)) = fromMaybe tok $ do
        conName <- lookup sub subs
        func <- lookup conName substExpTable
        pure (Interpolated (func sub))
      swap tok = tok

  -- Run the parser again (and this time with the swapping function)
  case execParser' p inp' (Position 0 r c) swap  of
    Left (_,msg) -> fail msg
    Right x -> dataToExpQ (\y -> case fromError y of
                                   Left ('S':'u':'b':'s':'t':' ':m) -> Just $ do
                                     Just n <- lookupValueName m
                                     varE n
                                   _ -> Nothing)
                          x

-- | Given a parser and an input string, turn it into the corresponding Haskell pattern. More
-- details on how this works are given in the long comment (above) after the imports.
patQuoter :: Data a => P a -> String -> Q Pat
patQuoter p inp = do
  Loc{ loc_start = (r,c) } <- location

  -- Define the swap function for the tokenizer to use
  let swap :: Token -> Token
      swap tok@(MatchNt (AST.Ident var _) (AST.Ident kind _)) = fromMaybe tok $ do
        func <- lookup kind substPatTable
        pure (Interpolated (func var))
      swap tok = tok

  -- Run the parser again (and this time with the swapping function)
  case execParser' p (inputStreamFromString inp) (Position 0 r c) swap  of
    Left (_,msg) -> fail msg
    Right x -> dataToPatQ (\y -> case fromError y of
                                   Left ('S':'u':'b':'s':'t':' ':m) -> Just $ varP (mkName m)
                                   Right _ -> pure WildP <$ (cast y :: Maybe Span)
                                   _ -> Nothing)
                          x


-- | Custom error type to throw and catch
data Subst = Subst String
instance Show Subst where show (Subst s) = "Subst " ++ s
instance Exception Subst

-- | Throw a 'Subst' with a given error message
throwSub :: String -> a
throwSub msg = throw (Subst msg)

-- | Substitution table for the mapping of 'Name' to 'AST.Nonterminal' constructor.
{-# ANN substExpTable "HLint: ignore Avoid lambda" #-}
substExpTable :: [(Name, String -> AST.Nonterminal a)]
substExpTable = 
  [ (''AST.Item,        \m -> AST.NtItem (throwSub m))
  , (''AST.Block,       \m -> AST.NtBlock (throwSub m))
  , (''AST.Stmt,        \m -> AST.NtStmt (throwSub m))
  , (''AST.Pat,         \m -> AST.NtPat (throwSub m))
  , (''AST.Expr,        \m -> AST.NtExpr (throwSub m))
  , (''AST.Ty,          \m -> AST.NtTy (throwSub m))
  , (''AST.Ident,       \m -> AST.NtIdent (throwSub m))
  , (''AST.MetaItem,    \m -> AST.NtMeta (throwSub m))
  , (''AST.Path,        \m -> AST.NtPath (throwSub m))
  , (''AST.TokenTree,   \m -> AST.NtTT (throwSub m))
  , (''AST.Arm,         \m -> AST.NtArm (throwSub m))
  , (''AST.ImplItem,    \m -> AST.NtImplItem (throwSub m))
  , (''AST.TraitItem,   \m -> AST.NtTraitItem (throwSub m))
  , (''AST.Generics,    \m -> AST.NtGenerics (throwSub m))
  , (''AST.WhereClause, \m -> AST.NtWhereClause (throwSub m))
  , (''AST.Arg,         \m -> AST.NtArg (throwSub m))
  , (''AST.Lit,         \m -> AST.NtLit (throwSub m))
  ]

-- | Substitution table for the mapping of 'String' to 'AST.Nonterminal' constructor.
{-# ANN substPatTable "HLint: ignore Avoid lambda" #-}
substPatTable :: [(String, String -> AST.Nonterminal a)]
substPatTable = 
  [ ("item",            \m -> AST.NtItem (throwSub m))
  , ("block",           \m -> AST.NtBlock (throwSub m))
  , ("stmt",            \m -> AST.NtStmt (throwSub m))
  , ("pat",             \m -> AST.NtPat (throwSub m))
  , ("expr",            \m -> AST.NtExpr (throwSub m))
  , ("ty",              \m -> AST.NtTy (throwSub m))
  , ("ident",           \m -> AST.NtIdent (throwSub m))
  , ("meta",            \m -> AST.NtMeta (throwSub m))
  , ("path",            \m -> AST.NtPath (throwSub m))
  , ("tt",              \m -> AST.NtTT (throwSub m))
  , ("literal",         \m -> AST.NtLit (throwSub m))
  ]


-- | Find the outer constructor of a type
outerCon :: Type -> Maybe Name
outerCon (ConT name) = Just name
outerCon (InfixT _ name _) = Just name
outerCon (AppT t _) = outerCon t
outerCon (ForallT _ _ t) = outerCon t
outerCon (SigT t _) = outerCon t
outerCon (ParensT t) = outerCon t
outerCon _ = Nothing

-- | Given a parser, convert it into a quasiquoter
quoter :: Data a => P a -> QuasiQuoter
quoter p = QuasiQuoter
             { quoteExp = expQuoter p
             , quotePat = patQuoter p
             , quoteDec = error "this quasiquoter does not support declarations"
             , quoteType = error "this quasiquoter does not support types"
             }


-- | Quasiquoter for literals (see 'AST.Lit')
lit :: QuasiQuoter
lit = quoter parseLit

-- | Quasiquoter for attributes (see 'AST.Attribute')
attr :: QuasiQuoter
attr = quoter parseAttr

-- | Quasiquoter for types (see 'AST.Ty')
ty :: QuasiQuoter
ty = quoter parseTy

-- | Quasiquoter for pattersn (see 'AST.Pat')
pat :: QuasiQuoter
pat = quoter parsePat

-- | Quasiquoter for statements (see 'AST.Stmt')
stmt :: QuasiQuoter
stmt = quoter parseStmt

-- | Quasiquoter for expressions (see 'AST.Expr')
expr :: QuasiQuoter
expr = quoter parseExpr

-- | Quasiquoter for items (see 'AST.Item')
item :: QuasiQuoter
item = quoter parseItem

-- | Quasiquoter for crates (see 'AST.Crate')
crate :: QuasiQuoter
crate = quoter parseCrate

-- | Quasiquoter for blocks (see 'AST.Block')
block :: QuasiQuoter
block = quoter parseBlock

-- | Quasiquoter for impl items (see 'AST.ImplItem')
implItem :: QuasiQuoter
implItem = quoter parseImplItem

-- | Quasiquoter for trait item (see 'AST.TraitItem')
traitItem :: QuasiQuoter
traitItem = quoter parseTraitItem

-- | Quasiquoter for token trees (see 'AST.TokenTree')
tokenTree :: QuasiQuoter
tokenTree = quoter parseTt

