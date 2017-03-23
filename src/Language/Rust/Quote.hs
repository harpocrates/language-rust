{-|
Module      : Language.Rust.Quote
Description : Quasiquotes for Rust AST
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

Quasiquoters for converting Rust code into the equivalent Haskell patterns and expressions. For
now, this doesn't do anything fancy. In the future, we will try to do something similar to Rust
macros to extract/inject ASTs out/into the quasiquotes.

Eventually, one should be able to just import this module for code generation. The following
interaction is what should eventually work.

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

-- | Do the unthinkable: check if a value is causes an error (when evaluated to WNHF) and return
-- either the error message or the value.
fromError :: a -> Either String a
fromError x = unsafePerformIO $ catch (Right x <$ evaluate x) getMessage
  where
    getMessage :: SomeException -> IO (Either String a)
    getMessage = pure . Left . show

-- | Given a parser and an input string, turn it into the corresponding Haskell expression
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
                                   Right _ -> Nothing
                                   Left m -> Just $ do
                                     Just n <- lookupValueName m
                                     varE n)
                          x

-- | Given a parser and an input string, turn it into the corresponding Haskell pattern
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
                                   Right _ -> pure WildP <$ (cast y :: Maybe Span)
                                   Left m -> Just $ varP (mkName m))
                          x


-- | Custom error type to throw and catch
data Subst = Subst String
instance Show Subst where show (Subst s) = s
instance Exception Subst

-- | Throw a 'Subst'
throwSub :: String -> a
throwSub msg = throw (Subst msg)

-- | Substitution table for the mapping of 'Name' to 'AST.Nonterminal' constructor.
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
  ]

-- | Substitution table for the mapping of 'String' to 'AST.Nonterminal' constructor.
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
  ]

-- | Find the outer constructor
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

