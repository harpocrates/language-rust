{-# LANGUAGE TemplateHaskell #-}
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
recursively ignore any 'Span' or 'Position' fields (replacing them with wildcard patterns).

Using quasiquotes instead of manually writing out the AST means that even if the AST evolves
(perhaps by adding extra fields to certain nodes), your code is likely to continue to work.

The examples below assume the following GHCi flag and import:

>>> :set -XQuasiQuotes
>>> import Control.Monad ( void )

This quotation library allows a restricted amount of unquoting, which allow you to splice
computed ASTs into a quoted item. In particular, you can:

  * Splice in an identifier (in any number of helpful places, including function names,
    use statements, etc.) by using @$$foo@ where @foo@ is a Haskell identifier in scope
    that has type `Language.Rust.Data.Ident`.
  * Splice in an expression by using @$$(foo)@, where @foo@ is a Haskell identifier in
    scope that has the type `Language.Rust.Syntax.Expr`.
  * Splice in a single statement by using @$${foo}@, where @foo@ is a Haskell identifer
    in scope that has the type `Language.Rust.Syntax.Stmt`.
  * Splice a series of statements into a block by using the @$\@{foo}@ syntax, where @foo@
    is a list of `Language.Rust.Syntax.Stmt`.

Putting it all together, you should be able to write things like the following:

> let initial_value = [expr| 1 |]
>     return_value  = [stmt| return v; |]
>     lines         = replicate 10 [stmt| v = v + 1; |]
> [item|
> fn foo(x: u64) -> u64 {
>   let mut v = $$(initial_value);
>   $@{lines}
>   v = v % 10;
>   $${return_value}
> |]

This will then generate the following Rust:

> fn foo(x: u64) -> u64 {
>   let mut v = 1;
>   v = v + 1;
>   v = v + 1;
>   v = v + 1;
>   v = v + 1;
>   v = v + 1;
>   v = v + 1;
>   v = v + 1;
>   v = v + 1;
>   v = v + 1;
>   v = v + 1;
>   v = v % 10;
>   return v;
> }

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

import Language.Rust.Data.Ident         ( Ident(..) )
import Language.Rust.Parser.ParseMonad
import Language.Rust.Parser.Internal
import Language.Rust.Data.InputStream   ( inputStreamFromString )
import Language.Rust.Data.Position      ( Position(..), Span )
import Language.Rust.Syntax.AST         ( Block(Block), Expr(UnquoteExpr), Stmt(UnquoteSplice,UnquoteStmt), TokenTree(Token) )
import Language.Rust.Syntax.Token       ( Token(UnquoteExprTok,UnquoteStmtTok) )

import Language.Haskell.TH       hiding ( Stmt )
import Language.Haskell.TH.Quote        ( QuasiQuoter(..), dataToExpQ, dataToPatQ )

import Control.Applicative              ( (<|>) )
import Control.Monad                    ( (>=>) )
import Data.Functor                     ( ($>) )
import Data.Generics.Aliases            ( extQ )
import Data.Typeable                    ( cast, Typeable )
import Data.Data                        ( Data )

-- | Given a parser, convert it into a quasiquoter. The quasiquoter produced does not support
-- declarations and types. For patterns, it replaces any 'Span' and 'Position' field with a
-- wild pattern.
quoter :: Data a => P a -> QuasiQuoter
quoter p = QuasiQuoter
           { quoteExp = parse >=> dataToExpQ qqExp
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

-- | Replace quasiquoted items with their appropriate template haskell
-- expansions.
qqExp :: Typeable a => a -> Maybe (Q Exp)
qqExp = const Nothing `extQ` replaceExpressions
                      `extQ` replaceStatements
                      `extQ` replaceNames
                      `extQ` replaceTokens
                      `extQ` replaceSplices

-- | Replace unquotes within expressions with TH references to the given name.
replaceExpressions :: Expr Span -> Maybe (Q Exp)
replaceExpressions (UnquoteExpr n _) = Just (varE (mkName n))
replaceExpressions _ = Nothing

-- | Replace unquotes within statements with TH references to the given name.
replaceStatements :: Stmt Span -> Maybe (Q Exp)
replaceStatements (UnquoteStmt n _) = Just (varE (mkName n))
replaceStatements _ = Nothing

-- | Replace unquoting names with TH references to the given name.
replaceNames :: Ident -> Maybe (Q Exp)
replaceNames (Ident n _ True _) = Just (varE (mkName n))
replaceNames _ = Nothing

-- | Replace unquoted tokens with TH reference to the given name.
replaceTokens :: TokenTree -> Maybe (Q Exp)
replaceTokens (Token _ (UnquoteExprTok s)) = Just (varE (mkName s))
replaceTokens (Token _ (UnquoteStmtTok s)) = Just (varE (mkName s))
replaceTokens _ = Nothing

data BlockPortion a = Normal [Stmt a] | Unquote (Q Exp)

replaceSplices :: Block Span -> Maybe (Q Exp)
replaceSplices (Block stmts unsaf x)
  | all (not . isUnquote) chunks = Nothing
  | otherwise =
      Just [| Block $(buildExpression chunks) $(dataToExpQ qqExp unsaf) $(dataToExpQ qqExp x) |]
 where
  chunks = go stmts []
  --
  go :: [Stmt a] -> [Stmt a] -> [BlockPortion a]
  go [] acc =
    [Normal (reverse acc)]
  go ((UnquoteSplice s _):rest) acc =
    Normal (reverse acc) : Unquote (varE (mkName s)) : go rest []
  go (h:rest) acc =
    go rest (h:acc)
  --
  isUnquote (Unquote _) = True
  isUnquote (Normal  _) = False
  --
  buildExpression :: Data a => [BlockPortion a] -> Q Exp
  buildExpression [] = [| [] |]
  buildExpression ((Unquote q) : rest) =
    [|$(q) ++ ($(buildExpression rest))|]
  buildExpression ((Normal parts) : rest) =
    [| $(listify (map (dataToExpQ qqExp) parts)) ++ $(buildExpression rest) |]
  --
  listify :: [Q Exp] -> Q Exp
  listify []       = [| [] |]
  listify (f:rest) = [| $(f) : $(listify rest) |]

-- | Quasiquoter for literals (see 'Language.Rust.Syntax.Lit'). Unquoting
-- ($$(name)) will not work in a literal.
--
-- >>> void [lit| 1.4e29f64 |]
-- Float 1.4e29 F64 ()
--
lit :: QuasiQuoter
lit = quoter parseLit

-- | Quasiquoter for attributes (see 'Language.Rust.Syntax.Attribute'). Unquoting
-- ($$(name)) will not work in an attribute.
--
-- >>> void [attr| #[no_mangle] |]
-- Attribute Outer (Path False [PathSegment "no_mangle" Nothing ()] ()) (Stream []) ()
--
attr :: QuasiQuoter
attr = quoter parseAttr

-- | Quasiquoter for types (see 'Language.Rust.Syntax.Ty'). Unquoting will not
-- work in a type (currently).
--
-- >>> void [ty| &(_,_) |]
-- Rptr Nothing Immutable (TupTy [Infer (),Infer ()] ()) ()
--
ty :: QuasiQuoter
ty = quoter parseTy

-- | Quasiquoter for patterns (see 'Language.Rust.Syntax.Pat'). Unquoting will
-- not work in a pattern (currently).
--
-- >>> void [pat| x @ 1...5 |]
-- IdentP (ByValue Immutable) "x" (Just (RangeP (Lit [] (Int Dec 1 Unsuffixed ()) ())
--                                              (Lit [] (Int Dec 5 Unsuffixed ()) ()) ())) ()
--
pat :: QuasiQuoter
pat = quoter parsePat

-- | Quasiquoter for statements (see 'Language.Rust.Syntax.Stmt'). Unquoting
-- will work in a statement block, either as a subexpression, substatement, or
-- (weirdly) the statement itself.
--
-- >>> void [stmt| let x = 4i32; |]
-- Local (IdentP (ByValue Immutable) "x" Nothing ()) Nothing (Just (Lit [] (Int Dec 4 I32 ()) ())) [] ()
--
stmt :: QuasiQuoter
stmt = quoter parseStmt

-- | Quasiquoter for expressions (see 'Language.Rust.Syntax.Expr')
--
-- >>> void [expr| (x,) |]
-- TupExpr [] [PathExpr [] Nothing (Path False [PathSegment "x" Nothing ()] ()) ()] ()
--
expr :: QuasiQuoter
expr = quoter parseExpr

-- | Quasiquoter for items (see 'Language.Rust.Syntax.Item')
--
-- >>> void [item| type Unit = (); |]
-- TyAlias [] InheritedV "Unit" (TupTy [] ()) (Generics [] [] (WhereClause [] ()) ()) ()
--
item :: QuasiQuoter
item = quoter parseItem

-- | Quasiquoter for a whole source file (see 'Language.Rust.Syntax.SourceFile')
--
-- >>> void [sourceFile| fn main() { } |]
-- SourceFile Nothing [] [Fn [] InheritedV "main"
--                           (FnDecl [] Nothing False ())
--                           Normal NotConst Rust
--                           (Generics [] [] (WhereClause [] ()) ())
--                           (Block [] Normal ()) ()]
--
sourceFile :: QuasiQuoter
sourceFile = quoter parseSourceFile

-- | Quasiquoter for blocks (see 'Language.Rust.Syntax.Block')
--
-- >>> void [block| unsafe { 1i32 } |]
-- Block [NoSemi (Lit [] (Int Dec 1 I32 ()) ()) ()] Unsafe ()
--
block :: QuasiQuoter
block = quoter parseBlock

-- | Quasiquoter for impl items (see 'Language.Rust.Syntax.ImplItem')
--
-- >>> void [implItem| type Item = (); |]
-- TypeI [] InheritedV Final "Item" (TupTy [] ()) ()
--
implItem :: QuasiQuoter
implItem = quoter parseImplItem

-- | Quasiquoter for trait items (see 'Language.Rust.Syntax.TraitItem')
--
-- >>> void [traitItem| type Item; |]
-- TypeT [] "Item" [] Nothing ()
--
traitItem :: QuasiQuoter
traitItem = quoter parseTraitItem

-- | Quasiquoter for token trees (see 'Language.Rust.Syntax.TokenTree')
--
-- >>> [tokenTree| fn |]
-- Token (Span (Position 1 2 14) (Position 3 2 16)) fn
--
tokenTree :: QuasiQuoter
tokenTree = quoter parseTt

