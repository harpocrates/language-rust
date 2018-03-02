{-|
Module      : Language.Rust.Pretty
Description : Pretty printing
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

This module provides functions for turning ASTs into values of type 'Doc'. These values can then be
rendered into concrete string types using functions from the @prettyprinter@ package. This has some
advantages over printing plain old strings:

  * /Backend independent/: you can use a variety of existing backends to efficiently render to all
    sorts of formats like 'Data.Text.Text', 'String', HTML, and terminal.

  * /Dynamic layouts/: the AST will render differently depending on the desired page width 

        >>> :set -XTypeApplications -XOverloadedStrings
        >>> import Language.Rust.Parser
        >>> import Data.Text.Prettyprint.Doc.Util ( putDocW )
        >>> let src = parse' @(SourceFile Span) "fn foo(x: i32, y: i32, z: i32) -> i32 { x - y + z }"
        >>> let doc = pretty' src <> "\n"
        >>> putDocW 80 doc
        fn foo(x: i32, y: i32, z: i32) -> i32 {
            x - y + z
        }
        >>> putDocW 10 doc
        fn foo(
          x: i32,
          y: i32,
          z: i32,
        ) -> i32 {
          x - y + z
        }

  * /Annotations/: Depending on the backend you are using to render the 'Doc', annotations can
    determine colours, styling, links, etc.

The examples below assume the following GHCi flag and import:

>>> :set -XOverloadedStrings
>>> import Language.Rust.Syntax.AST

-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Language.Rust.Pretty (
  -- * Printing
  pretty,
  pretty',
  prettyAnnotated,
  prettyAnnotated',
  writeSourceFile,
  writeTokens,
  Pretty(..),
  PrettyAnnotated(..),
  Doc,
  
  -- * Resolving
  Resolve(..),

  -- * Error reporting
  ResolveFail(..),
  Issue(..),
  Severity(..),
) where

import Language.Rust.Data.Ident
import Language.Rust.Data.Position

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token

import Language.Rust.Pretty.Internal
import Language.Rust.Pretty.Resolve

import System.IO                             ( Handle )
import Data.Typeable                         ( Typeable )
import Data.Text.Prettyprint.Doc.Render.Text ( renderIO )
import Data.Text.Prettyprint.Doc             ( Doc )
import qualified Data.Text.Prettyprint.Doc as PP

import Control.Exception                     ( throw )

-- | Resolve (see the 'Resolve' typeclass) and pretty print something.
--
-- >>> let one = Lit [] (Int Dec 1 Unsuffixed ()) ()
-- >>> let two = Lit [] (Int Dec 2 Unsuffixed ()) ()
-- >>> let three = Lit [] (Int Dec 3 Unsuffixed ()) ()
-- >>> let bogusVar = PathExpr [] Nothing (Path False [PathSegment "let" Nothing ()] ()) ()
-- >>> pretty (Binary [] MulOp (Binary [] AddOp one two ()) three ())
-- Right (1 + 2) * 3
-- >>> pretty (Binary [] AddOp one bogusVar ())
-- Left (invalid AST (identifier `let' is a keyword))
-- 
pretty :: (Resolve a, Pretty a) => a -> Either ResolveFail (Doc b)
pretty = fmap prettyUnresolved . resolve

-- | Same as 'pretty', but throws a 'ResolveFail' exception on invalid ASTs. This function is
-- intended for situations in which you are already stuck catching exceptions - otherwise you should
-- prefer 'pretty'.
--
-- >>> let one = Lit [] (Int Dec 1 Unsuffixed ()) ()
-- >>> let two = Lit [] (Int Dec 2 Unsuffixed ()) ()
-- >>> let three = Lit [] (Int Dec 3 Unsuffixed ()) ()
-- >>> let bogusVar = PathExpr [] Nothing (Path False [PathSegment "let" Nothing ()] ()) ()
-- >>> pretty' (Binary [] MulOp (Binary [] AddOp one two ()) three ())
-- (1 + 2) * 3
-- >>> pretty' (Binary [] AddOp one bogusVar ())
-- *** Exception: invalid AST (identifier `let' is a keyword))
--
pretty' :: (Resolve a, Pretty a) => a -> Doc b
pretty' = either throw id . pretty

-- | Resolve (see the 'Resolve' typeclass) and pretty print something with annotations. Read more
-- about annotations in "Data.Text.Prettyprint.Doc".
--
-- prop> fmap Data.Text.Prettyprint.Doc.noAnnotate . prettyAnnotated = pretty
--
prettyAnnotated :: (Resolve (f a), PrettyAnnotated f) => f a -> Either ResolveFail (Doc a)
prettyAnnotated = fmap prettyAnnUnresolved . resolve

-- | Same as 'prettyAnnotated', but throws a 'ResolveFail' exception on invalid ASTs. This function
-- is intended for situations in which you are already stuck catching exceptions - otherwise you
-- should prefer 'prettyAnnotated'.
--
-- prop> Data.Text.Prettyprint.Doc.noAnnotate . prettyAnnotated' = pretty'
--
prettyAnnotated' :: (Resolve (f a), PrettyAnnotated f) => f a -> Doc a
prettyAnnotated' = either throw id . prettyAnnotated

-- | Given a handle to a file, write a 'SourceFile' in with a desired width of 100 characters.
writeSourceFile :: (Monoid a, Typeable a) => Handle -> SourceFile a -> IO ()
writeSourceFile hdl = renderIO hdl . PP.layoutPretty layout . prettyAnnotated'
  where layout = PP.LayoutOptions (PP.AvailablePerLine 100 1.0)

-- | Given a handle to a file, write a 'SourceFile' in with a desired width of 100 characters.
--
-- The 'Span' associated with the tokens (if present) will be used as a hint for laying out and
-- spacing the tokens.
writeTokens :: Handle -> [Spanned Token] -> IO ()
writeTokens hdl = renderIO hdl . PP.layoutPretty layout . pretty' . Stream . map mkTT
  where layout = PP.LayoutOptions (PP.AvailablePerLine 100 1.0)
        mkTT (Spanned s t) = Tree (Token t s)

-- | Describes things that can be pretty printed.
class Pretty a where
  -- | Pretty print the given value without resolving it.
  prettyUnresolved :: a -> Doc b

instance Pretty Abi                where prettyUnresolved = printAbi
instance Pretty BindingMode        where prettyUnresolved = printBindingMode
instance Pretty BinOp              where prettyUnresolved = printBinOp
instance Pretty Ident              where prettyUnresolved = printIdent
instance Pretty ImplPolarity       where prettyUnresolved = printPolarity
instance Pretty Suffix             where prettyUnresolved = printLitSuffix
instance Pretty LitTok             where prettyUnresolved = printLitTok
instance Pretty Mutability         where prettyUnresolved = printMutability
instance Pretty RangeLimits        where prettyUnresolved = printRangeLimits
instance Pretty Token              where prettyUnresolved = printToken
instance Pretty TokenTree          where prettyUnresolved = printTt
instance Pretty TokenStream        where prettyUnresolved = printTokenStream
instance Pretty UnOp               where prettyUnresolved = printUnOp
instance Pretty Unsafety           where prettyUnresolved = printUnsafety
instance Pretty (Attribute a)      where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Block a)          where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (SourceFile a)     where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Expr a)           where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Field a)          where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (FieldPat a)       where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (FnDecl a)         where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (ForeignItem a)    where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Generics a)       where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (ImplItem a)       where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Item a)           where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Lifetime a)       where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (LifetimeDef a)    where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Lit a)            where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Mac a)            where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Nonterminal a)    where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Pat a)            where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Path a)           where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (PolyTraitRef a)   where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Stmt a)           where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (StructField a)    where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (TraitItem a)      where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (TraitRef a)       where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Ty a)             where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (TyParam a)        where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (TyParamBound a)   where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Variant a)        where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (UseTree a)        where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Visibility a)     where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (WhereClause a)    where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (WherePredicate a) where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty Position           where prettyUnresolved = PP.pretty . prettyPosition
instance Pretty Span               where prettyUnresolved = PP.pretty . prettySpan

-- | Similar to 'Pretty', but for types which are parametrized over an annotation type.
class PrettyAnnotated p where
  -- | Pretty print the given value without resolving it, adding annotations in the 'Doc' whenever
  -- possible.
  prettyAnnUnresolved :: p a -> Doc a

-- | This instance prints attributes inline
instance PrettyAnnotated Attribute      where prettyAnnUnresolved = flip printAttr True
instance PrettyAnnotated Block          where prettyAnnUnresolved = printBlock
instance PrettyAnnotated SourceFile     where prettyAnnUnresolved = printSourceFile
instance PrettyAnnotated Expr           where prettyAnnUnresolved = printExpr
instance PrettyAnnotated Field          where prettyAnnUnresolved = printField
instance PrettyAnnotated FieldPat       where prettyAnnUnresolved = printFieldPat
instance PrettyAnnotated FnDecl         where prettyAnnUnresolved = printFnArgsAndRet
instance PrettyAnnotated ForeignItem    where prettyAnnUnresolved = printForeignItem
instance PrettyAnnotated Generics       where prettyAnnUnresolved = printGenerics
instance PrettyAnnotated ImplItem       where prettyAnnUnresolved = printImplItem
instance PrettyAnnotated Item           where prettyAnnUnresolved = printItem
instance PrettyAnnotated Lifetime       where prettyAnnUnresolved = printLifetime
instance PrettyAnnotated LifetimeDef    where prettyAnnUnresolved = printLifetimeDef
instance PrettyAnnotated Lit            where prettyAnnUnresolved = printLit
instance PrettyAnnotated Mac            where prettyAnnUnresolved = printMac Paren
instance PrettyAnnotated Nonterminal    where prettyAnnUnresolved = printNonterminal
instance PrettyAnnotated Pat            where prettyAnnUnresolved = printPat
instance PrettyAnnotated Path           where prettyAnnUnresolved = flip printPath False
instance PrettyAnnotated PolyTraitRef   where prettyAnnUnresolved = printPolyTraitRef
instance PrettyAnnotated Stmt           where prettyAnnUnresolved = printStmt
instance PrettyAnnotated StructField    where prettyAnnUnresolved = printStructField
instance PrettyAnnotated TraitItem      where prettyAnnUnresolved = printTraitItem
instance PrettyAnnotated TraitRef       where prettyAnnUnresolved = printTraitRef
instance PrettyAnnotated Ty             where prettyAnnUnresolved = printType
instance PrettyAnnotated TyParam        where prettyAnnUnresolved = printTyParam
instance PrettyAnnotated TyParamBound   where prettyAnnUnresolved = printBound
instance PrettyAnnotated Variant        where prettyAnnUnresolved = printVariant
instance PrettyAnnotated UseTree        where prettyAnnUnresolved = printUseTree
instance PrettyAnnotated Visibility     where prettyAnnUnresolved = printVis
instance PrettyAnnotated WhereClause    where prettyAnnUnresolved = printWhereClause True
instance PrettyAnnotated WherePredicate where prettyAnnUnresolved = printWherePredicate

