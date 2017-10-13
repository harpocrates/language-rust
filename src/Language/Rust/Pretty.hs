{-|
Module      : Language.Rust.Pretty
Description : Pretty printing
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Using a pretty printer is as easy as calling 'pretty' or 'prettyAnn' on the AST node of interest.

>>> :set -XOverloadedStrings
>>> import Language.Rust.Syntax
>>> import Language.Rust.Pretty
>>> decl = FnDecl [SelfRegion Nothing Immutable ()] (Just (Never ())) False ()
decl :: FnDecl ()
>>> fn = Fn decl Normal NotConst Rust (Generics [] [] (WhereClause [] ()) ()) (Block [] Normal ()) ()
fn :: ItemKind ()
>>> pretty (Item "foo" [] fn PublicV ())
pub fn foo(&self) -> ! { }
it :: Doc a

-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Language.Rust.Pretty (
  Pretty(..), pretty, PrettyAnnotated(..), prettyAnnotated, Resolve(..), Issue(..), Severity(..), 
  runResolve, Doc, writeSourceFile
) where

import Language.Rust.Data.Position

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident

import Language.Rust.Pretty.Internal
import Language.Rust.Pretty.Resolve

import Data.Text.Prettyprint.Doc (Doc)
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Text (renderIO)
import System.IO (Handle)
import Data.Typeable (Typeable)

-- | Given a handle, write into it the given 'SourceFile' (with file width will be 100).
writeSourceFile :: (Monoid a, Typeable a) => Handle -> SourceFile a -> IO ()
writeSourceFile hdl = renderIO hdl . PP.layoutPretty (PP.LayoutOptions (PP.AvailablePerLine 100 1.0)) . pretty

-- | Resolve and pretty print. When in doubt, this is probably the function you want to use for
-- pretty-printing.
pretty :: (Resolve a, Pretty a) => a -> Doc b
pretty x = case resolve x of
             Left desc -> error ("Failed to resolve: " ++ desc)
             Right y -> prettyUnresolved y

-- | Resolve and pretty print with annotations.
prettyAnnotated :: (Resolve (f a), PrettyAnnotated f) => f a -> Doc a
prettyAnnotated x = case resolve x of
                      Left desc -> error ("Failed to resolve: " ++ desc)
                      Right y -> prettyAnnUnresolved y

-- | Represents things that can be pretty printed
class Pretty a where
  -- | Pretty-print the given value
  prettyUnresolved :: a -> Doc b

instance Pretty Abi                where prettyUnresolved = printAbi
instance Pretty BindingMode        where prettyUnresolved = printBindingMode
instance Pretty BinOp              where prettyUnresolved = printBinOp
instance Pretty Ident              where prettyUnresolved = printIdent
instance Pretty ImplPolarity       where prettyUnresolved = printPolarity
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
instance Pretty (ViewPath a)       where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (Visibility a)     where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (WhereClause a)    where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty (WherePredicate a) where prettyUnresolved = PP.unAnnotate . prettyAnnUnresolved
instance Pretty Position           where prettyUnresolved = PP.pretty . prettyPosition
instance Pretty Span               where prettyUnresolved = PP.pretty . prettySpan

-- | Similar to 'Pretty', but for types which are parametrized over an annotation type.
class PrettyAnnotated p where
  -- | Pretty-print the given value, adding annotations in the 'Doc' whenever possible.
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
instance PrettyAnnotated ViewPath       where prettyAnnUnresolved = printViewPath
instance PrettyAnnotated Visibility     where prettyAnnUnresolved = printVis
instance PrettyAnnotated WhereClause    where prettyAnnUnresolved = printWhereClause True
instance PrettyAnnotated WherePredicate where prettyAnnUnresolved = printWherePredicate

