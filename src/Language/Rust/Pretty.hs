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

module Language.Rust.Pretty (
  PrettyAnnotated(..), Pretty(..), Resolve(..), Doc
) where

import Language.Rust.Data.Position
import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Pretty.Internal
import Language.Rust.Pretty.Resolve

import Text.PrettyPrint.Annotated.WL (Doc, noAnnotate, text)

-- | Class of things that can be pretty printed (without any annotations). The is very similar to
-- the class defined in 'wl-pprint-annotated' itself. However, in order to avoid having orphan
-- instances or extra instance that don't make sense, we are redefining it.
class Pretty p where
  -- | Pretty-print the given value without any annotations.
  pretty :: p -> Doc a

instance Pretty Abi where pretty = printAbi
instance Pretty BindingMode where pretty = printBindingMode
instance Pretty BinOp where pretty = printBinOp
instance Pretty Ident where pretty = printIdent
instance Pretty ImplPolarity where pretty = printPolarity
instance Pretty LitTok where pretty = printLitTok
instance Pretty Mutability where pretty = printMutability
instance Pretty RangeLimits where pretty = printRangeLimits
instance Pretty Token where pretty = printToken
instance Pretty TokenTree where pretty = printTt
instance Pretty UnOp where pretty = printUnOp
instance Pretty Unsafety where pretty = printUnsafety
instance Pretty (Attribute a) where pretty = noAnnotate . prettyAnn
instance Pretty (Block a) where pretty = noAnnotate . prettyAnn
instance Pretty (SourceFile a) where pretty = noAnnotate . prettyAnn
instance Pretty (Expr a) where pretty = noAnnotate . prettyAnn
instance Pretty (Field a) where pretty = noAnnotate . prettyAnn
instance Pretty (FieldPat a) where pretty = noAnnotate . prettyAnn
instance Pretty (FnDecl a) where pretty = noAnnotate . prettyAnn
instance Pretty (ForeignItem a) where pretty = noAnnotate . prettyAnn
instance Pretty (Generics a) where pretty = noAnnotate . prettyAnn
instance Pretty (ImplItem a) where pretty = noAnnotate . prettyAnn
instance Pretty (InlineAsm a) where pretty = noAnnotate . prettyAnn
instance Pretty (InlineAsmOutput a) where pretty = noAnnotate . prettyAnn
instance Pretty (Item a) where pretty = noAnnotate . prettyAnn
instance Pretty (Lifetime a) where pretty = noAnnotate . prettyAnn
instance Pretty (LifetimeDef a) where pretty = noAnnotate . prettyAnn
instance Pretty (Lit a) where pretty = noAnnotate . prettyAnn
instance Pretty (MetaItem a) where pretty = noAnnotate . prettyAnn
instance Pretty (NestedMetaItem a) where pretty = noAnnotate . prettyAnn
instance Pretty (Nonterminal a) where pretty = noAnnotate . prettyAnn
instance Pretty (Pat a) where pretty = noAnnotate . prettyAnn
instance Pretty (Path a) where pretty = noAnnotate . prettyAnn
instance Pretty (PolyTraitRef a) where pretty = noAnnotate . prettyAnn
instance Pretty (Stmt a) where pretty = noAnnotate . prettyAnn
instance Pretty (StructField a) where pretty = noAnnotate . prettyAnn
instance Pretty (TraitItem a) where pretty = noAnnotate . prettyAnn
instance Pretty (TraitRef a) where pretty = noAnnotate . prettyAnn
instance Pretty (Ty a) where pretty = noAnnotate . prettyAnn
instance Pretty (TyParam a) where pretty = noAnnotate . prettyAnn
instance Pretty (TyParamBound a) where pretty = noAnnotate . prettyAnn
instance Pretty (Variant a) where pretty = noAnnotate . prettyAnn
instance Pretty (ViewPath a) where pretty = noAnnotate . prettyAnn
instance Pretty (Visibility a) where pretty = noAnnotate . prettyAnn
instance Pretty (WhereClause a) where pretty = noAnnotate . prettyAnn
instance Pretty (WherePredicate a) where pretty = noAnnotate . prettyAnn
instance Pretty Position where pretty = text . prettyPosition
instance Pretty Span where pretty = text . prettySpan

-- | Similar to 'Pretty', but for types which are parametrized over an annotation type.
class PrettyAnnotated p where
  -- | Pretty-print the given value, adding annotations in the 'Doc' whenever possible.
  prettyAnn :: p a -> Doc a

-- | This instance prints attributes inline
instance PrettyAnnotated Attribute where prettyAnn = flip printAttr True
instance PrettyAnnotated Block where prettyAnn = printBlock
instance PrettyAnnotated SourceFile where prettyAnn = printSourceFile
instance PrettyAnnotated Expr where prettyAnn = printExpr
instance PrettyAnnotated Field where prettyAnn = printField
instance PrettyAnnotated FieldPat where prettyAnn = printFieldPat
instance PrettyAnnotated FnDecl where prettyAnn = printFnArgsAndRet
instance PrettyAnnotated ForeignItem where prettyAnn = printForeignItem
instance PrettyAnnotated Generics where prettyAnn = printGenerics
instance PrettyAnnotated ImplItem where prettyAnn = printImplItem
instance PrettyAnnotated InlineAsm where prettyAnn = printInlineAsm
instance PrettyAnnotated InlineAsmOutput where prettyAnn = printInlineAsmOutput
instance PrettyAnnotated Item where prettyAnn = printItem
instance PrettyAnnotated Lifetime where prettyAnn = printLifetime
instance PrettyAnnotated LifetimeDef where prettyAnn = printLifetimeDef
instance PrettyAnnotated Lit where prettyAnn = printLit
instance PrettyAnnotated MetaItem where prettyAnn = printMetaItem
instance PrettyAnnotated NestedMetaItem where prettyAnn = printMetaListItem
instance PrettyAnnotated Nonterminal where prettyAnn = printNonterminal
instance PrettyAnnotated Pat where prettyAnn = printPat
instance PrettyAnnotated Path where prettyAnn = flip printPath False
instance PrettyAnnotated PolyTraitRef where prettyAnn = printPolyTraitRef
instance PrettyAnnotated Stmt where prettyAnn = printStmt
instance PrettyAnnotated StructField where prettyAnn = printStructField
instance PrettyAnnotated TraitItem where prettyAnn = printTraitItem
instance PrettyAnnotated TraitRef where prettyAnn = printTraitRef
instance PrettyAnnotated Ty where prettyAnn = printType
instance PrettyAnnotated TyParam where prettyAnn = printTyParam
instance PrettyAnnotated TyParamBound where prettyAnn = printBound
instance PrettyAnnotated Variant where prettyAnn = printVariant
instance PrettyAnnotated ViewPath where prettyAnn = printViewPath
instance PrettyAnnotated Visibility where prettyAnn = printVis
instance PrettyAnnotated WhereClause where prettyAnn = printWhereClause True
instance PrettyAnnotated WherePredicate where prettyAnn = printWherePredicate

