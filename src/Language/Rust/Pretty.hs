{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DefaultSignatures #-}

module Language.Rust.Pretty (
  -- * Pretty printing
  Pretty(..),
  PrettyAnnotated(..),
) where

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Pretty.Internal

import Text.PrettyPrint.Annotated.WL (Pretty(..), Doc, text)

instance Pretty Abi where pretty = printAbi
instance Pretty BindingMode where pretty = printBindingMode
instance Pretty BinOp where pretty = printBinOp
instance Pretty Ident where pretty = printIdent
instance Pretty ImplPolarity where pretty = printPolarity
instance Pretty LitTok where pretty = printLitTok
instance Pretty Mutability where pretty = printMutability
instance Pretty Name where pretty = printName
instance Pretty RangeLimits where pretty = printRangeLimits
instance Pretty Token where pretty = printToken
instance Pretty TokenTree where pretty = printTt
instance Pretty UnOp where pretty = printUnOp
instance Pretty Unsafety where pretty = printUnsafety

-- | Similar to 'Pretty', but for types which a parametrized over an annotation type.
class PrettyAnnotated p where
  -- | Pretty print the given value, adding annotations in the 'Doc' whenever possible. When
  -- deriving 'PrettyAnnotated', 'pretty' defaults to using the 'Show' instance if there is one.
  prettyAnn :: p a -> Doc a

  default prettyAnn :: Show (p a) => p a -> Doc a
  prettyAnn = text . show

instance PrettyAnnotated Block where prettyAnn = printBlock
instance PrettyAnnotated Crate where prettyAnn = printCrate
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
instance PrettyAnnotated WhereClause where prettyAnn = printWhereClause

