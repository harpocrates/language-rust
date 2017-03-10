module Language.Rust.Data.Located where

import Language.Rust.Data.Position
import Language.Rust.Syntax.AST

-- | Describes types that can be located - their span can be extracted
class Located a where
  posOf :: a -> Span

instance Located Span where
  posOf = id

instance Located (Spanned a) where
  posOf (Spanned _ s) = s

instance Located a => Located [a] where
  posOf = foldMap posOf

instance Located a => Located (Arg a) where
  posOf (Arg _ _ s) = posOf s
  posOf (SelfValue _ s) = posOf s
  posOf (SelfRegion _ _ s) = posOf s
  posOf (SelfExplicit _ _ s) = posOf s

instance Located a => Located (Arm a) where posOf (Arm _ _ _ _ s) = posOf s
instance Located a => Located (Attribute a) where posOf (Attribute _ _ _ s) = posOf s
instance Located a => Located (Block a) where posOf (Block _ _ s) = posOf s
instance Located a => Located (Crate a) where posOf (Crate _ _ _ _ s) = posOf s

instance Located a => Located (Expr a) where
  posOf (Box _ _ s) = posOf s
  posOf (InPlace _ _ _ s) = posOf s
  posOf (Vec _ _ s) = posOf s
  posOf (Call _ _ _ s) = posOf s
  posOf (MethodCall _ _ _ _ s) = posOf s
  posOf (TupExpr _ _ s) = posOf s
  posOf (Binary _ _ _ _ s) = posOf s
  posOf (Unary _ _ _ s) = posOf s
  posOf (Lit _ _ s) = posOf s
  posOf (Cast _ _ _ s) = posOf s
  posOf (TypeAscription _ _ _ s) = posOf s
  posOf (If _ _ _ _ s) = posOf s
  posOf (IfLet _ _ _ _ _ s) = posOf s
  posOf (While _ _ _ _ s) = posOf s
  posOf (WhileLet _ _ _ _ _ s) = posOf s
  posOf (ForLoop _ _ _ _ _ s) = posOf s
  posOf (Loop _ _ _ s) = posOf s
  posOf (Match _ _ _ s) = posOf s
  posOf (Closure _ _ _ _ s) = posOf s
  posOf (BlockExpr _ _ s) = posOf s
  posOf (Assign _ _ _ s) = posOf s
  posOf (AssignOp _ _ _ _ s) = posOf s
  posOf (FieldAccess _ _ _ s) = posOf s
  posOf (TupField _ _ _ s) = posOf s
  posOf (Index _ _ _ s) = posOf s
  posOf (Range _ _ _ _ s) = posOf s
  posOf (PathExpr _ _ _ s) = posOf s
  posOf (AddrOf _ _ _ s) = posOf s
  posOf (Break _ _ s) = posOf s
  posOf (Continue _ _ s) = posOf s
  posOf (Ret _ _ s) = posOf s
  posOf (InlineAsmExpr _ _ s) = posOf s
  posOf (MacExpr _ _ s) = posOf s
  posOf (Struct _ _ _ _ s) = posOf s
  posOf (Repeat _ _ _ s) = posOf s
  posOf (ParenExpr _ _ s) = posOf s
  posOf (Try _ _ s) = posOf s

instance Located a => Located (Field a) where posOf (Field _ _ s) = posOf s
instance Located a => Located (FieldPat a) where posOf (FieldPat _ _ s) = posOf s
instance Located a => Located (FnDecl a) where posOf (FnDecl _ _ _ s) = posOf s
instance Located a => Located (ForeignItem a) where posOf (ForeignItem _ _ _ _ s) = posOf s
instance Located a => Located (Generics a) where posOf (Generics _ _ _ s) = posOf s
instance Located a => Located (ImplItem a) where posOf (ImplItem _ _ _ _ _ s) = posOf s
instance Located a => Located (InlineAsm a) where posOf (InlineAsm _ _ _ _ _ _ _ _ s) = posOf s
instance Located a => Located (Item a) where posOf (Item _ _ _ _ s) = posOf s
instance Located a => Located (Lifetime a) where posOf (Lifetime _ s) = posOf s
instance Located a => Located (LifetimeDef a) where posOf (LifetimeDef _ _ _ s) = posOf s

instance Located a => Located (Lit a) where
  posOf (Str _ _ _ s) = posOf s
  posOf (ByteStr _ _ _ s) = posOf s
  posOf (Char _ _ s) = posOf s
  posOf (Byte _ _ s) = posOf s
  posOf (Int _ _ s) = posOf s
  posOf (Float _ _ s) = posOf s
  posOf (Bool _ _ s) = posOf s

instance Located a => Located (Mac a) where posOf (Mac _ _ s) = posOf s
instance Located a => Located (MacroDef a) where posOf (MacroDef _ _ _ _ _ _ _ s) = posOf s

instance Located a => Located (MetaItem a) where
  posOf (Word _ s) = posOf s
  posOf (List _ _ s) = posOf s 
  posOf (NameValue _ _ s) = posOf s

instance Located a => Located (NestedMetaItem a) where
  posOf (MetaItem _ s) = posOf s
  posOf (Literal _ s) = posOf s

instance Located a => Located (Pat a) where
  posOf (WildP s) = posOf s
  posOf (IdentP _ _ _ s) = posOf s
  posOf (StructP _ _ _ s) = posOf s
  posOf (TupleStructP _ _ _ s) = posOf s
  posOf (PathP _ _ s) = posOf s
  posOf (TupleP _ _ s) = posOf s
  posOf (BoxP _ s) = posOf s
  posOf (RefP _ _ s) = posOf s
  posOf (LitP _ s) = posOf s
  posOf (RangeP _ _ s) = posOf s
  posOf (SliceP _ _ _ s) = posOf s 
  posOf (MacP _ s) = posOf s

instance Located a => Located (Path a) where posOf (Path _ _ s) = posOf s
instance Located a => Located (PathListItem a) where posOf (PathListItem _ _ s) = posOf s

instance Located a => Located (PathParameters a) where
  posOf (AngleBracketed _ _ _ s) = posOf s
  posOf (Parenthesized _ _ s) = posOf s

instance Located a => Located (PolyTraitRef a) where posOf (PolyTraitRef _ _ s) = posOf s 

instance Located a => Located (Stmt a) where
  posOf (Local _ _ _ _ s) = posOf s
  posOf (ItemStmt _ s) = posOf s
  posOf (NoSemi _ s) = posOf s
  posOf (Semi _ s) = posOf s
  posOf (MacStmt _ _ _ s) = posOf s

instance Located a => Located (StructField a) where posOf (StructField _ _ _ _ s) = posOf s

instance Located TokenTree where
  posOf (Token s _) = s
  posOf (Delimited s _ _ _ _) = s
  posOf (Sequence s _ _ _ _) = s
  
instance Located a => Located (TraitItem a) where posOf (TraitItem _ _ _ s) = posOf s
instance Located a => Located (TraitRef a) where posOf (TraitRef _ s) = posOf s

instance Located a => Located (Ty a) where
  posOf (Slice _ s) = posOf s
  posOf (Array _ _ s) = posOf s
  posOf (Ptr _ _ s) = posOf s
  posOf (Rptr _ _ _ s) = posOf s
  posOf (BareFn _ _ _ _ s) = posOf s
  posOf (Never s) = posOf s
  posOf (TupTy _ s) = posOf s
  posOf (PathTy _ _ s) = posOf s
  posOf (ObjectSum _ _ s) = posOf s
  posOf (PolyTraitRefTy _ s) = posOf s
  posOf (ImplTrait _ s) = posOf s
  posOf (ParenTy _ s) = posOf s
  posOf (Typeof _ s) = posOf s
  posOf (Infer s) = posOf s
  posOf (ImplicitSelf s) = posOf s
  posOf (MacTy _ s) = posOf s

instance Located a => Located (TyParam a) where posOf (TyParam _ _ _ _ s) = posOf s

instance Located a => Located (Variant a) where posOf (Variant _ _ _ _ s) = posOf s

instance Located a => Located (VariantData a) where
  posOf (StructD _ s) = posOf s
  posOf (TupleD _ s) = posOf s
  posOf (UnitD s) = posOf s

instance Located a => Located (ViewPath a) where
  posOf (ViewPathSimple _ _ s) = posOf s
  posOf (ViewPathGlob _ s) = posOf s
  posOf (ViewPathList _ _ s) = posOf s

instance Located a => Located (WhereClause a) where
  posOf (WhereClause _ s) = posOf s

instance Located a => Located (WherePredicate a) where
  posOf (BoundPredicate _ _ _ s) = posOf s
  posOf (RegionPredicate _ _ s) = posOf s
  posOf (EqPredicate _ _ s) = posOf s

