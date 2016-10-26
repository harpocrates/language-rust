module Language.Rust.Pretty (Pretty(..)) where

import Language.Rust.Syntax.AST
import Text.PrettyPrint.HughesPJ


{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

-- lifted from <https://hackage.haskell.org/package/language-c-0.5.0/docs/src/Language-C-Pretty.html#Pretty>

-- | A class of types which can be pretty printed
class Pretty p where
  -- | pretty print the given value
  pretty :: p -> Doc
  pretty = prettyPrec 0
  -- | @prettyPrec prec p@ pretty prints p assuming
  -- that the surrounding context has a precedence of
  -- @prec@
  prettyPrec :: Int -> p -> Doc
  prettyPrec _ = pretty

-- | comma delimited
commaDelimited :: Pretty a => [a] -> Doc
commaDelimited = hsep . punctuate comma . map pretty


commaDelimited' :: [Doc] -> Doc
commaDelimited' = hsep . punctuate comma

-- | comma delimited
commas :: (a -> Doc) -> [a] -> Doc
commas f = hsep . punctuate comma . map f

-- | + delimited
plusDelimited :: Pretty a => [a] -> Doc
plusDelimited = foldr1 (\x y -> x <+> "+" <+> y) . map pretty

-- | | delimited
pipeDelimited :: Pretty a => [a] -> Doc
pipeDelimited = foldr1 (\x y -> x <+> "|" <+> y) . map pretty

perhaps :: Pretty a => Maybe a -> Doc
perhaps Nothing = empty
perhaps (Just a) = pretty a

when :: Bool -> Doc -> Doc
when False _ = empty
when True d = d

-------------------------------------------

-- aka  print_inner_attributes / print_inner_attributes_no_trailing_hardbreak / print_inner_attributes_inline
-- (distinction has to be made at callsite whether to force newline)
printInnerAttributes :: [Attribute a] -> Doc
printInnerAttributes attrs = printEitherAttributes attrs Inner False

-- aka  print_outer_attributes / print_outer_attributes_no_trailing_hardbreak
-- (distinction has to be made at callsite whether to force newline)
printOuterAttributes :: [Attribute a] -> Doc
printOuterAttributes attrs = printEitherAttributes attrs Outer False

-- aka  print_either_attributes
printEitherAttributes :: [Attribute a] -> AttrStyle -> Bool -> Doc
printEitherAttributes attrs kind inline = glue [ printAttribute attr inline | attr <- attrs, style attr == kind ]
  where glue = if inline then hsep else vcat

-- aka  print_attribute_inline / print_attribute
printAttribute :: Attribute a -> Bool -> Doc
printAttribute a@Attribute{..} inline | isSugaredDoc && inline = "/*!" <+> valueStr a <+> "*/"
                                      | isSugaredDoc = "//!" <+> valueStr a 
                                      | style == Inner = "#![" <> printMetaItem value <> "]"
                                      | style == Outer = "#[" <> printMetaItem value <> "]"

valueStr :: Attribute a -> Maybe String
valueStr (Attribute _ (NameValue _  (Str s _ _) _) _ _) = Just s
valueStr _ = Nothing

-- aka  print_meta_list_item
printMetaListItem :: NestedMetaItem a -> Doc
printMetaListItem (MetaItem item _) = printMetaItem item
printMetaListItem (Literal lit _) = printLiteral lit

-- aka  print_meta_item
printMetaItem :: MetaItem a -> Doc
printMetaItem (Word name _) = pretty name
printMetaItem (NameValue name lit _) = pretty name <+> "=" <+> printLiteral lit
printMetaItem (List name items _) = pretty name <> "(" <> commas printMetaListItem items <> ")"

-- aka  print_literal
printLiteral :: Literal a -> Doc
printLiteral = error "Unimplemented"

-- | Synthesizes a comment that was not textually present in the original source file.
-- aka  synth_comment
synthComment :: String -> Doc
synthComment com = "/*" <+> com <+> "*/"


printItem :: Item a -> Doc
printItem Item{..} = case node of
  ExternCrate optionalPath -> printVisibility vis <+> "extern" <+> "crate" 
  Use (ViewPath a)
  Static (Ty a) Mutability (Expr a)
  ConstItem (Ty a) (Expr a)
  Fn (FnDecl a) Unsafety Constness Abi (Generics a) (Block a)
  Mod { items :: [Item a] }
  ForeignMod { abi :: Abi, foreignItems :: [ForeignItem a] }
  TyAlias (Ty a) (Generics a)
  Enum [Variant a] (Generics a)
  StructItem (VariantData a) (Generics a)
  Union (VariantData a) (Generics a)
  Trait Unsafety (Generics a) [TyParamBound a] [TraitItem a]
  DefaultImpl Unsafety (TraitRef a)
  Impl Unsafety ImplPolarity (Generics a) (Maybe (TraitRef a)) (Ty a) [ImplItem a]
  MacItem (Mac a)


printVisibility :: Visibility a -> Doc
printVisibility (PublicV _) = "pub"
printVisibility (CrateV _) = "pub(crate)"
printVisibility (RestrictedV path _) = "pub(" <> printPath path <> ")"
printVisibility (InheritedV _) = empty

printForeignItem :: ForeignItem a -> Doc
printForeignItem = error "Unimplemented"

-- used in printVisibility
printPath :: Path a -> Doc
------------

instance Pretty (Lit a) where
  pretty (Str str Cooked _) = "\"" <> escapeDefault str <> "\""
  pretty (Str str (Raw n) _) = let delim = replicate n '#' in "r" <> delim <> "\"" <> str <> "\"" <> delim
  pretty (ByteStr [w8] _) = 
  pretty (Byte w8 _) = 
  pretty (Char ch _) = 
  pretty (Int w64 litType _) = 
  pretty (Float InternedString FloatTy _)
  pretty (FloatUnsuffixed InternedString _)
  pretty (Bool b _)


instance Pretty UnOp where
  pretty Deref = "*"
  pretty Not = "!"
  pretty Neg = "~"

-- aka  binop_to_string
instance Pretty BinOp where
  pretty AddOp = "+"
  pretty SubOp = "-"
  pretty MulOp = "*"
  pretty DivOp = "/"
  pretty RemOp = "%"
  pretty AndOp = "&&"
  pretty OrOp = "||"
  pretty BitXorOp = "^"
  pretty BitAndOp = "&"
  pretty BitOrOp = "|"
  pretty ShlOp = "<<"
  pretty ShrOp = ">>"
  pretty EqOp = "=="
  pretty LtOp = "<"
  pretty LeOp = "<="
  pretty NeOp = "!="
  pretty GeOp = ">="
  pretty GtOp = ">"

instance Pretty UintTy where
  pretty Us = "usize"
  pretty U8 = "u8"
  pretty U16 = "u16"
  pretty U32 = "u32"
  pretty U64 = "u64"

instance Pretty IntTy where
  pretty Is = "isize"
  pretty I8 = "i8"
  pretty I16 = "i16"
  pretty I32 = "i32"
  pretty I64 = "i64"

instance Pretty Abi where
  pretty Cdecl = "\"cdecl\""
  pretty Stdcall = "\"stdcall\""
  pretty Fastcall = "\"fastcall\""
  pretty Vectorcall = "\"\""
  pretty Aapcs = "\"aapcs\""
  pretty Win64 = "\"win64\""
  pretty SysV64 = "\"sysv64\""
  pretty Rust = "\"Rust\""
  pretty C = "\"C\""
  pretty System = "\"system\""
  pretty RustIntrinsic = "\"rust-intrinsic\""
  pretty RustCall = "\"rust-call\""
  pretty PlatformIntrinsic = "\"platform-intrinsic\""

instance Pretty Unsafety where
  pretty Unsafe = "unsafe"
  pretty Normal = empty

-- aka  print_lifetime
instance Pretty (Lifetime a) where
  pretty Lifetime{..} = pretty name

-- aka  print_lifetime_bounds    -- review if this is really for lifetimedef
instance Pretty (LifetimeDef a) where
  pretty LifetimeDef{..}
    | null bounds = pretty lifetime
    | otherwise = pretty lifetime <> colon <+> (plusDelimited bounds)

-- aka  print_generics
instance Pretty (Generics a) where
  pretty Generics{..}
    | null lifetimes && null tyParams = empty
    | otherwise = "<" <> commaDelimited' ((pretty <$> lifetimes) ++ (pretty <$> tyParams)) <> ">"

-- aka  print_where_clause
instance Pretty (WhereClause a) where
  pretty WhereClause{..}
    | null predicates = empty
    | otherwise = "where" <+> commaDelimited predicates

instance Pretty (PathParameters a) where
  pretty Parenthesized{..} = "(" <> commaDelimited inputs <> ")" <+> maybe empty (\t -> "->" <+> pretty t)
  pretty AngleBracketed{..} = "<" <> commaDelimited' (lifetimes' ++ types' ++ bindings') ">"
    where
      lifetimes' = pretty <$> lifetimes
      types' = pretty <$> types
      bindings' = (\(i,t) -> pretty i <+> equals <+> pretty ty) <$> bindings

formalLifetimeList :: [LifetimeDef a] -> Doc
formalLifetimeList [] = empty
formalLifetimeList ls = "for" <> "<" <> commaDelimited ls <> ">"

instance Pretty (WherePredicate a) where
  pretty BoundPredicate{..} = formalLifetimeList boundLifetimes <+> pretty boundedTy <> colon <+> error 2852
  pretty RegionPredicate{..} = error 
  pretty EqPredicate{..} = printPath path <+> "=" <+> pretty ty

-- | second argument says whether to put colons before params
printPath :: Path a -> Bool -> Doc
printPath Path{..} c = (when global "::") <> hcat (segments >>= \(i,pp) -> [ pretty i, when (c && notNull segments) "::", pretty pp ]

instance Pretty (ViewPath a) where
  pretty (ViewPathSimple i p _) = printPath p False <> when (last (segments p)) ...
  pretty (ViewPathGlob p _) = printPath p False <> "::" <> "*"
  pretty (ViewPathList p ps _) = path <> when (not (isEmpty path)) "::" <> "{" <> commaDelimited ps <> "}"
     where
        path = printPath p False

instance Pretty (PathListItem a) where
  pretty (PathListItem name (Just rename) _) = pretty name <+> "as" <+> pretty rename
  pretty (PathListItem name Nothing _) = pretty name

-- aka  print_mutability
instance Pretty Mutability where
  pretty Mutable = "mut"
  pretty Immutable = empty

-- aka  print_poly_trait_ref
instance Pretty (PolyTraitRef a) where
  pretty PolyTraitRef{..} = formalLifetimeList boundLifetimes <+> pretty traitRef

-- aka  print_trait_ref
instance Pretty (TraitRef a) where
  pretty TraitRef{..} = printPath path False

instance Pretty (Ty a) where
  pretty (Slice t _)           = brackets (pretty t)
  pretty (Array t e _)         = brackets (pretty t <> semi <+> pretty e)
  pretty (Ptr m t _)           = "*" <> pretty m <+> pretty t
  pretty (Rptr l m t _)        = "&" <> hsep [ perhaps l, pretty m ] <+> pretty t
  pretty (BareFn u a l f _)    = error "Unimplemented"
  pretty (Never _)             = "!"
  pretty (TupTy ts _)          = parens (hsep (punctuate comma (pretty <$> ts)))
  pretty (PathTy q p _)        = error "Unimplemented"
  pretty (ObjectSum bs _)      = error "Unimplemented"
  pretty (PolyTraitRefTy bs _) = error "Unimplemented"
  pretty (ImplTrait bs _)      = error "Unimplemented"
  pretty (ParenTy t _)         = parens (pretty t)
  pretty (Typeof e _)          = "typeof" <> parens (pretty e)
  pretty (Infer _)             = "_"
  pretty (ImplicitSelf _)      = "Self"
  pretty (MacTy m _)           = error "Unimplemented"
 
instance Pretty (Pat a) where
  pretty (WildP _) = "_"
  pretty (IdentP (ByRef m) i p _) = "ref" <+> pretty m <+> i <+> maybe empty (\p' -> "@" <> pretty p')
  pretty (IdentP (ByValue Mutable) i p _) = "mut" <+> i <+> maybe empty (\p' -> "@" <> pretty p')
  pretty (IdentP (ByValue Immutable) i p _) = i <+> maybe empty (\p' -> "@" <> pretty p')
  pretty (StructP (Path a) [FieldPat a] Bool a) = 
  pretty (TupleStructP (Path a) [Pat a] (Maybe Word64) a) = 
  pretty (PathP Nothing p _) = printPath True p
  pretty (PathP (Just q) p _) = error "Unimplemented"
  pretty (TupleP [Pat a] (Maybe Word64) a) = 
  pretty (BoxP p _) = "box" <+> p
  pretty (RefP p Mutable _) = "&" <> "mut" <+> pretty p
  pretty (RefP p Immutable _) = "&" <+> pretty p
  pretty (LitP (Expr a) a) = 
  pretty (RangeP (Expr a) (Expr a) a) = 
  pretty (SliceP [Pat a] (Maybe (Pat a)) [Pat a] a) = 
  pretty (MacP (Mac a) a) = 

-- aka  print_arm
instance Pretty (Arm a) where
  pretty Arm{..} = hsep [
    printOuterAttributes attrs,
    pipeDelimited pats,
    maybe empty ("if" <+>) guard,
    "=>",
    error "Unimplemented"
  ] 

expr' :: Expr a -> Doc
expr' 


-- TODO attributes


expr :: Bool -> Expr a -> Doc
expr f (Box a e _)         = withAttrs f a (text "box" <+> e)
expr f (InPlace a e1 e2 _) = withAttrs f a (e1 <+> text "<-" <+> e2) 
expr f (Vec a es _)        = withAttrs f a (brackets (commaDelimited es))
expr f (Call a f es _)     = withAttrs f a (pretty f <> parens (commaDelimited es)
expr f (MethodCall a m tys (e:es) _) = withAttrs f a (hcat [ pretty e, text ".", pretty m, if null es then empty else text "::<" <> commaDelimited tys <> ">", parens (commaDelimited es) ])
expr f (TupExpr a es _) = withAttrs f a (parens (commaDelimited es))
expr f (Binary a BinOp (Expr a) (Expr a) _) = withAttrs f a $ 
expr f (Unary a UnOp (Expr a) _) = withAttrs f a (error "Unimplemented")
expr f (Lit a l _) = withAttrs f a (pretty l)
expr f (Cast a e ty _) = withAttrs f a (pretty e <+> text "as" <+> pretty ty) 
expr f (TypeAscription a (Expr a) (Ty a) _) = withAttrs f a (pretty e <+> colon <+> pretty ty) 
expr f (If a (Expr a) (Block a) (Maybe (Expr a)) _) = withAttrs f a (text "if" <+> pretty )
expr f (IfLet a (Pat a) (Expr a) (Block a) (Maybe (Expr a)) _) = withAttrs f a $ 
expr f (While a (Expr a) (Block a) (Maybe (Ident a)) _) = withAttrs f a $ 
expr f (WhileLet a (Pat a) (Expr a) (Block a) (Maybe (Ident a)) _) = withAttrs f a $ 
expr f (ForLoop a (Pat a) (Expr a) (Block a) (Maybe (Ident a)) _) = withAttrs f a $ 
expr f (Loop a (Block a) (Maybe (Ident a)) _) = withAttrs f a $ 
expr f (Match a (Expr a) [Arm a] _) = withAttrs f a $ 
expr f (Closure a CaptureBy (FnDecl a) (Block a) _) = withAttrs f a $ 
expr f (BlockExpr a (Block a) _) = withAttrs f a $ 
expr f (Assign a (Expr a) (Expr a) _) = withAttrs f a $ 
expr f (AssignOp a BinOp (Expr a) (Expr a) _) = withAttrs f a $ 
expr f (FieldAccess a (Expr a) (Ident a) _) = withAttrs f a $ 
expr f (TupField a (Expr a) Int _) = withAttrs f a $ 
expr f (Index a (Expr a) (Expr a) _) = withAttrs f a $ 
expr f (Range a (Maybe (Expr a)) (Maybe (Expr a)) RangeLimits _) = withAttrs f a $ 
expr f (PathExpr a (Maybe (QSelf a)) (Path a) _) = withAttrs f a $ 
expr f (AddrOf a Mutability (Expr a) _) = withAttrs f a $ 
expr f (Break a (Maybe (Ident a)) _) = withAttrs f a $ 
expr f (Continue a (Maybe (Ident a)) _) = withAttrs f a $ 
expr f (Ret a (Maybe (Expr a)) _) = withAttrs f a $ 
expr f (InlineAsmExpr a (InlineAsm a) _) = withAttrs f a $ 
expr f (MacExpr a (Mac a) _) = withAttrs f a $ 
expr f (Struct a (Path a) [Field a] (Maybe (Expr a)) _) = withAttrs f a $ 
expr f (Repeat a (Expr a) (Expr a) _) = withAttrs f a $ 
expr f (ParenExpr a (Expr a) _) = withAttrs f a $ 
expr f (Try a (Expr a) _) = withAttrs f a $ 




