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
commas :: [a] -> (a -> Doc) -> Doc
commas xs f = hsep . punctuate comma . map f xs

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
-- aka print_stmt
printStmt :: Stmt a -> Doc
printStmt Local{..} = 
  printOuterAttributes attrs
    <+> "let" <+> printPat pat <> (maybe empty (\t -> ":" <+> printType t) ty)
    <+> (maybe empty (\e -> "=" <+> printExpr e) init) <> ";"
printStmt (ItemStmt item _) = printItem item
printStmt (NoSemi expr _) = printExprOuterAttrStyle expr false <> when (exprRequiresSemiToBeStmt expr) ";"
printStmt (Semi expr _) = printExprOuterAttrStyle expr false <> ";"
printStmt (MacStmt m ms attrs _) = error "Unimplemented"


-- aka parse::classify::expr_requires_semi_to_be_stmt
exprRequiresSemiToBeStmt :: Expr a -> Bool


-- print_expr_outer_attr_style
-- Inlined print_expr_in_place
-- Inlined print_expr_call
-- Inlined print_expr_method_call
-- Inlined print_expr_tup
-- Inlined print_expr_binary
-- Inlined print_expr_unary
-- Inlined print_expr_addr_of
printExprOuterAttrStyle :: Expr a -> Bool -> Doc
printExprOuterAttrStyle expr isInline = printOuterAttributes (expressionAttrs expr) <+>
  case expr of
    Box _ expr' _ -> "box" <+> printExpr expr'
    InPlace _ place expr _ -> printExprMaybeParen place <+> "<-" <+> printExprMaybeParen expr
    Vec attrs exprs _ -> "[" <> printInnerAttributes attrs <+> commas exprs printExpr <> "]"
    Call _ func args _ -> printExprMaybeParen func <> "(" <> commas args printExpr <> ")"
    MethodCall _ ident tys (self:args) _ -> printExpr self <> "." <> printIdent ident <> when (not (null tys)) ("::<" <> commas tys printType <> ">") <> "(" <> commas args printExpr <> ")"
    TupExpr attrs exprs _ -> "(" <> printInnerAttributes attrs <> commas exprs printExpr <> when (length exprs == 1) "," <> ")"
    Binary _ op lhs rhs _ -> checkExprBinNeedsParen lhs op <+> printBinop op <+> checkExprBinNeedsParen lhs op
    Unary _ op expr _ -> printUnop op <> printExprMaybeParen expr
    Lit _ lit _ -> printLiteral lit
    Cast _ expr ty _ -> case expr of { Cast{} -> printExpr expr; _ -> printExprMaybeParen expr } <+> "as" <+> printType ty
    TypeAscription [Attribute a] (Expr a) (Ty a) a
    If [Attribute a] (Expr a) (Block a) (Maybe (Expr a)) a
    IfLet [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Expr a)) a
    While [Attribute a] (Expr a) (Block a) (Maybe (Ident a)) a
    WhileLet [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Ident a)) a
    ForLoop [Attribute a] (Pat a) (Expr a) (Block a) (Maybe (Ident a)) a
    Loop [Attribute a] (Block a) (Maybe (Ident a)) a
    Match [Attribute a] (Expr a) [Arm a] a
    Closure [Attribute a] CaptureBy (FnDecl a) (Block a) a
    BlockExpr [Attribute a] (Block a) a
    Assign [Attribute a] (Expr a) (Expr a) a
    AssignOp [Attribute a] BinOp (Expr a) (Expr a) a
    FieldAccess [Attribute a] (Expr a) (Ident a) a
    TupField [Attribute a] (Expr a) Int a
    Index [Attribute a] (Expr a) (Expr a) a
    Range [Attribute a] (Maybe (Expr a)) (Maybe (Expr a)) RangeLimits a
    PathExpr [Attribute a] (Maybe (QSelf a)) (Path a) a
    AddrOf _ mut expr _ -> "&" <> printMutability mut <+> printExprMaybeParen expr
    Break _ brk _ -> "break" <+> maybe empty printIdent cont
    Continue _ cont _ -> "continue" <+> maybe empty printIdent cont
    Ret _ result _ -> "return" <+> maybe empty printExpr result
    InlineAsmExpr _ inlineAsm _ -> error "Unimplemented"
    MacExpr _ m _ -> error "Unimplemented"
    Struct [Attribute a] (Path a) [Field a] (Maybe (Expr a)) a
    Repeat [Attribute a] (Expr a) (Expr a) a
    ParenExpr attrs expr _ -> "(" <> printInnerAttributes attrs <+> printExpr expr <> ")"
    Try _ expr _ -> printExpr expr <> "?"

-- no similar
printBinop :: BinOp
printBinop AddOp = "+"
printBinop SubOp = "-"
printBinop MulOp = "*"
printBinop DivOp = "/"
printBinop RemOp = "%"
printBinop AndOp = "&&"
printBinop OrOp = "||"
printBinop BitXorOp = "^"
printBinop BitAndOp = "&"
printBinop BitOrOp = "|"
printBinop ShlOp = "<<"
printBinop ShrOp = ">>"
printBinop EqOp = "=="
printBinop LtOp = "<"
printBinop LeOp = "<="
printBinop NeOp = "!="
printBinop GeOp = ">="
printBinop GtOp = ">"

-- no similar
printUnop :: Unop -> Doc
printUnop Deref = "*"
printUnop Not = "!"
printUnop Neg = "~"

-- aka print_literal
printLiteral :: Lit a -> Doc


-- similar to check_expr_bin_needs_paren
checkExprBinNeedsParen :: Expr a -> BinOp -> Doc
checkExprBinNeedsParen e@(Binary _ op' _ _ _) op | opPrecedence op' < opPrecedence op = "(" <> printExpr e <> ")"
checkExprBinNeedsParen e = printExpr e

-- aka print_expr_maybe_paren
printExprMaybeParen :: Expr a -> Doc
printExprMaybeParen expr = when needs "(" <> printExpr expr <> when needs ")"
  where needs = needsParentheses expr 

-- aka needs_parentheses
needsParentheses :: Expr a -> Bool
needsParentheses @Assign{} = True
needsParentheses @Binary{} = True
needsParentheses @Closure{} = True
needsParentheses @AssignOp{} = True
needsParentheses @Cast{} = True
needsParentheses @InPlace{} = True
needsParentheses @Type{} = True
needsParentheses _ = False

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
printMetaItem (List name items _) = pretty name <> "(" <> commas items printMetaListItem <> ")"

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
  Use vp -> printVisibility vis <+> "use" <+> printViewPath vp <> ";"
  Static ty m expr -> printVisibility vis <+> "static" <+> when (m == Mutable) "mut" <+> printIdent ident <> ":" <+> printType ty <+> "=" <+> printExpr <> ";"
  ConstItem ty expr -> printVisibility vis <+> "const" <+> printIdent ident <> ":" <+> printType ty <+> "=" <+> printExpr expr <> ";"
  Fn decl unsafety constness abi tyParams body -> printFn decl unsafety constness abi (Just ident) tyParams vis <+> printBlockWithAttrs body attrs
  Mod items -> printVisibility vis <+> "mod" <+> printIdent ident <> "{" <> printMod items attrs <> "}"
  ForeignMod abi foreignItems -> "extern" <+> abi <> "{" printForeignMod foreignItems attrs <> "}"
  TyAlias ty params -> printVisibility vis <+> "type" <+> printIdent ident <> printGenerics params <> printWhereClause (whereClause params) <+> "=" <+> printType ty <> ";"
  Enum variants params -> printEnumDef variants params ident vis
  StructItem structDef generics -> printVisibility vis <+> "struct" <+> printStruct structDef generics ident True
  Union structDef generics -> printVisibility vis <+> "union" <+> printStruct structDef generics ident True
  DefaultImpl unsafety traitRef -> printVisibility vis <+> printUnsafety unsafe <+> "impl" <> printTraitRef traitRef <+> "for" <+> ".." <+> "{ }"
  Impl unsafety polarity generics traitRef_m ty implItems ->
      printVisibility vis <+> printUnsafety unsafety <+> "impl"
        <+> (case generics of { Generics [] [] _ _ -> empty; _ -> printGenerics generics })
        <+> (case traitRef_m of { Just trait -> ; _ -> empty })
        <+> printType ty
        <+> printWhereClause (whereClause generics)
        <+> "{" <+> printInnerAttributes attrs <+> hsep (printImplItem <$> implItems) <+> "}"
  Trait unsafety generics typarambounds traitItems ->
      let seperateBounds :: [TyParamBound a] -> ([Doc], [TyParamBound a])
          seperateBounds [] = ([],[])
          seperateBounds (t:ts) = case t of
              TraitTyParamBound ptr Maybe : ts) = (("for ?" <+> printTraitRef (traitRef ptr)) : ds,tybs)
              _ -> (ds, t:tybs)
            where ~(ds,tybs) = seperateBounds ts

          (bounds,realBounds) = seperateBounds typarambounds
      in printVisibility vis <+> printUnsafety unsafety <+> "trait" <+> printIdent ident
            <+> printGenerics generics
            <+> hsep bounds
            <+> hsep (printBounds ":" realBounds)
            <+> printWhereClause (whereClause generics)
            <+> "{" <+> hsep (printTraitItem <+> traitItems) <+> "}"
  MacItem m -> error "Unimplemented (pprust.rs line 1333)"


-- aka print_trait_item
printTraitItem :: TraitItem a -> Doc
printTraitItem TraitItem{..} = printOuterAttributes attrs <+>
  case node of ->
    ConstT ty default_m -> printAssociatedConst ident ty default_m InheritedV
    MethodT sig block_m -> printMethodSig ident sig InheritedV <+> maybe ";" (\body -> printBlockWithAttrs body attrs) block_m
    TypeT bounds default_m -> printAssociatedType ident (Just bounds) default_m
    MacroT m -> error "Unimplemented (pprust.rs line 1572)"


-- aka print_bounds
printBounds :: Doc -> [TyParamBound a] -> Doc

-- aka print_impl_item
printImplItem :: ImplItem a -> Doc
printImplItem ImplItem{..} =
  printOuterAttributes attrs
    <+> (case defaultness of { Default -> "default"; Final -> empty })
    <+> (case node of
            ConstI ty expr -> printAssociatedConst ident ty (Just expr) vis
            MethodI sig body -> printMethodSig ident sig vis <+> printBlockWithAttrs body attrs
            TypeI ty -> printAssociatedType ident Nothing (Just ty) 
            MacroI m -> error "Unimplemented (pprust.rs line 1608")

-- aka printAssociatedType
printAssociatedType :: Ident a ->  Maybe _ -> Maybe (Ty a) -> Doc
printAssociatedType ident bounds_m ty_m = "type" <+> printIdent ident
  <+> maybe empty (printBounds ":") bounds_m
  <+> maybe empty (\ty -> "=" <+> printType ty) ty_m
  <> ";"

-- aka print_method_sig
printMethodSig :: Ident a -> MethodSig a -> Visibility a -> Doc
printMethodSig ident MethodSig{..} vis = printFn decl unsafety (node constness) abi (Just ident) generics vis 

-- aka print_associated_const
printAssociatedConst :: Ident a -> Ty a -> Maybe (Expr a) -> Visibility a -> Doc
printAssociatedConst ident ty default_m vis = printVisibility vis
  <+> "const" <+> printIdent ident <> ":" <+> printType ty
  <+> maybe empty (\expr -> "=" <+> printExpr expt) default_m
  <> ";"

-- no aka
printPolarity :: ImplPolarity -> Doc
printPolarity Negative = "!"
printPolarity Positive = empty

-- aka print_trait_ref
printTraitRef :: TraitRef a -> Doc
printTraitRef TraitRef{..} = printPath path False

-- aka print_visibility
printVisibility :: Visibility a -> Doc
printVisibility (PublicV _) = "pub"
printVisibility (CrateV _) = "pub(crate)"
printVisibility (RestrictedV path _) = "pub(" <> printPath path <> ")"
printVisibility (InheritedV _) = empty

-- aka print_foreign_item
printForeignItem :: ForeignItem a -> Doc
printForeignItem ForeignItem{..} = printOuterAttributes attrs <+>
  case node of
    ForeignFn decl generics -> printFn decl Normal NotConst <> ";"
    ForeignStatic ty mut -> printVisibility vis <+> "static" <+> when mut "mut" <+> ident <> ":" <+> printType ty <> ";"


-- aka print_struct
printStruct :: VariantData a -> Generics a -> Ident a -> Bool -> Doc
printStruct structDef generics{ whereClause = whereClause } ident printFinalizer =
  printIdent ident <+> printGenerics generics <+> case structDef of 
    StructD fields _ -> printWhereClause whereClause <+> "{" <+> commas fields printStructField <+> "}"
    TupleD fields _ -> "(" <> commas fields printStructField <> ")" <+> printWhereClause whereClause <+> when printFinalizer ";" 
    UnitD _ -> "()" <+> printWhereClause whereClause <+> when printFinalizer ";"


printStructField :: StructField a -> Doc
printStructField StructField{..} =
  printOuterAttributes attrs
    <+> printVisibility vis
    <+> maybe empty ident (\i -> printIdent i <> ":") <+> ty


-- aka print_unsafety
printUnsafety :: Unsafety -> Doc
printUnsafety Normal = empty
printUnsafety Unsafe = "unsafe"

-- aka print_enum_def
printEnumDef :: [Variant a] -> Generics a -> Ident a -> Visibility -> Doc

-- aka print_where_clause
printWhereClause :: WhereClause a -> Doc

-- used in printVisibility
printPath :: Path a -> Doc

-- aka print_view_path
printViewPath :: ViewPath a -> Doc

-- aka  print_fn
printFn :: FnDecl a -> Unsafety -> Constness -> Abi -> Maybe (Ident a) -> Generics a -> Visibility a -> Doc
printFn decl unsafety constness abi name generics vis =
  printFnHeaderInfo unsafety constness abi vis
    <+> maybe empty printIdent name
    <> printGenerics generics
    <> printFnArgsAndRet decl
    <+> printWhereClause (whereClause generics)

-- aka print_fn_args_and_ret
printFnArgsAndRet :: FnDecl a -> Doc
printFnArgsAndRet FnDecl{..} = "(" <> commas inputs (\arg -> printArg arg False) <> when variadic ", ..." <> ")" 

-- aka print_arg TODO double check this
printArg :: Arg a -> Bool -> Doc
printArg Arg{..} isClosure = case node of
  Infer | isClosure -> printPat pat
  _ otherwise -> let invalid = case pat of IdentP _ ident _ _ -> ident == "" -- TODO define constant `invalid :: Ident a`
                                    _ -> False
                  in when (not invalid) (printPat pat <> ":") <+> printTy ty

-- print_explicit_self
printExplicitSelf :: SelfKind a -> Doc
printExplicitSelf (ValueSelf mut) = printMutability mut <+> "self"
printExplicitSelf (Region lifetime_m mut) = "&" <> maybe empty printLifetime lifetime_m <+> printMutability mut <+> "self"
printExplicitSelf (Explicit ty mut) = printMutability mut <+> "self" <> ":" <+> printType ty

-- aka print_lifetime
printLifetime :: Lifetime a -> Doc
printLifetime Lifetime{..} = printName name

-- print_mutability
printMutability :: Mutability -> Doc
printMutability Mutable = "mut"
printMutability Immutable = empty

-- print_pat
printPat :: Pat a -> Doc
printPat (WildP _) = "_"
printPat (IdentP bindingMode path1 sub _) = printBindingMode bindingMode <+> printIdent path1 <+> maybe empty (\p -> "@" <> printPat p) sub
printPat (StructP (Path a) [FieldPat a] Bool _) = 
printPat (TupleStructP path elts ddpos _) = 
printPat (PathP (Maybe (QSelf a)) (Path a) _)
printPat (TupleP [Pat a] (Maybe Word64) _)
printPat (BoxP inner _) = "box" <+> printPat inner
printPat (RefP inner mutbl _) = "&" <> printMutability mutbl <+> printPat inner
printPat (LitP expr _) = printExpr expr
printPat (RangeP lo hi _) = printExpr lo <+> "..." <+> printExpr hi
printPat (SliceP before slice_m after _) = "[" <> commas before printPat
printPat (MacP (Mac a) _) =


printBindingMode :: BindingMode a -> Doc
printBindingMode (ByRef mutbl) = "ref" <+> printMutability
printBindingMode (ByValue Immutable) = empty
printBindingMode (ByValue Mutable) = "mut"


-- aka  print_fn_header_info
printFnHeaderInfo :: Unsafety -> Constness -> Abi -> Visibility a -> Doc
printFnHeaderInfo unsafety constness abi vis =
  printVisibility vis
  <+> (case constness of { Const -> "const"; _ -> empty })
  <+> printUnsafety safety
  <+> printAbi abi
  <+> "fn"

printAbi :: Abi -> Doc
printAbi Rust = empty
printAbi abi = "extern" <+> "\"" <> show abi <> "\""

-- aka print_block_with_attrs
printBlockWithAttrs :: Block a -> [Attribute a] -> Doc

-- aka print_mod
printMod :: [Item a] -> [Attribute a] -> Doc
printMod items attrs = printInnerAttributes attrs <+> hsep (printItem <$> items)

-- aka print_foreign_mod
printForeignMod :: [ForeignItem a] -> [Attribute a] -> Doc
printForeignMod items attrs = printInnerAttributes attrs <+> hsep (printForeignItem <$> items)

-- aka  print_generics
printGenerics :: Generics a -> Doc
printGenerics Generics{..}
  | null lifetimes && null tyParams = empty
  | otherwise =  let lifetimes = [ printOuterAttributes attrs <+> printLifetimeBounds lifetime bounds | l@(Lifetime attrs lifetime bounds _)<-lifetimes ]
                     bounds = [ printTyParam param | param<-tyParams ]
                 in "<" <> hsep (punctuate "," (punctuatelifetime ++ bounds)) <> ">"
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




