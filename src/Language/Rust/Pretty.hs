{-# LANGUAGE RecordWildCards, OverloadedStrings, DuplicateRecordFields #-}

module Language.Rust.Pretty where

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Ident
import Text.PrettyPrint.HughesPJ (hsep, hcat, vcat, punctuate, sep, (<>), (<+>), Doc, empty, text)

-- | comma delimited
commas :: [a] -> (a -> Doc) -> Doc
commas xs f = hsep (punctuate "," (map f xs))

when :: Bool -> Doc -> Doc
when False _ = empty
when True d = d

notNull :: [a] -> Bool
notNull = not . null

perhaps :: (a -> Doc) -> Maybe a -> Doc
perhaps = maybe empty

-------------------------------------------

printIdent :: Ident -> Doc
printIdent Ident{..} = let (Name s) = name in text s

-- aka print_type
-- Inlined print_ty_fn
printType :: Ty a -> Doc
printType (Slice ty _) = "[" <> printType ty <> "]"
printType (Array ty v _) = "[" <> printType ty <> ";" <+> printExpr v <> "]"
printType (Ptr mut ty _) = "*" <> printFullMutability mut <+> printType ty
printType (Rptr lifetime mut ty _) = "&" <> perhaps printLifetime lifetime <+> printMutability mut <+> printType ty
printType (BareFn unsafety abi lifetimes decl _) =
  when (notNull lifetimes) ("for" <+> "<" <> printFormalLifetimeList lifetimes <> ">")
    <+> printFnHeaderInfo unsafety NotConst abi InheritedV
    <+> printFnArgsAndRet decl
printType (Never _) = "!"
printType (TupTy [elt] _) = "(" <> printType elt <> ",)"
printType (TupTy elts _) = "(" <> commas elts printType <> ")"
printType (PathTy Nothing path _) = printPath path False
printType (PathTy (Just qself) path _) = printQPath path qself False
printType (ObjectSum ty bounds _) = printType ty <+> printBounds "+" bounds
printType (PolyTraitRefTy bounds _) = printBounds "" bounds
printType (ImplTrait bounds _) = printBounds "impl " bounds
printType (ParenTy ty _) = "(" <> printType ty <> ")"
printType (Typeof e _) = "typeof(" <> printExpr e <> ")"
printType (Infer _) = "_"
printType (ImplicitSelf _) = "Self"
printType (MacTy m _) = error "Unimplemented"


-- aka print_stmt
printStmt :: Stmt a -> Doc
printStmt Local{..} = 
  printOuterAttributes attrs
    <+> "let" <+> printPat pat <> (perhaps (\t -> ":" <+> printType t) ty)
    <+> (perhaps (\e -> "=" <+> printExpr e) init) <> ";"
printStmt (ItemStmt item _) = printItem item
printStmt (NoSemi expr _) = printExprOuterAttrStyle expr False <> when (exprRequiresSemiToBeStmt expr) ";"
printStmt (Semi expr _) = printExprOuterAttrStyle expr False <> ";"
printStmt (MacStmt m ms attrs _) = error "Unimplemented"


-- aka parse::classify::expr_requires_semi_to_be_stmt
exprRequiresSemiToBeStmt :: Expr a -> Bool
exprRequiresSemiToBeStmt If{} = False
exprRequiresSemiToBeStmt IfLet{} = False
exprRequiresSemiToBeStmt While{} = False
exprRequiresSemiToBeStmt WhileLet{} = False
exprRequiresSemiToBeStmt ForLoop{} = False
exprRequiresSemiToBeStmt Loop{} = False
exprRequiresSemiToBeStmt Match{} = False
exprRequiresSemiToBeStmt BlockExpr{} = False
exprRequiresSemiToBeStmt _ = True


printExpr :: Expr a -> Doc
printExpr expr = printExprOuterAttrStyle expr True

-- print_expr_outer_attr_style
-- Inlined print_expr_in_place
-- Inlined print_expr_call
-- Inlined print_expr_method_call
-- Inlined print_expr_tup
-- Inlined print_expr_binary
-- Inlined print_expr_unary
-- Inlined print_expr_addr_of
-- Inlined print_if
-- Inlined print_if_let
-- Inlined print_expr_repeat
printExprOuterAttrStyle :: Expr a -> Bool -> Doc
printExprOuterAttrStyle expr isInline = printOuterAttributes (expressionAttrs expr) <+>
  case expr of
    Box _ expr' _ -> "box" <+> printExpr expr'
    InPlace _ place expr _ -> printExprMaybeParen place <+> "<-" <+> printExprMaybeParen expr
    Vec attrs exprs _ -> "[" <> printInnerAttributes attrs <+> commas exprs printExpr <> "]"
    Call _ func args _ -> printExprMaybeParen func <> "(" <> commas args printExpr <> ")"
    MethodCall _ ident tys (self:args) _ -> printExpr self <> "." <> printIdent ident <> when (not (null tys)) ("::<" <> commas tys printType <> ">") <> "(" <> commas args printExpr <> ")"
    TupExpr attrs exprs _ -> "(" <> printInnerAttributes attrs <> commas exprs printExpr <> when (length exprs == 1) "," <> ")"
    Binary _ op lhs rhs _ -> checkExprBinNeedsParen lhs op <+> printBinOp op <+> checkExprBinNeedsParen lhs op
    Unary _ op expr _ -> printUnOp op <> printExprMaybeParen expr
    Lit _ lit _ -> printLiteral lit
    Cast _ expr ty _ -> case expr of { Cast{} -> printExpr expr; _ -> printExprMaybeParen expr } <+> "as" <+> printType ty
    TypeAscription _ expr ty _ -> printExpr expr <> ":" <+> printType ty
    If _ test blk els _ -> "if" <+> printExpr test <+> printBlock blk <+> printElse els
    IfLet _ pat expr blk els _ -> "if let" <+> printPat pat <+> "=" <+> printExpr expr <+> printBlock blk <+> printElse els
    While attrs test blk label _ -> perhaps (\i -> printIdent i <> ":") label <+> "while" <+> printExpr test <+> printBlockWithAttrs blk attrs
    WhileLet attrs pat expr blk label _ -> perhaps (\i -> printIdent i <> ":") label <+> "while let" <+> printPat pat <+> "=" <+> printExpr expr <+> printBlockWithAttrs blk attrs
    ForLoop attrs pat expr blk label _ -> perhaps (\i -> printIdent i <> ":") label <+> "for"  <+> printPat pat <+> "in" <+> printExpr expr <+> printBlockWithAttrs blk attrs
    Loop attrs blk label _ -> perhaps (\i -> printIdent i <> ":") label <+> "loop" <+> printBlockWithAttrs blk attrs
    Match attrs expr arms _ -> "match" <+> printExpr expr <+> "{" <+> printInnerAttributes attrs <+> hsep (printArm <$> arms) <+> "}"
    Closure _ captureBy decl@FnDecl{ output = output } body _ -> when (captureBy == Value) "move" 
      <+> printFnBlockArgs decl
      <+> case (stmts body, output) of
            ([NoSemi iExpr _], Nothing) -> case iExpr of
                                              BlockExpr attrs blk _ -> printBlockUnclosedWithAttrs blk attrs
                                              _ -> printExpr iExpr
            _ -> printBlockUnclosed body
    BlockExpr attrs blk _ -> printBlockWithAttrs blk attrs
    Assign _ lhs rhs _ -> printExpr lhs <+> "=" <+> printExpr rhs
    AssignOp _ op lhs rhs _ -> printExpr lhs <+> printBinOp op <> "=" <+> printExpr rhs
    FieldAccess _ expr ident _ -> printExpr expr <> "." <> printIdent ident
    TupField _ expr num _ -> printExpr expr <> "." <> text (show num)
    Index _ expr index _ -> printExpr expr <> "[" <> printExpr index <> "]"
    Range _ start end limits _ -> perhaps printExpr start <> printRangeLimits limits <> perhaps printExpr end
    PathExpr _ Nothing path _ -> printPath path True
    PathExpr _ (Just qself) path _ -> printQPath path qself True
    AddrOf _ mut expr _ -> "&" <> printMutability mut <+> printExprMaybeParen expr
    Break _ brk _ -> "break" <+> perhaps printIdent brk
    Continue _ cont _ -> "continue" <+> perhaps printIdent cont
    Ret _ result _ -> "return" <+> perhaps printExpr result
    InlineAsmExpr _ inlineAsm _ -> error "Unimplemented"
    MacExpr _ m _ -> error "Unimplemented"
    Struct attrs path fields wth _ -> printPath path True <+> "{"
      <+> printInnerAttributes attrs
      <+> sep (printField <$> fields)
      <+> perhaps (\e -> ".." <> printExpr e) wth
      <+> "}"
    Repeat attrs element count _ -> "[" <> printInnerAttributes attrs <+> printExpr element <> ";" <+> printExpr count  <> "]"
    ParenExpr attrs expr _ -> "(" <> printInnerAttributes attrs <+> printExpr expr <> ")"
    Try _ expr _ -> printExpr expr <> "?"

expressionAttrs :: Expr a -> [Attribute a]
expressionAttrs (Box as _ _) = as
expressionAttrs (InPlace as _  _ _) = as
expressionAttrs (Vec as _ _) = as
expressionAttrs (Call as _  _ _) = as
expressionAttrs (MethodCall as _  _ _ _) = as
expressionAttrs (TupExpr as _ _) = as
expressionAttrs (Binary as _ _ _ _) = as
expressionAttrs (Unary as _ _ _) = as
expressionAttrs (Lit as _ _) = as
expressionAttrs (Cast as _ _ _) = as
expressionAttrs (TypeAscription as _  _ _) = as
expressionAttrs (If as _ _ _ _) = as
expressionAttrs (IfLet as _ _ _ _ _) = as
expressionAttrs (While as _  _ _ _) = as
expressionAttrs (WhileLet as _ _ _ _ _) = as
expressionAttrs (ForLoop as _ _ _ _ _) = as
expressionAttrs (Loop as _ _ _) = as
expressionAttrs (Match as _ _ _) = as
expressionAttrs (Closure as _ _ _ _) = as
expressionAttrs (BlockExpr as _ _) = as
expressionAttrs (Assign as _ _ _) = as
expressionAttrs (AssignOp as _ _ _ _) = as
expressionAttrs (FieldAccess as _ _ _) = as
expressionAttrs (TupField as _ _ _) = as
expressionAttrs (Index as _ _ _) = as
expressionAttrs (Range as _ _ _ _) = as
expressionAttrs (PathExpr as _ _ _) = as
expressionAttrs (AddrOf as _ _ _) = as
expressionAttrs (Break as _ _) = as
expressionAttrs (Continue as _ _) = as
expressionAttrs (Ret as _ _) = as
expressionAttrs (InlineAsmExpr as _ _) = as
expressionAttrs (MacExpr as _ _) = as
expressionAttrs (Struct as _ _ _ _) = as
expressionAttrs (Repeat as _ _ _) = as
expressionAttrs (ParenExpr as _ _) = as
expressionAttrs (Try as _ _) = as


printField :: Field a -> Doc
printField Field{..} = printIdent ident <> ":" <+> printExpr expr <> ","

printRangeLimits :: RangeLimits -> Doc
printRangeLimits HalfOpen = ".."
printRangeLimits Closed = "..."

-- print_fn_block_args
printFnBlockArgs :: FnDecl a -> Doc
printFnBlockArgs FnDecl{..} = "|" <> commas inputs (\a -> printArg a True) <> "|" <+> perhaps (\ty -> "->" <+> printType ty) output

-- print_arm
printArm :: Arm a -> Doc
printArm Arm{..} = printOuterAttributes attrs
  <+> foldr1 (\x y -> x <+> "|" <+> y) (printPat <$> pats)
  <+> perhaps (\e -> "if" <+> printExpr e) guard
  <+> "=>"
  <+> case body of 
        BlockExpr _ blk _ -> printBlockUnclosed blk <> when (rules blk == UnsafeBlock False) ","
        _ -> printExpr body <> ","

printBlock :: Block a -> Doc
printBlock blk = printBlockWithAttrs blk []

-- print_block_unclosed/print_block_unclosed_indent
printBlockUnclosed :: Block a -> Doc
printBlockUnclosed = printBlock -- TODO maybe?

-- print_block_unclosed_with_attrs
printBlockUnclosedWithAttrs :: Block a -> [Attribute a] -> Doc
printBlockUnclosedWithAttrs = printBlockWithAttrs -- TODO maybe?

-- aka print_block_with_attrs
-- Actually print_block_maybe_unclosed
printBlockWithAttrs :: Block a -> [Attribute a] -> Doc
printBlockWithAttrs Block{..} attrs = 
    safety <+> "{" <+> printInnerAttributes attrs <+> body <+> lastStmt <+> "}"
  where
  body :: Doc
  body = if null stmts then empty else hsep (printStmt <$> Prelude.init stmts)
  
  lastStmt :: Doc
  lastStmt = if null stmts
    then empty
    else case last stmts of
            NoSemi expr _ -> printExprOuterAttrStyle expr False
            stmt -> printStmt stmt

  safety :: Doc
  safety = case rules of
              DefaultBlock -> empty
              UnsafeBlock _ -> "unsafe"

-- print_else
printElse :: Maybe (Expr a) -> Doc
printElse Nothing = empty
printElse (Just (If _ test thn els _)) = "else if" <+> printExpr test <+> printBlock thn <+> printElse els
printElse (Just (IfLet _ pat expr thn els _)) = "else if let" <+> printPat pat <+> "=" <+> printExpr expr <+> printBlock thn <+> printElse els
printElse (Just (BlockExpr _ blk _)) = "else" <+> printBlock blk
printElse _ = error "printElse saw `if` with a weird alternative"

-- no similar
printBinOp :: BinOp -> Doc
printBinOp AddOp = "+"
printBinOp SubOp = "-"
printBinOp MulOp = "*"
printBinOp DivOp = "/"
printBinOp RemOp = "%"
printBinOp AndOp = "&&"
printBinOp OrOp = "||"
printBinOp BitXorOp = "^"
printBinOp BitAndOp = "&"
printBinOp BitOrOp = "|"
printBinOp ShlOp = "<<"
printBinOp ShrOp = ">>"
printBinOp EqOp = "=="
printBinOp LtOp = "<"
printBinOp LeOp = "<="
printBinOp NeOp = "!="
printBinOp GeOp = ">="
printBinOp GtOp = ">"

-- aka util::parser::AssocOp::precedence
opPrecedence :: BinOp -> Int
opPrecedence AddOp = 12
opPrecedence SubOp = 12
opPrecedence MulOp = 13
opPrecedence DivOp = 13
opPrecedence RemOp = 13
opPrecedence AndOp = 6
opPrecedence OrOp = 5
opPrecedence BitXorOp = 9
opPrecedence BitAndOp = 10
opPrecedence BitOrOp = 8
opPrecedence ShlOp = 11
opPrecedence ShrOp = 11
opPrecedence EqOp = 7
opPrecedence LtOp = 7
opPrecedence LeOp = 7
opPrecedence NeOp = 7
opPrecedence GeOp = 7
opPrecedence GtOp = 7

-- no similar
printUnOp :: UnOp -> Doc
printUnOp Deref = "*"
printUnOp Not = "!"
printUnOp Neg = "~"

-- aka print_literal
-- TODO this is broken with respect to escaping characters
printLiteral :: Lit a -> Doc
printLiteral (Str str _ _ _) = "\"" <> text str <> "\""
{-printLiteral (Char c _) = "'" <> text [c] <> "'"
printLiteral (Int i _) = text (show i)
printLiteral (Float str F32 _) = text str <> "f32"
printLiteral (Float str F64 _) = text str <> "f64"
printLiteral (FloatUnsuffixed str _) = text str
printLiteral (Bool True _) = "true"
printLiteral (Bool False _) = "false"
printLiteral (ByteStr w8s _) = error "Unimplemented"
printLiteral (Byte w8 _) = error "Unimplemented"-}

-- similar to check_expr_bin_needs_paren
checkExprBinNeedsParen :: Expr a -> BinOp -> Doc
checkExprBinNeedsParen e@(Binary _ op' _ _ _) op | opPrecedence op' < opPrecedence op = "(" <> printExpr e <> ")"
checkExprBinNeedsParen e _ = printExpr e

-- aka print_expr_maybe_paren
printExprMaybeParen :: Expr a -> Doc
printExprMaybeParen expr = when needs "(" <> printExpr expr <> when needs ")"
  where needs = needsParentheses expr 

-- aka needs_parentheses
needsParentheses :: Expr a -> Bool
needsParentheses Assign{} = True
needsParentheses Binary{} = True
needsParentheses Closure{} = True
needsParentheses AssignOp{} = True
needsParentheses Cast{} = True
needsParentheses InPlace{} = True
needsParentheses TypeAscription{} = True
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
printAttribute a@Attribute{..} inline | isSugaredDoc && inline = "/*!" <+> perhaps text (valueStr a) <+> "*/"
                                      | isSugaredDoc = "//!" <+> perhaps text (valueStr a) 
                                      | style == Inner = "#![" <> printMetaItem value <> "]"
                                      | style == Outer = "#[" <> printMetaItem value <> "]"

valueStr :: Attribute a -> Maybe String
valueStr (Attribute _ (NameValue _  (Str s _ _ _) _) _ _) = Just s
valueStr _ = Nothing

-- aka  print_meta_list_item
printMetaListItem :: NestedMetaItem a -> Doc
printMetaListItem (MetaItem item _) = printMetaItem item
printMetaListItem (Literal lit _) = printLiteral lit

-- aka  print_meta_item
printMetaItem :: MetaItem a -> Doc
printMetaItem (Word name _) = printIdent name
printMetaItem (NameValue name lit _) = printIdent name <+> "=" <+> printLiteral lit
printMetaItem (List name items _) = printIdent name <> "(" <> commas items printMetaListItem <> ")"

-- | Synthesizes a comment that was not textually present in the original source file.
-- aka  synth_comment
synthComment :: String -> Doc
synthComment com = "/*" <+> text com <+> "*/"

-- aka print_item
printItem :: Item a -> Doc
printItem Item{..} = case node of
  ExternCrate optionalPath -> printVisibility vis <+> "extern" <+> "crate" 
  Use vp -> printVisibility vis <+> "use" <+> printViewPath vp <> ";"
  Static ty m expr -> printVisibility vis <+> "static" <+> when (m == Mutable) "mut" <+> printIdent ident <> ":" <+> printType ty <+> "=" <+> printExpr expr <> ";"
  ConstItem ty expr -> printVisibility vis <+> "const" <+> printIdent ident <> ":" <+> printType ty <+> "=" <+> printExpr expr <> ";"
  Fn decl unsafety constness abi tyParams body -> printFn decl unsafety constness abi (Just ident) tyParams vis <+> printBlockWithAttrs body attrs
  Mod items -> printVisibility vis <+> "mod" <+> printIdent ident <> "{" <> printMod items attrs <> "}"
  ForeignMod abi foreignItems -> "extern" <+> printAbi abi <> "{" <> printForeignMod foreignItems attrs <> "}"
  TyAlias ty params -> printVisibility vis <+> "type" <+> printIdent ident <> printGenerics params <> printWhereClause (whereClause params) <+> "=" <+> printType ty <> ";"
  Enum variants params -> printEnumDef variants params ident vis
  StructItem structDef generics -> printVisibility vis <+> "struct" <+> printStruct structDef generics ident True
  Union structDef generics -> printVisibility vis <+> "union" <+> printStruct structDef generics ident True
  DefaultImpl unsafety traitRef -> printVisibility vis <+> printUnsafety unsafety <+> "impl" <> printTraitRef traitRef <+> "for" <+> ".." <+> "{ }"
  Impl unsafety polarity generics traitRef_m ty implItems ->
      printVisibility vis <+> printUnsafety unsafety <+> "impl"
        <+> (case generics of { Generics [] [] _ _ -> empty; _ -> printGenerics generics })
        <+> perhaps (\t -> printTraitRef t <+> "for") traitRef_m
        <+> printType ty
        <+> printWhereClause (whereClause generics)
        <+> "{" <+> printInnerAttributes attrs <+> hsep (printImplItem <$> implItems) <+> "}"
  Trait unsafety generics typarambounds traitItems ->
      let seperateBounds :: [TyParamBound a] -> ([Doc], [TyParamBound a])
          seperateBounds [] = ([],[])
          seperateBounds (t:ts) = case t of
                                    TraitTyParamBound ptr Maybe -> (("for ?" <+> printTraitRef (traitRef ptr)) : ds, tybs)
                                    _ -> (ds, t:tybs)
            where ~(ds,tybs) = seperateBounds ts

          (bounds,realBounds) = seperateBounds typarambounds
      in printVisibility vis <+> printUnsafety unsafety <+> "trait" <+> printIdent ident
            <+> printGenerics generics
            <+> hsep bounds
            <+> printBounds ":" realBounds
            <+> printWhereClause (whereClause generics)
            <+> "{" <+> hsep (printTraitItem <$> traitItems) <+> "}"
  MacItem m -> error "Unimplemented (pprust.rs line 1333)"


-- aka print_trait_item
printTraitItem :: TraitItem a -> Doc
printTraitItem TraitItem{..} = printOuterAttributes attrs <+>
  case node of
    ConstT ty default_m -> printAssociatedConst ident ty default_m InheritedV
    MethodT sig block_m -> printMethodSig ident sig InheritedV <+> maybe ";" (\body -> printBlockWithAttrs body attrs) block_m
    TypeT bounds default_m -> printAssociatedType ident (Just bounds) default_m
    MacroT m -> error "Unimplemented (pprust.rs line 1572)"


-- aka print_bounds
printBounds :: Doc -> [TyParamBound a] -> Doc
printBounds _ [] = empty
printBounds prefix bounds = prefix <+> foldr1 (\x y -> x <+> "+" <+> y) (printBound <$> bounds)
  where
  printBound :: TyParamBound a -> Doc
  printBound (RegionTyParamBound lt) = printLifetime lt
  printBound (TraitTyParamBound PolyTraitRef{..} modi) =
    case modi of { Maybe -> "?"; _ -> empty }
      <> printFormalLifetimeList boundLifetimes
      <+> printTraitRef traitRef

-- aka print_formal_lifetime_list
printFormalLifetimeList :: [LifetimeDef a] -> Doc
printFormalLifetimeList [] = empty
printFormalLifetimeList lifetimes = "for<" <> commas lifetimes (\LifetimeDef{..} -> printOuterAttributes attrs <+> printLifetimeBounds lifetime bounds) <> ">"

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
printAssociatedType :: Ident ->  Maybe [TyParamBound a] -> Maybe (Ty a) -> Doc
printAssociatedType ident bounds_m ty_m = "type" <+> printIdent ident
  <+> perhaps (printBounds ":") bounds_m
  <+> perhaps (\ty -> "=" <+> printType ty) ty_m
  <> ";"

-- aka print_method_sig
printMethodSig :: Ident -> MethodSig a -> Visibility a -> Doc
printMethodSig ident MethodSig{..} vis = printFn decl unsafety constness abi (Just ident) generics vis 

-- aka print_associated_const
printAssociatedConst :: Ident -> Ty a -> Maybe (Expr a) -> Visibility a -> Doc
printAssociatedConst ident ty default_m vis = printVisibility vis
  <+> "const" <+> printIdent ident <> ":" <+> printType ty
  <+> perhaps (\expr -> "=" <+> printExpr expr) default_m
  <> ";"

-- no aka
printPolarity :: ImplPolarity -> Doc
printPolarity Negative = "!"
printPolarity Positive = empty

-- aka print_visibility
printVisibility :: Visibility a -> Doc
printVisibility PublicV = "pub"
printVisibility CrateV = "pub(crate)"
printVisibility (RestrictedV path) = "pub(" <> printPath path False <> ")"
printVisibility InheritedV = empty

-- aka print_foreign_item
printForeignItem :: ForeignItem a -> Doc
printForeignItem ForeignItem{..} = printOuterAttributes attrs <+>
  case node of
    ForeignFn decl generics -> printFn decl Normal NotConst Rust (Just ident) generics vis <> ";"
    ForeignStatic ty mut -> printVisibility vis <+> "static" <+> when mut "mut" <+> printIdent ident <> ":" <+> printType ty <> ";"


-- aka print_struct
printStruct :: VariantData a -> Generics a -> Ident -> Bool -> Doc
printStruct structDef generics@Generics{..} ident printFinalizer =
  printIdent ident <+> printGenerics generics
    <+> case structDef of 
          StructD fields _ -> printWhereClause whereClause <+> "{" <+> commas fields printStructField <+> "}"
          TupleD fields _ -> "(" <> commas fields printStructField <> ")" <+> printWhereClause whereClause <+> when printFinalizer ";" 
          UnitD _ -> "()" <+> printWhereClause whereClause <+> when printFinalizer ";"


printStructField :: StructField a -> Doc
printStructField StructField{..} =
  printOuterAttributes attrs
    <+> printVisibility vis
    <+> perhaps (\i -> printIdent i <> ":") ident <+> printType ty


-- aka print_unsafety
printUnsafety :: Unsafety -> Doc
printUnsafety Normal = empty
printUnsafety Unsafe = "unsafe"

-- aka print_enum_def
printEnumDef :: [Variant a] -> Generics a -> Ident -> Visibility a -> Doc
printEnumDef variants generics ident vis =
  printVisibility vis <+> "enum" <+> printIdent ident <+> printGenerics generics
    <+> printWhereClause (whereClause generics) <+> "{"
    <+> sep ((\v@Variant{..} -> printOuterAttributes attrs <+> printVariant v <> ",") <$> variants)
    <+> "}"

-- print_variant
printVariant :: Variant a -> Doc
printVariant Variant{..} = 
  perhaps (\e -> "=" <+> printExpr e) disrExpr

-- aka print_where_clause
printWhereClause :: WhereClause a -> Doc
printWhereClause WhereClause{..}
  | null predicates = empty
  | otherwise = "where" <+> commas predicates printWherePredicate
  where
  printWherePredicate :: WherePredicate a -> Doc
  printWherePredicate BoundPredicate{..} = printFormalLifetimeList boundLifetimes <+> printType boundedTy <> printBounds ":" traitLifetimeBounds 
  printWherePredicate RegionPredicate{..} = printLifetimeBounds lifetime lifetimeBounds
  printWherePredicate EqPredicate{..} = printPath path False <+> "=" <+> printType ty

-- aka  print_fn
printFn :: FnDecl a -> Unsafety -> Constness -> Abi -> Maybe Ident -> Generics a -> Visibility a -> Doc
printFn decl unsafety constness abi name generics vis =
  printFnHeaderInfo unsafety constness abi vis
    <+> perhaps printIdent name
    <> printGenerics generics
    <> printFnArgsAndRet decl
    <+> printWhereClause (whereClause generics)

-- aka print_fn_args_and_ret
printFnArgsAndRet :: FnDecl a -> Doc
printFnArgsAndRet FnDecl{..} = "(" <> commas inputs (\arg -> printArg arg False) <> when variadic ", ..." <> ")" 

-- aka print_arg TODO double check this
printArg :: Arg a -> Bool -> Doc
printArg Arg{..} isClosure = case ty of
  Infer _ | isClosure -> printPat pat
  _ -> let (IdentP _ ident _ _) = pat
           invalid = ident == mkIdent ""
       in when (not invalid) (printPat pat <> ":") <+> printType ty

-- print_explicit_self
printExplicitSelf :: SelfKind a -> Doc
printExplicitSelf (ValueSelf mut) = printMutability mut <+> "self"
printExplicitSelf (Region lifetime_m mut) = "&" <> perhaps printLifetime lifetime_m <+> printMutability mut <+> "self"
printExplicitSelf (Explicit ty mut) = printMutability mut <+> "self" <> ":" <+> printType ty

-- aka print_lifetime
printLifetime :: Lifetime a -> Doc
printLifetime Lifetime{..} = "'" <> printName name

-- print_mutability
printMutability :: Mutability -> Doc
printMutability Mutable = "mut"
printMutability Immutable = empty

printFullMutability :: Mutability -> Doc
printFullMutability Mutable = "mut"
printFullMutability Immutable = "const"

-- print_pat
printPat :: Pat a -> Doc
printPat (WildP _) = "_"
printPat (IdentP bindingMode path1 sub _) = printBindingMode bindingMode <+> printIdent path1 <+> perhaps (\p -> "@" <> printPat p) sub
printPat (StructP path fieldPats b _) = printPath path True <+> "{" <+> commas fieldPats (\FieldPat{..} -> when (not isShorthand) (printIdent ident <> ":") <+> printPat pat) <+> when b ".." <+> "}"
printPat (TupleStructP path elts Nothing _) = printPath path True <> "(" <> commas elts printPat <> ")"
printPat (TupleStructP path elts (Just ddpos) _) = let (before,after) = splitAt ddpos elts
  in printPath path True <> "(" <> commas before printPat <> when (notNull elts) ","
      <+> ".." <> when (ddpos /= length elts) ("," <+> commas after printPat) <> ")"
printPat (PathP Nothing path _) = printPath path True
printPat (PathP (Just qself) path _) = printQPath path qself False
printPat (TupleP elts Nothing _) = "(" <> commas elts printPat <> ")"
printPat (TupleP elts (Just ddpos) _) = let (before,after) = splitAt ddpos elts
  in "(" <> commas before printPat <> when (notNull elts) ","
      <+> ".." <> when (ddpos /= length elts) ("," <+> commas after printPat) <> ")"
printPat (BoxP inner _) = "box" <+> printPat inner
printPat (RefP inner mutbl _) = "&" <> printMutability mutbl <+> printPat inner
printPat (LitP expr _) = printExpr expr
printPat (RangeP lo hi _) = printExpr lo <+> "..." <+> printExpr hi
printPat (SliceP before slice_m after _) = "[" <> commas before printPat
printPat (MacP _ _) = error "Unimplemented"


printBindingMode :: BindingMode -> Doc
printBindingMode (ByRef mutbl) = "ref" <+> printMutability mutbl
printBindingMode (ByValue Immutable) = empty
printBindingMode (ByValue Mutable) = "mut"


-- aka  print_fn_header_info
printFnHeaderInfo :: Unsafety -> Constness -> Abi -> Visibility a -> Doc
printFnHeaderInfo unsafety constness abi vis =
  printVisibility vis
  <+> (case constness of { Const -> "const"; _ -> empty })
  <+> printUnsafety unsafety
  <+> printAbi abi
  <+> "fn"

printAbi :: Abi -> Doc
printAbi Rust = empty
printAbi abi = "extern" <+> raw abi
  where
  raw Cdecl = "\"cdecl\""
  raw Stdcall = "\"stdcall\""
  raw Fastcall = "\"fastcall\""
  raw Vectorcall = "\"\""
  raw Aapcs = "\"aapcs\""
  raw Win64 = "\"win64\""
  raw SysV64 = "\"sysv64\""
  raw Rust = "\"Rust\""
  raw C = "\"C\""
  raw System = "\"system\""
  raw RustIntrinsic = "\"rust-intrinsic\""
  raw RustCall = "\"rust-call\""
  raw PlatformIntrinsic = "\"platform-intrinsic\""


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
  | otherwise =  let lifetimes' = [ printOuterAttributes attrs <+> printLifetimeBounds lifetime bounds | LifetimeDef attrs lifetime bounds _ <-lifetimes ]
                     bounds' = [ printTyParam param | param<-tyParams ]
                 in "<" <> hsep (punctuate "," (lifetimes' ++ bounds')) <> ">"


-- aka  print_poly_trait_ref
printPolyTraitRef :: PolyTraitRef a -> Doc
printPolyTraitRef PolyTraitRef{..} = printFormalLifetimeList boundLifetimes <+> printTraitRef traitRef

-- aka  print_trait_ref
printTraitRef :: TraitRef a -> Doc
printTraitRef TraitRef{..} = printPath path False

-- aka print_path_parameters
printPathParameters :: PathParameters a -> Bool -> Doc
printPathParameters (AngleBracketed [] [] [] _) _ = empty
printPathParameters Parenthesized{..} colons = when colons "::" <> "(" <> commas inputs printType <> ")" <+> perhaps (\t -> "->" <+> printType t) output
printPathParameters AngleBracketed{..} colons = when colons "::" <> "<" <> hsep (punctuate "," (lifetimes' ++ types' ++ bindings')) <> ">"
  where
    lifetimes' = printLifetime <$> lifetimes
    types' = printType <$> types
    bindings' = (\(ident,ty) -> printIdent ident <+> "=" <+> printType ty) <$> bindings

-- | second argument says whether to put colons before params
-- aka print_path
printPath :: Path a -> Bool -> Doc
printPath Path{..} colons = when global "::" <> hcat (printSegment <$> segments)
  where
  printSegment :: (Ident, PathParameters a) -> Doc
  printSegment (ident,parameters) = printIdent ident <> printPathParameters parameters colons

-- print_qpath
printQPath :: Path a -> QSelf a -> Bool -> Doc
printQPath Path{..} QSelf{..} colons =  
  "<" <> printType ty <+> when (position > 0) ("as" <+> printPath (Path global (take position segments) nodeInfo) False) 
      <> ">" <> "::" <> printIdent ident <> printPathParameters parameters colons
  where
  (ident, parameters) = last segments 

-- aka print_view_path
printViewPath :: ViewPath a -> Doc
printViewPath (ViewPathSimple ident path _) = printPath path False <+> when (fst (last (segments path)) /= ident) ("as" <+> printIdent ident)
printViewPath (ViewPathGlob path _) = printPath path False <> "::*"
printViewPath (ViewPathList path idents _) = prefix <> "::{" <> commas idents printPathListItem <> "}"
  where
  prefix = if (null (segments path)) then "{" else printPath path False

  printPathListItem :: PathListItem a -> Doc
  printPathListItem (PathListItem name (Just rename) _) = printIdent name <+> "as" <+> printIdent rename
  printPathListItem (PathListItem name Nothing _) = printIdent name


-- aka print_ty_param
printTyParam :: TyParam a -> Doc
printTyParam TyParam{..} = printOuterAttributes attrs
  <+> printIdent ident <> printBounds ":" bounds
  <+> perhaps (\d -> "=" <+> printType d) default_

printName :: Name -> Doc
printName (Name s) = text s

-- aka print_lifetime_bounds
printLifetimeBounds :: Lifetime a -> [Lifetime a] -> Doc
printLifetimeBounds lifetime bounds = printLifetime lifetime
  <> when (notNull bounds) (":" <+> foldr1 (\x y -> x <+> "+" <+> y) (printLifetime <$> bounds))
