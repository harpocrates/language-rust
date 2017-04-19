{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module Diff where

import Data.Aeson
import Language.Rust.Pretty
import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Ident
import Control.Monad (when)
import Data.String (fromString)
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Exception (throw)
import Data.Foldable (sequence_, toList)
import Data.List.NonEmpty ((<|))
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

import DiffUtils


-- | Lift a comparision to an array
liftDiff :: (Foldable f, Show a) => (a -> Value -> Diff) -> (f a -> Value -> Diff)
liftDiff f xs val@(Data.Aeson.Array v) = do
  let xs' = toList v
  when (length xs /= length xs') $
    diff "arrays have different lengths" (toList xs) val
  sequence_ [ f x x' | (x,x') <- toList xs `zip` xs' ]
liftDiff _ xs v = diff "comparing array to non-array" (toList xs) v

-- | Lift a comparision to accept 'null' as 'Nothing'
maybeDiff :: Show a => (a -> Value -> Diff) -> (Maybe a -> Value -> Diff)
maybeDiff f (Just x) val = f x val
maybeDiff f Nothing Data.Aeson.Null = pure ()
maybeDiff f n@Nothing val = diff "expected the JSON to be null" n val

-- | Report a difference
diff :: Show a => String -> a -> Value -> Diff
diff explanation v j = throw (DiffError msg)
  where msg = unlines [ explanation ++ " in"
                      , " * parsed AST"
                      , show (show v)
                      , " * dumped JSON"
                      , unpack (encode j)
                      ]

diffSourceFile :: Show a => SourceFile a -> Value -> Diff
diffSourceFile s@(SourceFile _ as items) val = do
  -- Check attributes
  liftDiff diffAttribute as (val ! "attrs")
 
  -- Check items
  liftDiff diffItem items (val ! "module" ! "items")

diffItem :: Show a => Item a -> Value -> Diff
diffItem item@(Item i as n v _) val = do
  -- Check name of item
  when (fromString (show i) /= val ! "ident") $
    diff "item has different name" item val
 
  -- Check attributes
  liftDiff (\a v -> diffAttribute a (v ! "node")) as (val ! "attrs")
  
  -- Check node
  let n' = val ! "node"
  case (n' ! "variant", n) of
    ("Fn", Fn decl u c a gen bod) -> do
      diffFnDecl    decl (n' ! "fields" ! 0)
      diffUnsafety  u    (n' ! "fields" ! 1)
      diffConstness c    (n' ! "fields" ! 2)
      diffAbi       a    (n' ! "fields" ! 3)
      diffGenerics  gen  (n' ! "fields" ! 4)
      diffBlock     bod  (n' ! "fields" ! 5)
    ("ExternCrate", ExternCrate Nothing) -> pure ()
    ("ExternCrate", ExternCrate (Just (Ident i _))) -> 
      when (n' ! "fields" ! 0 /= String (fromString i)) $
        diff "different crate import" item val
    ("Use", Use v) ->
      diffViewPath   v (n' ! "fields" ! 0)
    ("Static", Static t m e) -> do
      diffTy         t (n' ! "fields" ! 0)
      diffMutability m (n' ! "fields" ! 1)
      diffExpr       e (n' ! "fields" ! 2)
    ("Const", ConstItem t e) -> do
      diffTy         t (n' ! "fields" ! 0)
      diffExpr       e (n' ! "fields" ! 1)
    ("Mod", Mod is) ->
      liftDiff diffItem is (n' ! "fields" ! 0 ! "items")
    ("ForeignMod", ForeignMod a is) -> do
      diffAbi a (n' ! "fields" ! 0 ! "abi")
      liftDiff diffForeignItem is (n' ! "fields" ! 0 ! "items")
    ("Ty", TyAlias t g) -> do
      diffTy t (n' ! "fields" ! 0)
      diffGenerics g (n' ! "fields" ! 1)
    ("Enum", Enum vs g) -> do
      liftDiff diffVariant vs (n' ! "fields" ! 0 ! "variants") 
      diffGenerics g (n' ! "fields" ! 1)

    _ -> diff "Unimplemented: diffItem" item val

  -- Check visibility
  diffVisibility v (val ! "vis")

diffVariant :: Show a => Variant a -> Value -> Diff
diffVariant v@(Variant (Ident i _) as d e _) val = do
  let n = val ! "node"
  maybeDiff diffExpr e (n ! "disr_expr")
  diffVariantData    d (n ! "data")
  when (fromString i /= val ! "node" ! "name") $
    diff "variant has different name" v val

diffVariantData :: Show a => VariantData a -> Value -> Diff
diffVariantData d val =
  case (val ! "variant", d) of
    ("Tuple", TupleD sf _) -> liftDiff diffStructField sf (val ! "fields" ! 0) 
    ("Struct", StructD sf _) -> liftDiff diffStructField sf (val ! "fields" ! 0) 
    _ -> diff "Unimplemented: diffVariantData" d val

diffStructField :: Show a => StructField a -> Value -> Diff
diffStructField s@(StructField i v t as _) val = do

  -- TODO
  diffVisibility v (val ! "vis")
  diffTy         t (val ! "ty")

diffForeignItem :: Show a => ForeignItem a -> Value -> Diff
diffForeignItem f v = diff "Unimplemented: diffForeignItem" f v

diffViewPath :: Show a => ViewPath a -> Value -> Diff
diffViewPath v val = do
  let n' = val ! "node"
  case (n' ! "variant", v) of
    _ -> diff "Unimplemented: diffViewPath" v val

diffFnDecl :: Show a => FnDecl a -> Value -> Diff
diffFnDecl decl@(FnDecl as out v _) val = do
  -- Check inputs
  let as' = val ! "inputs"
  liftDiff diffArg as as'

  -- Check output
  let outTy = val ! "output" ! "variant"
  case (outTy, out) of
    ("Default", Nothing) -> pure ()
    ("Ty", Just t) -> diffTy t (val ! "output" ! "fields" ! 0)
    _ -> diff "different output types" out outTy

  -- Check variadic
  let v' = val ! "variadic"
  when (Data.Aeson.Bool v /= v') $
    diff "different variadicity" decl val


diffArg :: Show a => Arg a -> Value -> Diff
diffArg a val = diff "Unimplemented: diffArg" a val

diffUnsafety :: Unsafety -> Value -> Diff
diffUnsafety Unsafe val | val ! "variant" == "Unsafe" = pure ()
diffUnsafety Normal "Normal" = pure ()
diffUnsafety Normal "Default" = pure ()
diffUnsafety unsafety val = diff "different safety" unsafety val

diffConstness :: Constness -> Value -> Diff
diffConstness c val = case (val ! "node", c) of
                        ("Const", Const) -> pure ()
                        ("NotConst", NotConst) -> pure ()
                        _ -> error "different constness" -- diff "different constness" c val

diffAbi :: Abi -> Value -> Diff
diffAbi Rust "Rust" = pure ()
diffAbi C    "C"    = pure ()
diffAbi a v = diff "Unimplemented: diffAbi" a v


diffGenerics :: Show a => Generics a -> Value -> Diff
diffGenerics gen@(Generics lts tys whr _) val = do
  -- Check lifetimes
  liftDiff diffLifetimeDef lts (val ! "lifetimes")

  -- Check type parameters
  liftDiff diffTyParam tys (val ! "ty_params")

  -- Check where clause
  diffWhereClause whr (val ! "where_clause")


diffLifetimeDef :: LifetimeDef a -> Value -> Diff
diffLifetimeDef _ _ = error "Unimplemented: diffLifetimeDef"

diffTyParam :: Show a => TyParam a -> Value -> Diff
diffTyParam t val = diff "Unimplemented: diffTyParam" t val

diffWhereClause :: Show a => WhereClause a -> Value -> Diff
diffWhereClause whr@(WhereClause preds _) val =
  -- Check predicates
  liftDiff diffWherePredicate preds (val ! "predicates")


diffWherePredicate :: WherePredicate a -> Value -> Diff
diffWherePredicate _ _ = error "Unimplemented: diffWherePredicate"

diffBlock :: Show a => Block a -> Value -> Diff
diffBlock blk@(Block ss ru _) val = do
  -- Check statements
  liftDiff diffStmt ss (val ! "stmts")

  -- Check block mode
  diffUnsafety ru (val ! "rules")

diffStmt :: Show a => Stmt a -> Value -> Diff
diffStmt stmt val = do
  let n' = val ! "node"
  case (n' ! "variant", stmt) of
    ("Local", Local p t i as _) -> do
      diffPat            p (n' ! "fields" ! 0 ! "pat")
      maybeDiff diffTy   t (n' ! "fields" ! 0 ! "ty")
      maybeDiff diffExpr i (n' ! "fields" ! 0 ! "init")
    ("Expr", NoSemi e _)   -> diffExpr e (n' ! "fields" ! 0) 
    ("Semi", Semi e _)     -> diffExpr e (n' ! "fields" ! 0)
    ("Item", ItemStmt i _) -> diffItem i (n' ! "fields" ! 0)
    _ -> diff "Unimplemented: diffStmt" stmt val

diffVisibility :: Show a => Visibility a -> Value -> Diff
diffVisibiilty PublicV "Public" = pure ()
diffVisibility CrateV "Crate" = pure ()
diffVisibility (RestrictedV p) _ = error "todo"
diffVisibility InheritedV "Inherited" = pure ()
diffVisibility v val = diff "different visibilities" v val

diffAttribute :: Show a => Attribute a -> Value -> Diff
diffAttribute a@(Attribute sty meta sug _) val = do
  -- Check style
  case (val ! "style", sty) of
    ("Inner", Inner) -> pure ()
    ("Outer", Outer) -> pure ()
    _ -> diff "attribute style is different" a val

  -- Check meta item
  let meta' = val ! "value"
--  diffMetaItem meta meta'

  -- Check sugared doc
  when (val ! "is_sugared_doc" /= Data.Aeson.Bool sug) $
    diff "attribute is supposed to be sugared doc" a val

diffMetaItem :: Show a => MetaItem a -> Value -> Diff
diffMetaItem meta val = do
  -- Check that the names match
  when (fromString (name meta) /= val ! "name") $
    diff "meta item has different name" meta val

  case (val ! "node", meta)  of
    ("Word", Word{}) -> pure()

    (val',          _) ->
      case (val' ! "variant", meta) of
        ("NameValue", NameValue _ lit _) -> do
          -- Check value
          let lit' = val' ! "fields" ! 0
          diffLit lit lit' 
    
        ("List",      List _ args _) ->
          -- Check arguments in list
          liftDiff diffNestedMetaItem args (val' ! "fields" ! 0)
  where 
    name :: MetaItem a -> String
    name (Word (Ident i _) _) = i
    name (List (Ident i _) _ _) = i
    name (NameValue (Ident i _) _ _) = i

diffNestedMetaItem :: Show a => NestedMetaItem a -> Value -> Diff
diffNestedMetaItem n val =  do
  let val' = val ! "node"
  case (val' ! "variant", n) of
    ("Literal", Literal l _) -> diffLit l (val' ! "fields" ! 0)
    ("MetaItem", MetaItem a _) -> diffMetaItem a (val' ! "fields" ! 0)
    _ -> diff "differing nested-metaitem" n val

diffPat :: Show a => Pat a -> Value -> Diff
diffPat p val = do
  let val' = val ! "node"
  case (val', p) of
    ("Wild", WildP _) -> pure ()
    (Data.Aeson.Object{}, _) ->
      case (val' ! "variant", p) of
        ("Box", BoxP p _) -> diffPat p (val' ! "fields" ! 0)
        ("Lit", LitP e _) -> diffExpr e (val' ! "fields" ! 0)
        ("Mac", MacP m _) -> diffMac  m (val' ! "fields" ! 0)
        ("Ident", IdentP bm i m _) -> do
          diffBindingMode   bm (val' ! "fields" ! 0)
          diffIdent         i  (val' ! "fields" ! 1)
          maybeDiff diffPat m  (val' ! "fields" ! 2)
        ("Ref", RefP p m _) -> do
          diffPat        p (val' ! "fields" ! 0)
          diffMutability m (val' ! "fields" ! 1)
        ("Struct", StructP p fp d _) -> do
          diffPath              p  (val' ! "fields" ! 0)
          liftDiff diffFieldPat fp (val' ! "fields" ! 1)
          when (Data.Aeson.Bool d /= (val' ! "fields" ! 2)) $ diff "differing `..'" p val
        ("TupleStruct", TupleStructP p fp mi _) -> do
          diffPath               p  (val' ! "fields" ! 0)
          liftDiff diffPat       fp (val' ! "fields" ! 1)
          maybeDiff diffIntegral mi (val' ! "fields" ! 2)
        ("Tuple", TupleP fp mi _) -> do
          liftDiff diffPat       fp (val' ! "fields" ! 0)
          maybeDiff diffIntegral mi (val' ! "fields" ! 1)
        ("Range", RangeP e1 e2 _) -> do
          diffExpr e1 (val' ! "fields" ! 0)
          diffExpr e2 (val' ! "fields" ! 1)
        ("Slice", SliceP b m a _) -> do
          liftDiff  diffPat b (val' ! "fields" ! 0)
          maybeDiff diffPat m (val' ! "fields" ! 1)
          liftDiff  diffPat a (val' ! "fields" ! 2)
        ("Path", PathP q p _) -> do
          maybeDiff diffQSelf q (val' ! "fields" ! 0)
          diffPath            p (val' ! "fields" ! 1)
        _ -> diff "differing patterns" p val
    _ -> diff "differing patterns" p val

diffMac :: Show a => Mac a -> Value -> Diff
diffMac m val = diff "Unimplemented: diffMac" m val

diffIntegral :: (Show i, Integral i) => i -> Value -> Diff
diffIntegral i (Number s) | fromIntegral i == s = pure ()
diffIntegral i val = diff "different integral values" i val

diffFieldPat :: Show a => FieldPat a -> Value -> Diff
diffFieldPat f@(FieldPat mi p _) val = do 
  -- Extract the identifier and whether the pattern is shorthand
  let (Ident i _,s) = case (mi,p) of
                        (Nothing, IdentP (ByValue Immutable) i Nothing _) -> (i, True)
                        (Just i', p) -> (i', False)
  
  when (String (fromString i) /= val ! "node" ! "ident") $
    diff "differing field pat identifier" f val
  when (Data.Aeson.Bool s /= val ! "node" ! "is_shorthand") $
    diff "differing shorthand" f val
  diffPat p (val ! "node" ! "pat")

diffField :: Show a => Field a -> Value -> Diff
diffField f@(Field i me _) val = do 
  -- Extract the expression and whether the pattern is shorthand
  case (me, val ! "is_shorthand") of
    (Nothing, Data.Aeson.Bool True) -> pure ()
    (Just e, Data.Aeson.Bool False) -> diffExpr e (val ! "expr")
  
  diffIdent i (val ! "ident")

diffIdent :: Ident -> Value -> Diff
diffIdent a@(Ident i _) val
  | String (fromString i) == val ! "node" = pure ()
  | otherwise = diff "different identifiers" a val 

diffBindingMode :: BindingMode -> Value -> Diff
diffBindingMode bm val =
  case (val ! "variant", bm) of
    ("ByValue", ByValue m) -> diffMutability m (val ! "fields" ! 0)
    ("ByRef", ByRef m) -> diffMutability m (val ! "fields" ! 0)
    _ -> diff "differing binding mode" bm val

diffMutability :: Mutability -> Value -> Diff
diffMutability Mutable "Mutable" = pure ()
diffMutability Immutable "Immutable" = pure ()
diffMutability m val = diff "differing mutability" m val

diffTy :: Show a => Ty a -> Value -> Diff
diffTy t val = do
  let n = val ! "node"
  case (n, t) of
    ("Never", Never _) -> pure ()
    ("Infer", Infer _) -> pure ()
    (Data.Aeson.Object{}, _) ->
      case (n ! "variant", t) of
        ("Path", PathTy q p _) -> do
          maybeDiff diffQSelf q (n ! "fields" ! 0)
          diffPath p (n ! "fields" ! 1)
        ("Tup", TupTy t _) -> liftDiff diffTy t (n ! "fields" ! 0)
        ("Slice", Slice t _) -> diffTy t (n ! "fields" ! 0)
        ("Array", Language.Rust.Syntax.AST.Array t e _) -> do
          diffTy t   (n ! "fields" ! 0)
          diffExpr e (n ! "fields" ! 1)
        ("Ptr", Ptr m t _) -> do
          diffMutability m (n ! "fields" ! 0 ! "mutbl")
          diffTy         t (n ! "fields" ! 0 ! "ty")
        ("Rptr", Rptr lt m t _) -> do
          maybeDiff diffLifetime lt (n ! "fields" ! 0)
          diffMutability m (n ! "fields" ! 1 ! "mutbl")
          diffTy         t (n ! "fields" ! 1 ! "ty")
        ("BareFn", BareFn u a lts decl _) -> do
          diffUnsafety u (n ! "fields" ! 0 ! "unsafety")
          diffAbi a (n ! "fields" ! 0 ! "abi")
          liftDiff diffLifetimeDef lts (n ! "fields" ! 0 ! "lifetimes")
          diffFnDecl   decl (n ! "fields" ! 0 ! "decl")
        ("TraitObject", TraitObject bds _) ->
          liftDiff diffTyParamBound bds (n ! "fields" ! 0)
        ("Paren", ParenTy t _) -> diffTy t (n ! "fields" ! 0)
        ("Typeof", Typeof e _) -> diffExpr e (n ! "fields" ! 0)
        ("Mac", MacTy m _) -> diffMac m (n ! "fields" ! 0)
        ("ImplTrait", ImplTrait bds _) ->
          liftDiff diffTyParamBound bds (n ! "fields" ! 0)
        _ -> diff "differing types" t val
    _ -> diff "differing types" t val


diffTyParamBound :: Show a => TyParamBound a -> Value -> Diff
diffTyParamBound bd val =
  case (val ! "variant", bd) of
    ("TraitTyParamBound", TraitTyParamBound p m _) -> do
      diffPolyTraitRef p (val ! "fields" ! 0)
      diffTraitBoundModifier m (val ! "fields" ! 1)
    ("RegionTyParamBound", RegionTyParamBound l _) ->
      diffLifetime l (val ! "fields" ! 0)
    _ -> diff "different type parameter bounds" bd val

diffPolyTraitRef :: Show a => PolyTraitRef a -> Value -> Diff
diffPolyTraitRef p@(PolyTraitRef lts tr _) val = do
  liftDiff diffLifetimeDef lts (val ! "bound_lifetimes")
  diffTraitRef tr (val ! "trait_ref")

diffTraitRef :: Show a => TraitRef a -> Value -> Diff
diffTraitRef (TraitRef p) val = diffPath p (val ! "path")

diffTraitBoundModifier :: TraitBoundModifier -> Value -> Diff
diffTraitBoundModifier None  "None" = pure ()
diffTraitBoundModifier Maybe "Maybe" = pure ()
diffTraitBoundModifier m val = diff "different trait boudn modifier" m val

diffLifetime :: Show a => Lifetime a -> Value -> Diff
diffLifetime l@(Lifetime n _) val
  | fromString ("'" ++ n) /= val ! "name" = diff "lifetime is different" l val
  | otherwise = pure ()

diffQSelf :: Show a => QSelf a -> Value -> Diff
diffQSelf q@(QSelf t p) val = do
  diffTy t (val ! "ty")
  when (Number (fromIntegral p) /= (val ! "position")) $
    diff "differing position in QSelf" q val

diffPath :: Show a => Path a -> Value -> Diff
diffPath (Path g segs _) val = do
  let segs' = if g then (("{{root}}", NoParameters undefined) <| segs) else segs
  liftDiff diffPathPair segs' (val ! "segments")
  where
    diffPathPair :: Show a => (Ident, PathParameters a) -> Value -> Diff
    diffPathPair (i, pp) val = do
      when (fromString (show i) /= val ! "identifier") $
        diff "path segment has different name" (i,pp) val
      diffPathParameters pp (val ! "parameters")

diffPathParameters :: Show a => PathParameters a -> Value -> Diff
diffPathParameters n@(NoParameters _) val = when (val /= Data.Aeson.Null) $
                                              diff "expected no parameters" n val 
diffPathParameters p val =
  case (val ! "variant", p) of
    ("AngleBracketed", AngleBracketed lts tys bds _) -> do
      liftDiff diffLifetime lts (val ! "fields" ! 0 ! "lifetimes")
      liftDiff diffTy       tys (val ! "fields" ! 0 ! "types")
      liftDiff diffBinding  bds (val ! "fields" ! 0 ! "bindings")
    _ -> diff "Unimplemented: diffPathParameters" p val
  where
  diffBinding :: Show a => (Ident, Ty a) -> Value -> Diff
  diffBinding l val = diff "Unimplemented: diffBindings" l val

-- TODO: attribtues
diffExpr :: Show a => Expr a -> Value -> Diff
diffExpr e val = do
  let n = val ! "node"
  case (n ! "variant", e) of
    ("Lit", Lit as l _) ->
    --  liftDiff diffAttribute as (val ! "attrs") TODO: this is funky
      diffLit l (n ! "fields" ! 0)
    ("Tup", TupExpr as es _) ->
      liftDiff diffExpr es (n ! "fields" ! 0)
    ("Match", Match as e arms _) -> do
      diffExpr e (n ! "fields" ! 0)
      liftDiff diffArm arms (n ! "fields" ! 1)
    ("Box", Box as e _) ->
      diffExpr e (n ! "fields" ! 0)
    ("InPlace", InPlace as e1 e2 _) -> do
      diffExpr e1 (n ! "fields" ! 0)
      diffExpr e2 (n ! "fields" ! 1)
    ("Path", PathExpr as q p _) -> do
      maybeDiff diffQSelf q (n ! "fields" ! 0)
      diffPath p  (n ! "fields" ! 1)
    ("Array", Vec as es _) -> 
      liftDiff diffExpr es (n ! "fields" ! 0)
    ("Call", Call as f es _) -> do
      diffExpr f (n ! "fields" ! 0)
      liftDiff diffExpr es (n ! "fields" ! 1)
    ("MethodCall", MethodCall as o i tys es _) -> do
      diffIdent i (n ! "fields" ! 0)
      liftDiff diffTy (fromMaybe [] tys) (n ! "fields" ! 1)
      liftDiff diffExpr (o : es) (n ! "fields" ! 2) 
    ("Binary", Binary as o e1 e2 _) -> do
      diffBinOp o (n ! "fields" ! 0)
      diffExpr e1 (n ! "fields" ! 1) 
      diffExpr e2 (n ! "fields" ! 2) 
    ("Unary", Unary as o e _) -> do
      diffUnOp o (n ! "fields" ! 0)
      diffExpr e (n ! "fields" ! 1)
    ("Cast", Cast as e t _) -> do
      diffExpr e (n ! "fields" ! 0)
      diffTy   t (n ! "fields" ! 1)
    ("Type", TypeAscription as e t _) -> do
      diffExpr e (n ! "fields" ! 0)
      diffTy   t (n ! "fields" ! 1)
    ("Block", BlockExpr as b _) ->
      diffBlock b (n ! "fields" ! 0)
    ("If", If as e tb ee _) -> do
      diffExpr e (n ! "fields" ! 0)
      diffBlock tb (n ! "fields" ! 1)
      maybeDiff diffExpr ee (n ! "fields" ! 2)
    ("IfLet", IfLet as p e tb ee _) -> do
      diffPat p (n ! "fields" ! 0)
      diffExpr e (n ! "fields" ! 1)
      diffBlock tb (n ! "fields" ! 2)
      maybeDiff diffExpr ee (n ! "fields" ! 3)
    ("While", While as e b l _) -> do
      diffExpr e (n ! "fields" ! 0)
      diffBlock b (n ! "fields" ! 1)
      maybeDiff diffLbl l (n ! "fields" ! 2)
    ("WhileLet", WhileLet as p e b l _) -> do
      diffPat p (n ! "fields" ! 0)
      diffExpr e (n ! "fields" ! 1)
      diffBlock b (n ! "fields" ! 2)
      maybeDiff diffLbl l (n ! "fields" ! 3)
    ("Continue", Continue as l _) ->
      maybeDiff diffLbl l (n ! "fields" ! 0)
    ("Break", Break as l e _) -> do
      maybeDiff diffLbl l (n ! "fields" ! 0)
      maybeDiff diffExpr e (n ! "fields" ! 1)
    ("ForLoop", ForLoop as p e b l _) -> do
      diffPat p (n ! "fields" ! 0)
      diffExpr e (n ! "fields" ! 1)
      diffBlock b (n ! "fields" ! 2)
      maybeDiff diffLbl l (n ! "fields" ! 3)
    ("Loop", Loop as b l _) -> do
      diffBlock b (n ! "fields" ! 0)
      maybeDiff diffLbl l (n ! "fields" ! 1)
    ("Range", Range as l h rl _) -> do
      maybeDiff diffExpr l (n ! "fields" ! 0)
      maybeDiff diffExpr h (n ! "fields" ! 1)
      diffRangeLimits rl (n ! "fields" ! 2) 
    ("Closure", Closure as c decl e _) -> do
      diffCaptureBy c (n ! "fields" ! 0)
      diffFnDecl decl (n ! "fields" ! 1)
      diffExpr e (n ! "fields" ! 2)
    ("Assign", Assign as l r _) -> do
      diffExpr l (n ! "fields" ! 0)
      diffExpr r (n ! "fields" ! 1)
    ("AssignOp", AssignOp as o l r _) -> do
      diffBinOp o (n ! "fields" ! 0)
      diffExpr  l (n ! "fields" ! 1)
      diffExpr  r (n ! "fields" ! 2)
    ("Field", FieldAccess as e i _) -> do
      diffExpr  e (n ! "fields" ! 0)
      diffIdent i (n ! "fields" ! 1)
    ("TupField", TupField as e i _) -> do
      diffExpr     e (n ! "fields" ! 0)
      diffIntegral i (n ! "fields" ! 1 ! "node")
    ("Index", Language.Rust.Syntax.AST.Index as e1 e2 _) -> do
      diffExpr e1 (n ! "fields" ! 0)
      diffExpr e2 (n ! "fields" ! 1)
    ("AddrOf", AddrOf as m e _) -> do
      diffMutability m (n ! "fields" ! 0)
      diffExpr       e (n ! "fields" ! 1)
    ("Ret", Ret as e _) ->
      maybeDiff diffExpr e (n ! "fields" ! 0)
    ("Mac", MacExpr as m _) ->
      diffMac m (n ! "fields" ! 0)
    ("Struct", Struct as p f e _) -> do
      diffPath p           (n ! "fields" ! 0)
      liftDiff diffField f (n ! "fields" ! 1)
      maybeDiff diffExpr e (n ! "fields" ! 2)
    ("Repeat", Repeat as e1 e2 _) -> do
      diffExpr e1 (n ! "fields" ! 0)
      diffExpr e2 (n ! "fields" ! 1)
    ("Paren", ParenExpr as e _) ->
      diffExpr e (n ! "fields" ! 0)
    ("Try", Try as e _) ->
      diffExpr e (n ! "fields" ! 0) 
    _ -> diff "differing expressions:" e val
  where
  diffLbl :: Show a => Lifetime a -> Value -> Diff
  diffLbl l@(Lifetime n _) val = when (String (fromString ("'" ++ n)) /= val ! "node") $
                                   diff "labels are different" l val

diffCaptureBy :: CaptureBy -> Value -> Diff
diffCaptureBy Value "Value" = pure ()
diffCaptureBy c val = diff "Unimplemented capture by" c val

diffRangeLimits :: RangeLimits -> Value -> Diff
diffRangeLimits HalfOpen "HalfOpen" = pure ()
diffRangeLimits Closed "Closed" = pure ()
diffRangeLimits rl val = diff "different range limits" rl val

diffBinOp :: BinOp -> Value -> Diff
diffBinOp b val =
  case (val ! "node", b) of
    ("Add", AddOp) -> pure ()
    ("Mul", MulOp) -> pure ()
    _ -> diff "Unimplemented: diffBinOp" b val

diffUnOp :: UnOp -> Value -> Diff
diffUnOp Not "Not" = pure ()
diffUnOp Deref "Deref" = pure ()
diffUnOp u v = diff "Unimplemented: diffUnOp" u v

diffArm :: Show a => Arm a -> Value -> Diff
diffArm (Arm as ps g b _) val = do
  liftDiff diffAttribute as (val ! "attrs")
  liftDiff diffPat ps (val ! "pats")
  maybeDiff diffExpr g (val ! "guard")
  diffExpr b (val ! "body")

diffLit :: Show a => Lit a -> Value -> Diff
diffLit l val = do
  let n = val ! "node"
  case (n ! "variant", l) of
    ("Int", Int _ i suf _) -> do
      when (Number (fromInteger i) /= n ! "fields" ! 0) $
        diff "int literal has two different values" l val
      diffSuffix suf (n ! "fields" ! 1)
    ("Float", Float f suf _) -> do
      let String j = n ! "fields" ! 0
      when (f /= read (T.unpack j)) $
        diff "float literal has two different values" l val
      diffSuffix suf (n ! "fields" ! 1)
    ("Str", Str s sty Unsuffixed _) -> do
      when (String (fromString s) /= n ! "fields" ! 0) $
        diff "string literal has two different values" l val
      diffStrStyle sty (n ! "fields" ! 1)
    ("Char", Char c Unsuffixed _) ->
      when (String (fromString [c]) /= n ! "fields" ! 0) $
        diff "character literal has two different values" l val
    ("Byte", Byte b Unsuffixed _) ->
      diffIntegral b (n ! "fields" ! 0)
    ("ByteStr", ByteStr s sty Unsuffixed _) ->
      liftDiff diffIntegral s (n ! "fields" ! 0)
    ("Bool", Language.Rust.Syntax.AST.Bool b Unsuffixed _) ->
      when (Data.Aeson.Bool b /= n ! "fields" ! 0) $
        diff "boolean literal has two different values" l val
    _ -> diff "different literals" l val

diffStrStyle :: StrStyle -> Value -> Diff
diffStrStyle Cooked "Cooked" = pure ()
diffStrStyle (Raw n) val | val ! "variant" == "Raw" = diffIntegral n (val ! "fields" ! 0)
diffStrStyle sty val = diff "different string style" sty val

diffSuffix :: Suffix -> Value -> Diff
diffSuffix Unsuffixed "Unsuffixed" = pure ()
diffSuffix F32 "F32" = pure ()
diffSuffix F64 "F64" = pure ()
diffSuffix Is val   | val ! "fields" == Data.Aeson.Array ["Is"]   = pure () 
diffSuffix I8 val   | val ! "fields" == Data.Aeson.Array ["I8"]   = pure () 
diffSuffix I16 val  | val ! "fields" == Data.Aeson.Array ["I16"]  = pure () 
diffSuffix I32 val  | val ! "fields" == Data.Aeson.Array ["I32"]  = pure () 
diffSuffix I64 val  | val ! "fields" == Data.Aeson.Array ["I64"]  = pure () 
diffSuffix I128 val | val ! "fields" == Data.Aeson.Array ["I128"] = pure () 
diffSuffix Us val   | val ! "fields" == Data.Aeson.Array ["Us"]   = pure () 
diffSuffix U8 val   | val ! "fields" == Data.Aeson.Array ["U8"]   = pure () 
diffSuffix U16 val  | val ! "fields" == Data.Aeson.Array ["U16"]  = pure () 
diffSuffix U32 val  | val ! "fields" == Data.Aeson.Array ["U32"]  = pure () 
diffSuffix U64 val  | val ! "fields" == Data.Aeson.Array ["U64"]  = pure () 
diffSuffix U128 val | val ! "fields" == Data.Aeson.Array ["U128"] = pure () 
diffSuffix u val = diff "Unimplemented: diffSuffix" u val
