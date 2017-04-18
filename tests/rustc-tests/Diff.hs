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

  -- Check visibility
  diffVisibility v (val ! "vis")

diffFnDecl :: Show a => FnDecl a -> Value -> Diff
diffFnDecl decl@(FnDecl as out v _) val = do
  -- Check inputs
  let as' = val ! "inputs"
  liftDiff diffArg as as'

  -- Check output
  let outTy = val ! "output" ! "variant"
  case (outTy, out) of
    ("Default", Nothing) -> pure ()
    _ -> error "unimplemented: diffFnDecl"

  -- Check variadic
  let v' = val ! "variadic"
  when (Data.Aeson.Bool v /= v') $
    diff "different variadicity" decl val


diffArg :: Show a => Arg a -> Value -> Diff
diffArg _ _ = error "Unimplemented: diffArg"

diffUnsafety :: Unsafety -> Value -> Diff
diffUnsafety Unsafe "Unsafe" = pure ()
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
diffAbi _ _ = error "diffAbi"


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

diffTyParam :: TyParam a -> Value -> Diff
diffTyParam _ _ = error "Unimplemented: diffTyParam"

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

    _ -> error "Unimplemented: diffStmt"

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
  diffMetaItem meta meta'

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

diffPat :: Pat a -> Value -> Diff
diffPat p val = do
  let val' = val ! "node"
  case (val' ! "variant", p) of
    ("Ident", IdentP bm i m _) ->
      diffBindingMode bm (val' ! "fields" ! 0)
-- TODO      
    _ -> error $ "Unimplemented: diffPat " ++ unpack (encode val)

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
    _ -> diff "Unimplemented: diffTy" t val

diffLifetime :: Show a => Lifetime a -> Value -> Diff
diffLifetime l@(Lifetime n _) val
  | fromString ("'" ++ n) /= val ! "name" = diff "lifetime is different" l val
  | otherwise = pure ()

diffQSelf :: Show a => QSelf a -> Value -> Diff
diffQSelf q val = diff "Unimplemented: diffQSelf" q val

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
diffPathParameters n@(NoParameters _) val = when (val /= Data.Aeson.Null) $ diff "expected no parameters" n val 
diffPathParameters p val = diff "Unimplemented: diffPathParameters" p val

diffExpr :: Show a => Expr a -> Value -> Diff
diffExpr e val = do
  let n = val ! "node"
  case (n ! "variant", e) of
    ("Lit", Lit as l _) -> do
    --  liftDiff diffAttribute as (val ! "attrs") TODO: this is funky
      diffLit l (n ! "fields" ! 0)
    ("Tup", TupExpr as es _) -> do
    --  liftDiff diffAttribute as (val ! "attrs") TODO: this is funky
      liftDiff diffExpr es (n ! "fields" ! 0)
    _ -> diff "Unimplemented: diffExpr" e val

diffLit :: Show a => Lit a -> Value -> Diff
diffLit l val = do
  let n = val ! "node"
  case (n ! "variant", l) of
    ("Int", Int _ i suf _) -> do
      when (Number (fromInteger i) /= n ! "fields" ! 0) $
        diff "int literal has two different values" l val
      diffSuffix suf (n ! "fields" ! 1)
    ("Str", Str s sty Unsuffixed _) -> do
      when (String (fromString s) /= n ! "fields" ! 0) $
        diff "string literal has two different values" l val
      diffStrStyle sty (n ! "fields" ! 1)
    _ -> diff "Unimplemented: diffLit" l val

diffStrStyle :: StrStyle -> Value -> Diff
diffStrStyle Cooked "Cooked" = pure ()
diffStrStyle sty val = diff "Unimplemented: diffStrStyle" sty val

diffSuffix :: Suffix -> Value -> Diff
diffSuffix Unsuffixed "Unsuffixed" = pure ()
diffSuffix I32 val | val ! "variant" == "Signed" && val ! "fields" == Data.Aeson.Array ["I32"] = pure () 
diffSuffix u val = diff "Unimplemented: diffSuffix" u val
