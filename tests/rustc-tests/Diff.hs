{-# LANGUAGE OverloadedStrings #-}
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

import DiffUtils


instance Pretty a => Pretty [a] where
  pretty xs = error "unimplemented"

liftDiff :: (Foldable f, Show a) => (a -> Value -> Diff) -> (f a -> Value -> Diff)
liftDiff f xs val@(Data.Aeson.Array v) = do
  let xs' = toList v
  when (length xs /= length xs') $
    diff "arrays have different lengths" (toList xs) val
  sequence_ [ f x x' | (x,x') <- toList xs `zip` xs' ]
liftDiff _ xs v = diff "comparing array to non-array" (toList xs) v

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
   -- (Just t', Just t) -> diffTy t t'

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
    ("Local", Local p t i as _) ->
      diffPat       p    (n' ! "fields" ! 0 ! "pat")
   --   diffTy        t    (n' ^?! key "fields" . nth 0 . key "ty")
   --   diffInit      i    (n' ^?! key "fields" . nth 0 . key "init")

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

diffNestedMetaItem :: NestedMetaItem a -> Value -> Diff
diffNestedMetaItem _ _ = pure ()


diffLit :: Lit a -> Value -> Diff
diffLit _ _ = pure ()

diffPat :: Pat a -> Value -> Diff
diffPat p val = do
  let val' = val ! "node"
  case (val' ! "variant", p) of
    ("Ident", IdentP bm i m _) ->
      diffBindingMode bm (val' ! "fields" ! 0)
-- TODO      
    _ -> error $ "Unimplemented: diffPat " ++ unpack (encode val)

diffBindingMode :: BindingMode -> Value -> Diff
diffBindingMode _ _ = error "Unimplemented: diffBindingMode"


diffTy :: Ty a -> Value -> Diff
diffTy _ _ = error "Unimplemented: diffTy"

