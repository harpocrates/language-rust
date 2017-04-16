{-# LANGUAGE OverloadedStrings #-}
module Diff where

import Data.Aeson
import Language.Rust.Pretty
import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Ident
import Control.Monad.Trans.Writer
import Data.Aeson.Lens
import Control.Lens hiding (List)
import Control.Monad
import Data.String
import Data.ByteString.Lazy.Char8 (unpack)

type Diff = Writer [String] ()

diff :: Pretty a => String -> a -> Value -> Diff
diff explanation v j = tell [ msg ]
  where msg = unlines [ explanation ++ " in"
                      , " * parsed AST"
                      , show (pretty v)
                      , " * dumped JSON"
                      , unpack (encode j)
                      ]

diffSourceFile :: SourceFile a -> Value -> Diff
diffSourceFile s@(SourceFile _ as items) val = do
  -- Check attributes
  let as' = val ^.. key "attrs" . values . key "node"
  when (length as /= length as') $
    diff "different number of attributes" s val
  sequence_ [ diffAttribute a a' | (a,a') <- as `zip` as' ]
 

diffAttribute :: Attribute a -> Value -> Diff
diffAttribute a@(Attribute sty meta sug _) val = do
  -- Check style
  case (val ^?! key "style" . _String, sty) of
    ("Inner", Inner) -> pure ()
    ("Outer", Outer) -> pure ()
    _ -> diff "attribute style is different" a val

  -- Check meta item
  let meta' = val ^?! key "value"
--  diffMetaItem meta meta'

  -- Check sugared doc
  when (val ^?! key "is_sugared_doc" . _Bool /= sug) $
    diff "attribute is supposed to be sugared doc" a val

diffMetaItem :: MetaItem a -> Value -> Diff
diffMetaItem meta val = do
  -- Check that the names match
  when (fromString (name meta) /= val ^?! key "name" . _String) $
    diff "meta item has different name" meta val

  case (val ^?! key "node", meta)  of
    (String "Word", Word{}) -> pure()

    (val',          _) ->
      case (val' ^?! key "variant" . _String, meta) of
        ("NameValue", NameValue _ lit _) -> do
          -- Check value
          let lit' = val' ^?! key "fields" . nth 0
          diffLit lit lit' 
    
        ("List",      List _ args _) -> do
          -- Check arguments in list
          let args' = val' ^.. key "fields" . nth 0 . values
          when (length args /= length args') $
            diff "list meta item has different number of arguments" meta val
          sequence_ [ diffNestedMetaItem arg arg' | (arg,arg') <- args `zip` args' ]
  where 
    name :: MetaItem a -> String
    name (Word (Ident i _) _) = i
    name (List (Ident i _) _ _) = i
    name (NameValue (Ident i _) _ _) = i

diffNestedMetaItem :: NestedMetaItem a -> Value -> Diff
diffNestedMetaItem _ _ = pure ()


diffLit :: Lit a -> Value -> Diff
diffLit _ _ = pure ()


--diffTy :: Ty a -> Value -> [String]

