{-# OPTIONS_GHC -Wno-missing-methods #-}
module DiffUtils where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import Language.Rust.Syntax.Ident
import Control.Monad
import Data.String
import Data.ByteString.Lazy.Char8 (unpack)
import Debug.Trace
import Control.Exception
import Data.Typeable
import Data.Foldable


-- | This type is a straightforward hack to let me index by both 'String' and 'Int' in '(!)' below.
data AesonKey = Index Int | Key String
instance Num AesonKey where fromInteger = Index . fromIntegral
instance IsString AesonKey where fromString = Key

-- | Accessor method for JSON with helpful error messages.
(!) :: Aeson.Value -> AesonKey -> Aeson.Value
val@(Aeson.Object map) ! Key key =
  case HM.lookup (fromString key) map of
    Nothing -> error $ "No key `" ++ key ++ "' on JSON object `" ++ show val ++ "'"
    Just v -> v
val ! Key key = error $ "Cannot lookup key `" ++ key ++ "' on non-object JSON `" ++ show val ++ "'"
val@(Aeson.Array vect) ! Index key =
  case vect V.!? key of
    Nothing -> error $ "Index `" ++ show key ++ "' is OOB on JSON array `" ++ show val ++ "'"
    Just v -> v
val ! Index key = error $ "Cannot lookup index `" ++ show key ++ "' on non-array JSON `" ++ show val ++ "'"

-- | Accessor method for JSON which fails with 'Nothing'
(!?) :: Aeson.Value -> AesonKey -> Maybe Aeson.Value
val@(Aeson.Object map) !? Key key = HM.lookup (fromString key) map
val@(Aeson.Array vect) !? Index key = vect V.!? key
_ !? _ = Nothing

-- | This lets us do whatever we want while comparing @rustc@ with our parser
type Diff = IO ()

-- | This data type exists only as an easy way to throw a new type of error
data DiffError = DiffError String deriving (Typeable)
instance Exception DiffError
instance Show DiffError where show (DiffError msg) = msg



  

