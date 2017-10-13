{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS_GHC -Wno-missing-methods #-}
#endif
module DiffUtils where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import qualified Data.List.NonEmpty as N
import Control.Monad
import Data.String
import Data.ByteString.Lazy.Char8 (unpack)
import Control.Exception
import Data.Typeable
import Data.Foldable
import Data.Word (Word8)


-- | This type is a straightforward hack to let me index by both 'String' and 'Int' in '(!)' below.
data AesonKey = Index Int | Key String
instance Num AesonKey where fromInteger = Index . fromIntegral
instance IsString AesonKey where fromString = Key

-- | Accessor method for JSON with helpful error messages.
(!) :: Aeson.Value -> AesonKey -> Aeson.Value
val@(Aeson.Object hashmap) ! Key key =
  case HM.lookup (fromString key) hashmap of
    Nothing -> error $ "No key `" ++ key ++ "' on JSON object `" ++ showAeson val ++ "'"
    Just v -> v
val ! Key key = error $ "Cannot lookup key `" ++ key ++ "' on non-object JSON `" ++ showAeson val ++ "'"
val@(Aeson.Array vect) ! Index key =
  case vect V.!? key of
    Nothing -> error $ "Index `" ++ show key ++ "' is OOB on JSON array `" ++ showAeson val ++ "'"
    Just v -> v
val ! Index key = error $ "Cannot lookup index `" ++ show key ++ "' on non-array JSON `" ++ showAeson val ++ "'"

-- | Pretty print 'Value'
showAeson :: Aeson.Value -> String
showAeson = unpack . Aeson.encode

-- | Accessor method for JSON which fails with 'Nothing'
(!?) :: Aeson.Value -> AesonKey -> Maybe Aeson.Value
Aeson.Object hashmap !? Key key = HM.lookup (fromString key) hashmap
Aeson.Array vect !? Index key = vect V.!? key
_ !? _ = Nothing

-- | This lets us do whatever we want while comparing @rustc@ with our parser
type Diff = IO ()

-- | This data type exists only as an easy way to throw a new type of error
data DiffError = DiffError String deriving (Typeable)
instance Exception DiffError
instance Show DiffError where show (DiffError msg) = msg

-- | Class of things that can be diff-ed against their JSON debug output
class Show a => Diffable a where
  (===) :: a -> Aeson.Value -> Diff

instance Diffable a => Diffable (N.NonEmpty a) where
  xs === json = toList xs === json

instance Diffable a => Diffable [a] where
  xs === json@(Aeson.Array v) = do
    let xs' = toList v
    when (length xs /= length xs') $
      diff ("arrays have different lengths " ++ show (length xs) ++ " /= " ++ show (length xs')) xs json
    sequence_ (zipWith (===) xs xs')
  xs === json = diff "comparing array to non-array" xs json

-- | Solely for an instance of 'Diffable [a]' where the empty list == null
newtype NullList a = NullList [a] deriving (Show)
instance Diffable a => Diffable (NullList a) where
  NullList xs === val = (if null xs then Nothing else Just xs) === val

-- | a comparision to accept 'null' as 'Nothing'
instance Diffable a => Diffable (Maybe a) where
  Just x    === json       = x === json
  Nothing   === Aeson.Null = pure ()
  n@Nothing === json       = diff "expected the JSON to be null" n json

instance Diffable Bool where
  b1 === j@(Aeson.Bool b2) | b1 == b2 = pure ()
                           | otherwise = diff "boolean values are different" b1 j
  b === j = diff "expected the JSON to be a boolean" b j

instance Diffable Word8 where (===) = diffIntegral
instance Diffable Int where (===) = diffIntegral
instance Diffable Integer where (===) = diffIntegral

-- | Diff something that is a number and can be shown
diffIntegral :: (Show i, Integral i) => i -> Aeson.Value -> Diff
diffIntegral i (Aeson.Number s) | fromIntegral i == s = pure ()
diffIntegral i val = diff "different integral values" i val

-- | Report a difference
diff :: Show a => String -> a -> Aeson.Value -> IO b
diff explanation v j = throw (DiffError msg)
  where msg = unlines [ explanation ++ " in"
                      , " * parsed AST"
                      , cropped (show v)
                      , " * dumped JSON"
                      , cropped (unpack (Aeson.encode j))
                      ]
        cropped msg' | length msg' > 500 = take 500 msg' ++ "..."
                     | otherwise = msg'

  

