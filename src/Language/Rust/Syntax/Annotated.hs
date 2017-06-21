{-|
Module      : Language.Rust.Syntax.Annotated
Description : Class for representing things that are annotated
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

Exposes a class that lets you extract an annotation from a type of kind '* -> *'. Since most of
these instances are mostly boilerplate, this also provides a way of /deriving/ the instances using
GHC's 'Generic1' given certain conditions.
-}

{-# LANGUAGE UndecidableInstances, PolyKinds, DeriveAnyClass, DataKinds, TypeOperators, ConstraintKinds, DefaultSignatures, TypeFamilies, FlexibleContexts #-}

module Language.Rust.Syntax.Annotated (
  Annotated(..)
) where


import GHC.Generics
import GHC.TypeLits
import Data.Type.Bool
import GHC.Exts


class Annotated f where
  annotation :: Monoid a => f a -> a

  default annotation :: (Generic1 f, GAnnotated (Rep1 f), ValidAnnotated (Rep1 f)) => f a -> a
  annotation = head . gAnnotation . from1

type family OneAnnotation (f :: * -> *) :: Bool where
  OneAnnotation (M1 _ _ x) = OneAnnotation x
  OneAnnotation U1 = 'False
  OneAnnotation (K1 _ _) = 'False
  OneAnnotation (Rec1 _) = 'False
  OneAnnotation (_ :.: _) = 'False
  OneAnnotation Par1 = 'True
  OneAnnotation (f :*: g) = OneAnnotation f `Xor` OneAnnotation g
  OneAnnotation (f :+: g) = OneAnnotation f && OneAnnotation g
  OneAnnotation t = TypeError ('Text "Non-exhaustive cases in 'OneAnnotation': " ':<>: 'ShowType t)

type family ValidAnnotated (f :: * -> *) :: Constraint where
  ValidAnnotated f = If (OneAnnotation f)
                        (() :: Constraint)
                        (TypeError ('Text "The type " ':<>:
                                    'ShowType f ':<>:
                                    'Text " does not have one annotation field per constructor"))

type l `Xor` r = (l && Not r) || (Not l && r)

class GAnnotated f where
  gAnnotation :: f a -> [a]

instance GAnnotated f => GAnnotated (M1 i c f) where
  gAnnotation (M1 x) = gAnnotation x

instance GAnnotated U1 where
  gAnnotation U1 = []

instance GAnnotated Par1 where
  gAnnotation (Par1 x) = [x]

instance (GAnnotated f, GAnnotated g) => GAnnotated (f :*: g) where
  gAnnotation (l :*: r) = gAnnotation l ++ gAnnotation r

instance (GAnnotated f, GAnnotated g) => GAnnotated (f :+: g) where
  gAnnotation (L1 l) = gAnnotation l
  gAnnotation (R1 r) = gAnnotation r

instance GAnnotated (f :.: g) where
  gAnnotation _ = []

instance GAnnotated (K1 i c) where
  gAnnotation _ = []

instance GAnnotated (Rec1 f) where
  gAnnotation _ = []

