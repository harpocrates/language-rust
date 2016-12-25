{-# LANGUAGE OverloadedStrings, UnicodeSyntax, FlexibleContexts  #-}
module ParserTest (parserSuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Parser.ParseMonad
import Language.Rust.Parser.Parser2
import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
import Language.Rust.Data.InputStream

import Control.Monad
import Control.Monad.Trans.Except

parserSuite :: Test
parserSuite = testGroup "parser suite" [ patternSuite, typeSuite, expressionSuite ]

-- | This contains tests for parsing a variety of patterns
patternSuite :: Test
patternSuite = testGroup "parsing patterns"
  [ testPat "_"                       (WildP ())
  , testPat "x"                       x
  , testPat "&x"                      (RefP x Immutable ())
  , testPat "&mut x"                  (RefP x Mutable ())
  , testPat "(x, ref mut y, box z)"   (TupleP [ x
                                              , IdentP (ByRef Mutable) "y" Nothing ()
                                              , BoxP (IdentP (ByValue Immutable) "z" Nothing ()) ()
                                              ] 
                                              Nothing ())
  , testPat "true"                    (LitP (Lit [] (Bool True Unsuffixed ()) ()) ())
  , testPat "-123"                    (LitP (Unary [] Neg (Lit [] (Int 123 Unsuffixed ()) ()) ()) ())
  , testPat "Point { .. }"            (StructP (Path False [("Point", AngleBracketed [] [] [] ())] ()) [] True ())
  , testPat "Point { x, y: y1 }"      (StructP (Path False [("Point", AngleBracketed [] [] [] ())] ())
                                               [ FieldPat "x" x True ()
                                               , FieldPat "y" (IdentP (ByValue Immutable) "y" (Just (IdentP (ByValue Immutable) "y1" Nothing ())) ()) False () ]
                                               False ()) 
  , testPat "Point { x, .. }"         (StructP (Path False [("Point", AngleBracketed [] [] [] ())] ())
                                               [ FieldPat "x" x True () ]
                                               True ()) 
  , testPat "math::PI"                (PathP Nothing (Path False [ ("math", AngleBracketed [] [] [] ())
                                                                 , ("PI", AngleBracketed [] [] [] ()) ] ()) ())
  , testPat "1...2"                   (RangeP (Lit [] (Int 1 Unsuffixed ()) ()) (Lit [] (Int 2 Unsuffixed ()) ()) ())
  , testPat "ref mut y@(x,x)"         (IdentP (ByRef Mutable) "y" (Just (TupleP [ x, x ] Nothing ())) ())
  , testPat "(1,2..3)"                (TupleP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int 2 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ]
                                              (Just 2) ())
  , testPat "(1,2,..,3)"              (TupleP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int 2 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ]
                                              (Just 2) ())
  , testPat "Point(x)"                (TupleStructP (Path False [("Point", AngleBracketed [] [] [] ())] ())
                                              [ x ] Nothing ())
  , testPat "<i32 as a(i32, i32)>::b::<'lt>::AssociatedItem"
            (PathP (Just (QSelf i32 1)) (Path False [ ("a", Parenthesized [i32, i32] Nothing ())
                                                    , ("b", AngleBracketed [Lifetime (Name "lt") ()] [] [] ())
                                                    , ("AssociatedItem", AngleBracketed [] [] [] ())
                                                    ] ()) ())
  , testPat "[1,2]"                   (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int 2 Unsuffixed ()) ()) () ] 
                                              (Just (WildP ())) [] ())
  , testPat "[1,..,3]"                (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ]
                                              (Just (WildP ()))
                                              [ LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ] ())
  , testPat "[1,x..,3]"               (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ]
                                              (Just x)
                                              [ LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ] ())
  , testPat "[1,..]"                  (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ] (Just (WildP ())) [] ())
  , testPat "[1,x..]"                 (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ] (Just x) [] ())
  ]
  where
    -- Just a common pattern to make the tests above more straightforward
    x :: Pat ()
    x = IdentP (ByValue Immutable) "x" Nothing ()
    -- Just a common type to make the tests above more straightforward
    i32 :: Ty ()
    i32 = PathTy Nothing (Path False [("i32", AngleBracketed [] [] [] ())] ()) ()


-- | This contains tests for parsing a variety of types
typeSuite :: Test
typeSuite = testGroup "parsing types"
  [ testType "i32"          i32
  , testType "()"           (TupTy [] ())
  , testType "(i32,)"       (TupTy [i32] ())
  , testType "(i32,())"     (TupTy [i32, TupTy [] ()] ())
  , testType "!"            (Never ())
  , testType "* i32"        (Ptr Immutable i32 ())
  , testType "*mut i32"     (Ptr Mutable i32 ())
  , testType "*const i32"   (Ptr Immutable i32 ())
  , testType "[i32]"        (Slice i32 ())
  , testType "[i32; 17]"    (Array i32 (Lit [] (Int 17 Unsuffixed ()) ()) ())
  , testType "&i32"         (Rptr Nothing Immutable i32 ())
  , testType "&'lt mut i32" (Rptr (Just (Lifetime (Name "lt") ())) Mutable i32  ())
  , testType "_"            (Infer ())
  , testType "typeof(123)"  (Typeof (Lit [] (Int 123 Unsuffixed ()) ()) ())
  , testType "Vec<i32>"     (PathTy Nothing (Path False [("Vec", AngleBracketed [] [i32] [] ())] ()) ())
  , testType "<i32 as a::b::Trait>::AssociatedItem"
             (PathTy (Just (QSelf i32 3)) (Path False [ ("a", AngleBracketed [] [] [] ())
                                                      , ("b", AngleBracketed [] [] [] ())
                                                      , ("Trait", AngleBracketed [] [] [] ())
                                                      , ("AssociatedItem", AngleBracketed [] [] [] ())
                                                      ] ()) ())
  , testType "<i32 as a(i32, i32)::b<'lt>::Trait(i32) -> i32>::AssociatedItem"
             (PathTy (Just (QSelf i32 3)) (Path False [ ("a", Parenthesized [i32, i32] Nothing ())
                                                      , ("b", AngleBracketed [Lifetime (Name "lt") ()] [] [] ())
                                                      , ("Trait", Parenthesized [i32] (Just i32) ())
                                                      , ("AssociatedItem", AngleBracketed [] [] [] ())
                                                      ] ()) ())
  , testType "fn(i32...)"
             (BareFn Normal Rust [] (FnDecl [Arg i32 (IdentP (ByValue Immutable) "" Nothing ()) ()] Nothing True ()) ())
  , testType "fn(i32) -> i32"
             (BareFn Normal Rust [] (FnDecl [Arg i32 (IdentP (ByValue Immutable) "" Nothing ()) ()] (Just i32) False ()) ())
  , testType "fn(_: i32) -> i32"
             (BareFn Normal Rust [] (FnDecl [Arg i32 (WildP ()) ()] (Just i32) False ()) ())
  , testType "unsafe extern \"C\" fn(_: i32)"
             (BareFn Unsafe C [] (FnDecl [Arg i32 (WildP ()) ()] Nothing False ()) ())
  , testType "PResult<'a, P<i32>>"
             (PathTy Nothing (Path False [("PResult", AngleBracketed [ Lifetime (Name "a") () ]
                                                                     [ PathTy Nothing (Path False [("P", AngleBracketed [] [ i32 ] [] ())] ()) () ]
                                                                     [] ())] ()) ())
  , testType "for<'l1: 'l2 + 'l3, 'l4: 'l5> fn(_: i32 + 'l1) -> i32"
             (BareFn Normal Rust
                            [ LifetimeDef [] (Lifetime (Name "l1") ()) [Lifetime (Name "l2") (), Lifetime (Name "l3") ()] ()
                            , LifetimeDef [] (Lifetime (Name "l4") ()) [Lifetime (Name "l5") ()] () ]
                            (FnDecl [Arg (ObjectSum i32 [RegionTyParamBound (Lifetime (Name "l1") ())] ()) (WildP ()) ()] 
                                    (Just i32) False ()) ())
  , testType "for <'a> Foo<&'a T>"
             (PolyTraitRefTy
                [TraitTyParamBound
                   (PolyTraitRef [LifetimeDef [] (Lifetime (Name "a") ()) [] ()]
                                 (TraitRef (Path False [("Foo", AngleBracketed [] [Rptr (Just (Lifetime (Name "a") ()))
                                                                                        Immutable
                                                                                        (PathTy Nothing (Path False [("T", AngleBracketed [] [] [] ())] ()) ())
                                                                                        ()]
                                                                                  [] ())] ()) ()) ()) None] ())
  ]
  where
    -- Just a common type to make the tests above more straightforward
    i32 :: Ty ()
    i32 = PathTy Nothing (Path False [("i32", AngleBracketed [] [] [] ())] ()) ()

-- | This contains tests for parsing a variety of expressions
expressionSuite :: Test
expressionSuite = testGroup "parsing expressions" []

testParse :: (Show (f ()), Functor f, Eq (f ())) => String -> P (Spanned (f Span)) -> f () -> Test
testParse inp parser x = testCase inp $ Right x @=? parseNoSpans (unspan <$> parser) (inputStreamFromString inp)

-- | Create a test for a code fragment that should parse to an expression.
testExpr :: String -> Expr () -> Test
testExpr inp expr = testCase inp $ Right expr @=? parseNoSpans expressionP (inputStreamFromString inp)

-- | Create a test for a code fragment that should parse to a type.
testType :: String -> Ty () -> Test
testType inp ty = testCase inp $ Right ty @=? parseNoSpans typeP (inputStreamFromString inp)

-- | Create a test for a code fragment that should parse to a pattern.
testPat :: String -> Pat () -> Test
testPat inp pat = testCase inp $ Right pat @=? parseNoSpans patternP (inputStreamFromString inp)

-- | Turn an InputStream into either an error or a parse.
parseNoSpans :: Functor f => P (f Span) -> InputStream -> Either (Position,String) (f ())
parseNoSpans parser inp = runExcept (fmap (const ()) <$> result)
  where
    -- result :: Except (Position,String) (f Span)
    result = execParser parser inp initPos

