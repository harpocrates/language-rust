{-# LANGUAGE OverloadedStrings, OverloadedLists, UnicodeSyntax, FlexibleContexts  #-}
module ParserTest (parserSuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Parser.ParseMonad
import Language.Rust.Parser.Parser
import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
import Language.Rust.Data.InputStream

import Control.Monad
import Control.Monad.Trans.Except

parserSuite :: Test
parserSuite = testGroup "parser suite" [ parserLiterals, parserAttributes, parserTypes ] --[ patternSuite, typeSuite, expressionSuite ]

-- | Test parsing of literals.
parserLiterals :: Test
parserLiterals = testGroup "parsing literals"
  -- bool's
  [ testLiteral "true" (Bool True Unsuffixed ())
  , testLiteral "false" (Bool False Unsuffixed ())
  -- byte's
  , testLiteral "b'a'" (Byte 97 Unsuffixed ())
  , testLiteral "b'\\n'" (Byte 10 Unsuffixed ())
  -- char's
  , testLiteral "'a'" (Char 'a' Unsuffixed ())
  , testLiteral "'\\n'" (Char '\n' Unsuffixed ()) 
  -- integers
  , testLiteral "123" (Int 123 Unsuffixed ())
  , testLiteral "123i32" (Int 123 I32 ())
  , testLiteral "0b1100_1101" (Int 205 Unsuffixed ())
  , testLiteral "0b1100_1101isize" (Int 205 Is ())
  , testLiteral "0o3170" (Int 1656 Unsuffixed ())
  , testLiteral "0o3170i64" (Int 1656 I64 ())
  , testLiteral "0xAFAC" (Int 44972 Unsuffixed ())
  , testLiteral "0xAFACu32" (Int 44972 U32 ())
  -- float's
  , testLiteral "123.1" (Float 123.1 Unsuffixed ())
  , testLiteral "123.f32" (Float 123.0 F32 ())
  , testLiteral "123.1f32" (Float 123.1 F32 ())
  , testLiteral "123e-9f32" (Float 123e-9 F32 ())
  -- string's
  , testLiteral "\"hello \\n world!\"" (Str "hello \n world!" Cooked Unsuffixed ())
  , testLiteral "r\"hello \n world!\"" (Str "hello \n world!" (Raw 0) Unsuffixed ()) 
  , testLiteral "r##\"hello \"#\n world!\"###" (Str "hello \"#\n world!" (Raw 2) Unsuffixed ())
  -- bytestring's
  , testLiteral "b\"hello \\n world!\"" (ByteStr "hello \n world!" Cooked Unsuffixed ())
  , testLiteral "rb\"hello \n world!\"" (ByteStr "hello \n world!" (Raw 0) Unsuffixed ())
  , testLiteral "rb##\"hello \"#\n world!\"###" (ByteStr "hello \"#\n world!" (Raw 2) Unsuffixed ())
  ]
  where
    -- | Create a test for a code fragment that should parse to a literal.
    testLiteral inp lit = testCase inp $ Right lit @=? parseNoSpans literalP (inputStreamFromString inp)

-- | Test parsing of (inner and outer) attributes.
parserAttributes :: Test
parserAttributes = testGroup "parsing attributes" 
  [ testAttribute "#[cfg]" (Attribute Outer (Word (mkIdent "cfg") ())False ())
  , testAttribute "#![cfg]" (Attribute Inner (Word (mkIdent "cfg") ()) False ())
  , testAttribute "#[test]" (Attribute Outer (Word (mkIdent "test") ()) False ())
  , testAttribute "#[inline(always)]" (Attribute Outer (List (mkIdent "inline") [MetaItem (Word (mkIdent "always") ()) ()] ()) False ())
  , testAttribute "#[inline(always, sometimes)]" (Attribute Outer (List (mkIdent "inline") [MetaItem (Word (mkIdent "always") ()) (),MetaItem (Word (mkIdent "sometimes") ()) ()] ()) False ())
  , testAttribute "#[cfg(target_os = \"macos\")]" (Attribute Outer (List (mkIdent "cfg") [MetaItem (NameValue (mkIdent "target_os") (Str "macos" Cooked Unsuffixed ()) ()) ()] ()) False ())
  , testAttribute "#[cfg(0, tar = \"mac\")]" (Attribute Outer (List (mkIdent "cfg") [Literal (Int 0 Unsuffixed ()) (), MetaItem (NameValue (mkIdent "tar") (Str "mac" Cooked Unsuffixed ()) ()) ()] ()) False ())
  ]
  where
    -- | Create a test for a code fragment that should parse to an attribute.
    testAttribute inp attr = testCase inp $ Right attr @=? parseNoSpans attributeP (inputStreamFromString inp)

parserTypes :: Test
parserTypes = testGroup "parsing types"
  [ testType "_" (Infer ())
  , testType "!" (Never ())
  , testType "i32" i32
  , testType "(i32,)" (TupTy [i32] ())
  , testType "(i32,())" (TupTy [i32, TupTy [] ()] ())
  , testType "()" (TupTy [] ())
  , testType "[_]" (Slice (Infer ()) ())
  , testType "[i32]" (Slice i32 ())
  , testType "[i32; 17]" (Array i32 (Lit [] (Int 17 Unsuffixed ()) ()) ())
  , testType "*()" (Ptr Immutable (TupTy [] ()) ())
  , testType "* i32" (Ptr Immutable i32 ())
  , testType "*const !" (Ptr Immutable (Never ()) ())
  , testType "*mut _" (Ptr Mutable (Infer ()) ())
  , testType "*mut i32" (Ptr Mutable i32 ())
  , testType "*const i32" (Ptr Immutable i32 ())
  , testType "&()" (Rptr Nothing Immutable (TupTy [] ()) ())
  , testType "&mut !" (Rptr Nothing Mutable (Never ()) ())
  , testType "&'lt ()" (Rptr (Just (Lifetime (Name "lt") ())) Immutable (TupTy [] ()) ())
  , testType "&'lt mut !" (Rptr (Just (Lifetime (Name "lt") ())) Mutable (Never ()) ())
  , testType "&i32" (Rptr Nothing Immutable i32 ())
  , testType "&'lt mut i32" (Rptr (Just (Lifetime (Name "lt") ())) Mutable i32  ())
  , testType "typeof(123)" (Typeof (Lit [] (Int 123 Unsuffixed ()) ()) ())
  , testType "Vec<i32>" (PathTy Nothing (Path False [("Vec", AngleBracketed [] [i32] [] ())] ()) ())
  , testType "std::vec::Vec<T>" (PathTy Nothing (Path False [ (mkIdent "std", AngleBracketed [] [] [] ())
                                                            , (mkIdent "vec", AngleBracketed [] [] [] ())
                                                            , (mkIdent "Vec", AngleBracketed [] [PathTy Nothing (Path False [(mkIdent "T", AngleBracketed [] [] [] ())] ()) ()] [] ())
                                                            ] ()) ())
  , testType "foo::baz<'a,A,B=!>" (PathTy Nothing (Path False [ (mkIdent "foo", AngleBracketed [] [] [] ())
                                                              , (mkIdent "baz", AngleBracketed [Lifetime (Name "a") ()] 
                                                                                               [PathTy Nothing (Path False [(mkIdent "A",AngleBracketed [] [] [] ())] ()) () ]
                                                                                               [(mkIdent "B", Never ())] ())
                                                              ] ()) ())
  , testType "Foo(!,!) -> !" (PathTy Nothing (Path False [ (mkIdent "Foo", Parenthesized [Never (), Never ()] (Just (Never ())) ()) ] ()) ())
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
             (BareFn Normal Rust [] (FnDecl [Arg i32 Nothing ()] Nothing True ()) ())
  , testType "fn(i32) -> i32"
             (BareFn Normal Rust [] (FnDecl [Arg i32 Nothing ()] (Just i32) False ()) ())
  , testType "fn(_: i32) -> i32"
             (BareFn Normal Rust [] (FnDecl [Arg i32 (Just (WildP ())) ()] (Just i32) False ()) ())
  , testType "unsafe extern \"C\" fn(_: i32)"
             (BareFn Unsafe C [] (FnDecl [Arg i32 (Just (WildP ())) ()] Nothing False ()) ())
  , testType "PResult<'a, P<i32>>"
             (PathTy Nothing (Path False [("PResult", AngleBracketed [ Lifetime (Name "a") () ]
                                                                     [ PathTy Nothing (Path False [("P", AngleBracketed [] [ i32 ] [] ())] ()) () ]
                                                                     [] ())] ()) ())
  , testType "for<'l1: 'l2 + 'l3, 'l4: 'l5> fn(_: i32 + 'l1) -> i32"
             (BareFn Normal Rust
                            [ LifetimeDef [] (Lifetime (Name "l1") ()) [Lifetime (Name "l2") (), Lifetime (Name "l3") ()] ()
                            , LifetimeDef [] (Lifetime (Name "l4") ()) [Lifetime (Name "l5") ()] () ]
                            (FnDecl [Arg (ObjectSum i32 [RegionTyParamBound (Lifetime (Name "l1") ())] ()) (Just (WildP ())) ()] 
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
    -- | Create a test for a code fragment that should parse to a type.
    testType inp ty = testCase inp $ Right ty @=? parseNoSpans typeP (inputStreamFromString inp)
   
    -- Just a common type to make the tests above more straightforward
    i32 :: Ty ()
    i32 = PathTy Nothing (Path False [("i32", AngleBracketed [] [] [] ())] ()) ()

{-
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
                                               [ FieldPat Nothing (IdentP (ByValue Immutable) "x" Nothing ()) ()
                                               , FieldPat (Just "y") (IdentP (ByValue Immutable) "y1" Nothing ()) () ]
                                               False ()) 
  , testPat "Point { x, .. }"         (StructP (Path False [("Point", AngleBracketed [] [] [] ())] ())
                                               [ FieldPat Nothing (IdentP (ByValue Immutable)"x" Nothing ()) () ]
                                               True ())
  , testPat "math::PI"                (PathP Nothing (Path False [ ("math", AngleBracketed [] [] [] ())
                                                                 , ("PI", AngleBracketed [] [] [] ()) ] ()) ())
  , testPat "1...2"                   (RangeP (Lit [] (Int 1 Unsuffixed ()) ()) (Lit [] (Int 2 Unsuffixed ()) ()) ())
  , testPat "ref mut y@(x,x)"         (IdentP (ByRef Mutable) "y" (Just (TupleP [ x, x ] Nothing ())) ())
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
                                              Nothing [] ())
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


  
-- | This contains tests for parsing a variety of expressions
expressionSuite :: Test
expressionSuite = testGroup "parsing expressions" []

testParse :: (Show (f ()), Functor f, Eq (f ())) => String -> P (Spanned (f Span)) -> f () -> Test
testParse inp parser x = testCase inp $ Right x @=? parseNoSpans (unspan <$> parser) (inputStreamFromString inp)

-- | Create a test for a code fragment that should parse to an expression.
testExpr :: String -> Expr () -> Test
testExpr inp expr = testCase inp $ Right expr @=? parseNoSpans expressionP (inputStreamFromString inp)

testType :: String -> Ty () -> Test
testType inp ty = testCase inp $ Right ty @=? parseNoSpans typeP (inputStreamFromString inp)

-- | Create a test for a code fragment that should parse to a pattern.
testPat :: String -> Pat () -> Test
testPat inp pat = testCase inp $ Right pat @=? parseNoSpans patternP (inputStreamFromString inp)
-}
-- | Turn an InputStream into either an error or a parse.
parseNoSpans :: Functor f => P (f Span) -> InputStream -> Either (Position,String) (f ())
parseNoSpans parser inp = runExcept (void <$> result)
  where
    -- result :: Except (Position,String) (f Span)
    result = execParser parser inp initPos

