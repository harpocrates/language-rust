{-# LANGUAGE OverloadedStrings, OverloadedLists, UnicodeSyntax, FlexibleContexts  #-}
module ParserTest (parserSuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Parser
import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
import Language.Rust.Data.InputStream

import Control.Monad
import Control.Monad.Trans.Except

parserSuite :: Test
parserSuite = testGroup "parser suite"
                [ parserLiterals
                , parserAttributes
                , parserTypes
                , parserPatterns
                , parserExpressions
                , parserStatements
                , parserItems
                ]

-- | Create a test for a code fragment that should parse to a type.
testP inp x = testCase inp $ Right x @=? parseNoSpans parser (inputStreamFromString inp)

-- | Turn an InputStream into either an error or a parse.
parseNoSpans :: Functor f => P (f Span) -> InputStream -> Either (Position,String) (f ())
parseNoSpans parser inp = runExcept (void <$> result)
  where
    -- result :: Except (Position,String) (f Span)
    result = execParser parser inp initPos


-- | Test parsing of literals.
parserLiterals :: Test
parserLiterals = testGroup "parsing literals"
  -- bool's
  [ testP "true" (Bool True Unsuffixed ())
  , testP "false" (Bool False Unsuffixed ())
  -- byte's
  , testP "b'a'" (Byte 97 Unsuffixed ())
  , testP "b'\\n'" (Byte 10 Unsuffixed ())
  -- char's
  , testP "'a'" (Char 'a' Unsuffixed ())
  , testP "'\\n'" (Char '\n' Unsuffixed ()) 
  -- integers
  , testP "123" (Int 123 Unsuffixed ())
  , testP "123i32" (Int 123 I32 ())
  , testP "0b1100_1101" (Int 205 Unsuffixed ())
  , testP "0b1100_1101isize" (Int 205 Is ())
  , testP "0o3170" (Int 1656 Unsuffixed ())
  , testP "0o3170i64" (Int 1656 I64 ())
  , testP "0xAFAC" (Int 44972 Unsuffixed ())
  , testP "0xAFACu32" (Int 44972 U32 ())
  -- float's
  , testP "123.1" (Float 123.1 Unsuffixed ())
  , testP "123.f32" (Float 123.0 F32 ())
  , testP "123.1f32" (Float 123.1 F32 ())
  , testP "123e-9f32" (Float 123e-9 F32 ())
  -- string's
  , testP "\"hello \\n world!\"" (Str "hello \n world!" Cooked Unsuffixed ())
  , testP "r\"hello \n world!\"" (Str "hello \n world!" (Raw 0) Unsuffixed ()) 
  , testP "r##\"hello \"#\n world!\"###" (Str "hello \"#\n world!" (Raw 2) Unsuffixed ())
  -- bytestring's
  , testP "b\"hello \\n world!\"" (ByteStr "hello \n world!" Cooked Unsuffixed ())
  , testP "rb\"hello \n world!\"" (ByteStr "hello \n world!" (Raw 0) Unsuffixed ())
  , testP "rb##\"hello \"#\n world!\"###" (ByteStr "hello \"#\n world!" (Raw 2) Unsuffixed ())
  ]


-- | Test parsing of (inner and outer) attributes.
parserAttributes :: Test
parserAttributes = testGroup "parsing attributes" 
  [ testP "#[cfg]" (Attribute Outer (Word (mkIdent "cfg") ())False ())
  , testP "#![cfg]" (Attribute Inner (Word (mkIdent "cfg") ()) False ())
  , testP "#[test]" (Attribute Outer (Word (mkIdent "test") ()) False ())
  , testP "#[inline(always)]" (Attribute Outer (List (mkIdent "inline") [MetaItem (Word (mkIdent "always") ()) ()] ()) False ())
  , testP "#[inline(always,)]" (Attribute Outer (List (mkIdent "inline") [MetaItem (Word (mkIdent "always") ()) ()] ()) False ())
  , testP "#[inline()]" (Attribute Outer (List (mkIdent "inline") [] ()) False ())
  , testP "#[inline(always, sometimes)]" (Attribute Outer (List (mkIdent "inline") [MetaItem (Word (mkIdent "always") ()) (),MetaItem (Word (mkIdent "sometimes") ()) ()] ()) False ())
  , testP "#[cfg(target_os = \"macos\")]" (Attribute Outer (List (mkIdent "cfg") [MetaItem (NameValue (mkIdent "target_os") (Str "macos" Cooked Unsuffixed ()) ()) ()] ()) False ())
  , testP "#[cfg(0, tar = \"mac\")]" (Attribute Outer (List (mkIdent "cfg") [Literal (Int 0 Unsuffixed ()) (), MetaItem (NameValue (mkIdent "tar") (Str "mac" Cooked Unsuffixed ()) ()) ()] ()) False ())
  ]


-- | Test parsing of types.
parserTypes :: Test
parserTypes = testGroup "parsing types"
  [ testP "_" (Infer ())
  , testP "!" (Never ())
  , testP "i32" i32
  , testP "(i32,)" (TupTy [i32] ())
  , testP "(i32,())" (TupTy [i32, TupTy [] ()] ())
  , testP "()" (TupTy [] ())
  , testP "[_]" (Slice (Infer ()) ())
  , testP "[i32]" (Slice i32 ())
  , testP "[i32; 17]" (Array i32 (Lit [] (Int 17 Unsuffixed ()) ()) ())
  , testP "*()" (Ptr Immutable (TupTy [] ()) ())
  , testP "* i32" (Ptr Immutable i32 ())
  , testP "*const !" (Ptr Immutable (Never ()) ())
  , testP "*mut _" (Ptr Mutable (Infer ()) ())
  , testP "*mut i32" (Ptr Mutable i32 ())
  , testP "*const i32" (Ptr Immutable i32 ())
  , testP "&()" (Rptr Nothing Immutable (TupTy [] ()) ())
  , testP "&mut !" (Rptr Nothing Mutable (Never ()) ())
  , testP "&'lt ()" (Rptr (Just (Lifetime "lt" ())) Immutable (TupTy [] ()) ())
  , testP "&'lt mut !" (Rptr (Just (Lifetime "lt" ())) Mutable (Never ()) ())
  , testP "&i32" (Rptr Nothing Immutable i32 ())
  , testP "&'lt mut i32" (Rptr (Just (Lifetime "lt" ())) Mutable i32  ())
  , testP "typeof(123)" (Typeof (Lit [] (Int 123 Unsuffixed ()) ()) ())
  , testP "Vec<i32>" (PathTy Nothing (Path False [("Vec", AngleBracketed [] [i32] [] ())] ()) ())
  , testP "Vec<i32>" (PathTy Nothing (Path False [("Vec", AngleBracketed [] [i32] [] ())] ()) ())
  , testP "Vec<<i32 as a>::b,i32>" (PathTy Nothing (Path False [("Vec", AngleBracketed [] [ PathTy (Just (QSelf i32 1))
                                                                                                   (Path False [ ("a",NoParameters ())
                                                                                                               , ("b",NoParameters ())
                                                                                                               ] ()) ()
                                                                                          , i32] [] ())] ()) ())
  , testP "Vec< <i32 as a>::b,i32>" (PathTy Nothing (Path False [("Vec", AngleBracketed [] [ PathTy (Just (QSelf i32 1))
                                                                                                   (Path False [ ("a",NoParameters ())
                                                                                                               , ("b",NoParameters ())
                                                                                                               ] ()) ()
                                                                                          , i32] [] ())] ()) ())
  , testP "std::vec::Vec<T>" (PathTy Nothing (Path False [ (mkIdent "std", AngleBracketed [] [] [] ())
                                                            , (mkIdent "vec", AngleBracketed [] [] [] ())
                                                            , (mkIdent "Vec", AngleBracketed [] [PathTy Nothing (Path False [(mkIdent "T", AngleBracketed [] [] [] ())] ()) ()] [] ())
                                                            ] ()) ())
  , testP "foo::baz<'a,A,B=!>" (PathTy Nothing (Path False [ (mkIdent "foo", AngleBracketed [] [] [] ())
                                                              , (mkIdent "baz", AngleBracketed [Lifetime "a" ()] 
                                                                                               [PathTy Nothing (Path False [(mkIdent "A",AngleBracketed [] [] [] ())] ()) () ]
                                                                                               [(mkIdent "B", Never ())] ())
                                                              ] ()) ())
  , testP "Foo(!,!)" (PathTy Nothing (Path False [ (mkIdent "Foo", Parenthesized [Never (), Never ()] Nothing ()) ] ()) ())
  , testP "Foo(!,!,)" (PathTy Nothing (Path False [ (mkIdent "Foo", Parenthesized [Never (), Never ()] Nothing ()) ] ()) ())
  , testP "Foo(!,!) -> !" (PathTy Nothing (Path False [ (mkIdent "Foo", Parenthesized [Never (), Never ()] (Just (Never ())) ()) ] ()) ())
  , testP "Foo(!,!,) -> !" (PathTy Nothing (Path False [ (mkIdent "Foo", Parenthesized [Never (), Never ()] (Just (Never ())) ()) ] ()) ())
  , testP "<i32 as a>::b"
             (PathTy (Just (QSelf i32 1)) (Path False [ ("a", AngleBracketed [] [] [] ())
                                                      , ("b", AngleBracketed [] [] [] ())
                                                      ] ()) ())
  , testP "<i32 as a::b::Trait>::AssociatedItem"
             (PathTy (Just (QSelf i32 3)) (Path False [ ("a", AngleBracketed [] [] [] ())
                                                      , ("b", AngleBracketed [] [] [] ())
                                                      , ("Trait", AngleBracketed [] [] [] ())
                                                      , ("AssociatedItem", AngleBracketed [] [] [] ())
                                                      ] ()) ())
  , testP "< <i32 as a::b::Trait>::AssociatedItem as x>::Another"
             (PathTy (Just (QSelf (PathTy (Just (QSelf i32 3)) (Path False [ ("a", AngleBracketed [] [] [] ())
                                                      , ("b", AngleBracketed [] [] [] ())
                                                      , ("Trait", AngleBracketed [] [] [] ())
                                                      , ("AssociatedItem", AngleBracketed [] [] [] ())
                                                      ] ()) ()) 1))
                     (Path False [ ("x", NoParameters ())
                                 , ("Another", NoParameters ())
                                 ] ())
                     ())
  , testP "<<i32 as a::b::Trait>::AssociatedItem as x>::Another"
             (PathTy (Just (QSelf (PathTy (Just (QSelf i32 3)) (Path False [ ("a", AngleBracketed [] [] [] ())
                                                      , ("b", AngleBracketed [] [] [] ())
                                                      , ("Trait", AngleBracketed [] [] [] ())
                                                      , ("AssociatedItem", AngleBracketed [] [] [] ())
                                                      ] ()) ()) 1))
                     (Path False [ ("x", NoParameters ())
                                 , ("Another", NoParameters ())
                                 ] ())
                     ())
  , testP "< <i32 + 'static as a::b::Trait>::AssociatedItem as x>::Another"
             (PathTy (Just (QSelf (PathTy (Just (QSelf (ObjectSum i32 [RegionTyParamBound (Lifetime "static" ())] ()) 3)) 
                                                (Path False [ ("a", AngleBracketed [] [] [] ())
                                                      , ("b", AngleBracketed [] [] [] ())
                                                      , ("Trait", AngleBracketed [] [] [] ())
                                                      , ("AssociatedItem", AngleBracketed [] [] [] ())
                                                      ] ()) ()) 1))
                     (Path False [ ("x", NoParameters ())
                                 , ("Another", NoParameters ())
                                 ] ())
                     ())
  , testP "<<i32 + 'static as a::b::Trait>::AssociatedItem as x>::Another"
             (PathTy (Just (QSelf (PathTy (Just (QSelf (ObjectSum i32 [RegionTyParamBound (Lifetime "static" ())] ()) 3)) 
                                                (Path False [ ("a", AngleBracketed [] [] [] ())
                                                      , ("b", AngleBracketed [] [] [] ())
                                                      , ("Trait", AngleBracketed [] [] [] ())
                                                      , ("AssociatedItem", AngleBracketed [] [] [] ())
                                                      ] ()) ()) 1))
                     (Path False [ ("x", NoParameters ())
                                 , ("Another", NoParameters ())
                                 ] ())
                     ())
  , testP "<i32 as a(i32, i32)::b<'lt>::Trait(i32) -> i32>::AssociatedItem"
             (PathTy (Just (QSelf i32 3)) (Path False [ ("a", Parenthesized [i32, i32] Nothing ())
                                                      , ("b", AngleBracketed [Lifetime "lt" ()] [] [] ())
                                                      , ("Trait", Parenthesized [i32] (Just i32) ())
                                                      , ("AssociatedItem", AngleBracketed [] [] [] ())
                                                      ] ()) ())
  , testP "fn(i32,...)"
             (BareFn Normal Rust [] (FnDecl [Arg Nothing i32 ()] Nothing True ()) ())
  , testP "fn(i32) -> i32"
             (BareFn Normal Rust [] (FnDecl [Arg Nothing i32 ()] (Just i32) False ()) ())
  , testP "fn(i32,) -> i32"
             (BareFn Normal Rust [] (FnDecl [Arg Nothing i32 ()] (Just i32) False ()) ())
  , testP "fn(_: i32) -> i32"
             (BareFn Normal Rust [] (FnDecl [Arg (Just (WildP ())) i32 ()] (Just i32) False ()) ())
  , testP "unsafe extern \"C\" fn(_: i32)"
             (BareFn Unsafe C [] (FnDecl [Arg (Just (WildP ())) i32 ()] Nothing False ()) ())
  , testP "PResult<'a, P<i32>>"
             (PathTy Nothing (Path False [("PResult", AngleBracketed [ Lifetime "a" () ]
                                                                     [ PathTy Nothing (Path False [("P", AngleBracketed [] [ i32 ] [] ())] ()) () ]
                                                                     [] ())] ()) ())
  , testP "for<'l1: 'l2 + 'l3, 'l4: 'l5> fn(_: i32 + 'l1) -> i32"
             (BareFn Normal Rust
                            [ LifetimeDef [] (Lifetime "l1" ()) [Lifetime "l2" (), Lifetime "l3" ()] ()
                            , LifetimeDef [] (Lifetime "l4" ()) [Lifetime "l5" ()] () ]
                            (FnDecl [Arg (Just (WildP ())) (ObjectSum i32 [RegionTyParamBound (Lifetime "l1" ())] ()) ()] 
                                    (Just i32) False ()) ())
  , testP "for <'a> Foo<&'a T>"
             (PolyTraitRefTy
                [TraitTyParamBound
                   (PolyTraitRef [LifetimeDef [] (Lifetime "a" ()) [] ()]
                                 (TraitRef (Path False [("Foo", AngleBracketed [] [Rptr (Just (Lifetime "a" ()))
                                                                                        Immutable
                                                                                        (PathTy Nothing (Path False [("T", AngleBracketed [] [] [] ())] ()) ())
                                                                                        ()]
                                                                                  [] ())] ()) ()) ()) None] ())
  --, testP "for <'a,> Debug + for <'b> Clone + for <'c> Clone"
  --           (PolyTraitRefTy
  --             [TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime (Name "a") ()) [] ()]
  --                                              (TraitRef (Path False [("Debug", AngleBracketed [] [] [] ())] ()) ()) ()) None] ())
  , testP "&for<'a> Tr<'a> + Send"
           (ObjectSum
              (Rptr Nothing Immutable (PolyTraitRefTy [TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime "a" ()) [] ()] (TraitRef (Path False [("Tr",AngleBracketed [Lifetime "a" ()] [] [] ())] ()) ()) ()) None] ()) ())
              [TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [("Send",AngleBracketed [] [] [] ())] ()) ()) ()) None]
              ()) 
  , testP "&(for<'a> Tr<'a> + Send)"
           (Rptr Nothing Immutable (ParenTy (ObjectSum
              (PolyTraitRefTy [TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime  "a" ()) [] ()] (TraitRef (Path False [("Tr",AngleBracketed [Lifetime "a" ()] [] [] ())] ()) ()) ()) None] ())
              [TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [("Send",AngleBracketed [] [] [] ())] ()) ()) ()) None]
              ()) ()) ()) 
  , testP "Fn() -> &(Object+Send)"
           (PathTy Nothing (Path False [("Fn", Parenthesized [] (Just (Rptr Nothing Immutable (ParenTy (ObjectSum (PathTy Nothing (Path False [("Object",AngleBracketed [] [] [] ())] ()) ()) [TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [("Send",AngleBracketed [] [] [] ())] ()) ()) ()) None] ()) ()) ())) ())] ()) ())
  ]


-- | Test parsing of patterns.
parserPatterns :: Test
parserPatterns = testGroup "parsing patterns"
  [ testP "_"                       (WildP ())
  , testP "x"                       x
  , testP "ref mut x"               (IdentP (ByRef Mutable) "x" Nothing ())
  , testP "&x"                      (RefP x Immutable ())
  , testP "&mut x"                  (RefP x Mutable ())
  , testP "(x,)"                    (TupleP [ x ] Nothing ())
  , testP "(..)"                    (TupleP [] (Just 0) ())
  , testP "(x, x)"                  (TupleP [ x, x ] Nothing ())
  , testP "(x,.., x)"               (TupleP [ x, x ] (Just 1) ())
  , testP "(..,x)"                  (TupleP [ x ] (Just 0) ())
  , testP "(..,x,)"                 (TupleP [ x ] (Just 0) ())
  , testP "(x,..)"                  (TupleP [ x ] (Just 1) ())
  , testP "(x,x,)"                  (TupleP [ x, x ] Nothing ())
  , testP "(x, ref mut y, box z)"   (TupleP [ x
                                              , IdentP (ByRef Mutable) "y" Nothing ()
                                              , BoxP (IdentP (ByValue Immutable) "z" Nothing ()) ()
                                              ] 
                                              Nothing ())
  , testP "true"                    (LitP (Lit [] (Bool True Unsuffixed ()) ()) ())
  , testP "-123"                    (LitP (Unary [] Neg (Lit [] (Int 123 Unsuffixed ()) ()) ()) ())
  , testP "Point { .. }"            (StructP (Path False [("Point", AngleBracketed [] [] [] ())] ()) [] True ())
  , testP "Point { x, y: y1 }"      (StructP (Path False [("Point", AngleBracketed [] [] [] ())] ())
                                               [ FieldPat Nothing (IdentP (ByValue Immutable) "x" Nothing ()) ()
                                               , FieldPat (Just "y") (IdentP (ByValue Immutable) "y1" Nothing ()) () ]
                                               False ()) 
  , testP "Point { x, .. }"         (StructP (Path False [("Point", AngleBracketed [] [] [] ())] ())
                                               [ FieldPat Nothing (IdentP (ByValue Immutable) "x" Nothing ()) () ]
                                               True ())
  , testP "math"                    (IdentP (ByValue Immutable) "math" Nothing ())
  , testP "math::PI"                (PathP Nothing (Path False [ ("math", AngleBracketed [] [] [] ())
                                                                 , ("PI", AngleBracketed [] [] [] ()) ] ()) ())
  , testP "math::<i32>"             (PathP Nothing (Path False [ ("math", AngleBracketed [] [i32] [] ()) ] ()) ())
  , testP "math::<i32>::PI"         (PathP Nothing (Path False [ ("math", AngleBracketed [] [i32] [] ())
                                                                 , ("PI", AngleBracketed [] [] [] ()) ] ()) ())
  , testP "1...2"                   (RangeP (Lit [] (Int 1 Unsuffixed ()) ()) (Lit [] (Int 2 Unsuffixed ()) ()) ())
  , testP "ref mut y@(x,x)"         (IdentP (ByRef Mutable) "y" (Just (TupleP [ x, x ] Nothing ())) ())
  , testP "ref mut y@_"         (IdentP (ByRef Mutable) "y" (Just (WildP ())) ())
  , testP "(1,2,..,3)"              (TupleP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int 2 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ]
                                              (Just 2) ())
  , testP "Point(x)"                (TupleStructP (Path False [("Point", AngleBracketed [] [] [] ())] ())
                                              [ x ] Nothing ())
  , testP "Point(x,)"               (TupleStructP (Path False [("Point", AngleBracketed [] [] [] ())] ())
                                              [ x ] Nothing ())
  , testP "<i32 as a(i32, i32)>::b::<'lt>::AssociatedItem"
            (PathP (Just (QSelf i32 1)) (Path False [ ("a", Parenthesized [i32, i32] Nothing ())
                                                    , ("b", AngleBracketed [Lifetime "lt" ()] [] [] ())
                                                    , ("AssociatedItem", AngleBracketed [] [] [] ())
                                                    ] ()) ())
  , testP "[1,2]"                   (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int 2 Unsuffixed ()) ()) () ] 
                                              Nothing [] ())
  , testP "[1,2,]"                  (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int 2 Unsuffixed ()) ()) () ] 
                                              Nothing [] ())
  , testP "[1,..,3]"                (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ]
                                              (Just (WildP ()))
                                              [ LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ] ())
  , testP "[1,..,3,]"               (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ]
                                              (Just (WildP ()))
                                              [ LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ] ())
  , testP "[1,x..,3]"               (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ]
                                              (Just x)
                                              [ LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ] ())
  , testP "[1,..]"                  (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ] (Just (WildP ())) [] ())
  , testP "[1,x..]"                 (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ] (Just x) [] ())
  , testP "[x]"                     (SliceP [ x ] Nothing [] ())
  , testP "[x,]"                    (SliceP [ x ] Nothing [] ())
  , testP "[x..]"                   (SliceP [] (Just x) [] ())
  , testP "[..]"                    (SliceP [] (Just (WildP ())) [] ())
  ]

  
-- | Test parsing of expressions.
parserExpressions :: Test
parserExpressions = testGroup "parsing expressions"
  [ testP "123" (Lit [] (Int 123 Unsuffixed ()) ())
  , testP "()" (TupExpr [] [] ()) 
  , testP "(1,)" (TupExpr [] [Lit [] (Int 1 Unsuffixed ()) ()] ())
  , testP "(1,2)" (TupExpr [] [Lit [] (Int 1 Unsuffixed ()) (), Lit [] (Int 2 Unsuffixed ()) ()] ())
  , testP "|| 1" (Closure [] Ref (FnDecl [] Nothing False ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "|_: ()| 1" (Closure [] Ref (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] Nothing False ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "|_| 1" (Closure [] Ref (FnDecl [Arg (Just (WildP ())) (Infer ()) ()] Nothing False ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "|_: ()| -> () { (); }" (Closure [] Ref (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] (Just (TupTy [] ())) False ()) (BlockExpr [] (Block [Semi (TupExpr [] [] ()) ()] DefaultBlock ()) ()) ())
  , testP "move || 1" (Closure [] Value (FnDecl [] Nothing False ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "move |_: ()| 1" (Closure [] Value (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] Nothing False ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "move |_| 1" (Closure [] Value (FnDecl [Arg (Just (WildP ())) (Infer ()) ()] Nothing False ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "move |_: ()| -> () { (); }" (Closure [] Value (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] (Just (TupTy [] ())) False ()) (BlockExpr [] (Block [Semi (TupExpr [] [] ()) ()] DefaultBlock ()) ()) ())
  , testP "|_: ()| -> () { () }" (Closure [] Ref (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] (Just (TupTy [] ())) False ()) (BlockExpr [] (Block [NoSemi (TupExpr [] [] ()) ()] DefaultBlock ()) ()) ())
  , testP "move |_: ()| -> () { () }" (Closure [] Value (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] (Just (TupTy [] ())) False ()) (BlockExpr [] (Block [NoSemi (TupExpr [] [] ()) ()] DefaultBlock ()) ()) ())
  , testP "[(); 512]" (Repeat [] (TupExpr [] [] ()) (Lit [] (Int 512 Unsuffixed ()) ()) ())
  , testP "[]" (Vec [] [] ())
  , testP "[1]" (Vec [] [Lit [] (Int 1 Unsuffixed ()) ()] ())
  , testP "[1,]" (Vec [] [Lit [] (Int 1 Unsuffixed ()) ()] ())
  , testP "[1,2]" (Vec [] [Lit [] (Int 1 Unsuffixed ()) (), Lit [] (Int 2 Unsuffixed ()) ()] ())
  , testP "{ 1; 2 }" (BlockExpr [] (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) (), NoSemi (Lit [] (Int 2 Unsuffixed ()) ()) ()] DefaultBlock ()) ())
  , testP "unsafe { 1; 2 }" (BlockExpr [] (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) (), NoSemi (Lit [] (Int 2 Unsuffixed ()) ()) ()] (UnsafeBlock False) ()) ())
  , testP "{ 1; 2; }" (BlockExpr [] (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) (), Semi (Lit [] (Int 2 Unsuffixed ()) ()) ()] DefaultBlock ()) ())
  , testP "unsafe { 1; 2; }" (BlockExpr [] (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) (), Semi (Lit [] (Int 2 Unsuffixed ()) ()) ()] (UnsafeBlock False) ()) ())
  , testP "return" (Ret [] Nothing ())
  , testP "return 1" (Ret [] (Just (Lit [] (Int 1 Unsuffixed ()) ())) ())
  , testP "continue" (Continue [] Nothing ())
  , testP "break" (Break [] Nothing ())
  , testP "continue 'lbl" (Continue [] (Just (Lifetime "lbl" ())) ())
  , testP "break 'lbl" (Break [] (Just (Lifetime "lbl" ())) ())
  , testP "math" (PathExpr [] Nothing (Path False [ ("math", NoParameters ()) ] ()) ())
  , testP "math::PI" (PathExpr [] Nothing (Path False [ ("math", NoParameters ())
                                                      , ("PI", NoParameters ()) ] ()) ())
  , testP "math::<i32>" (PathExpr [] Nothing (Path False [ ("math", AngleBracketed [] [i32] [] ()) ] ()) ())
  , testP "math::<i32>::PI" (PathExpr [] Nothing (Path False [ ("math", AngleBracketed [] [i32] [] ())
                                                             , ("PI", AngleBracketed [] [] [] ()) ] ()) ())
  , testP "<i32 as a(i32, i32)>::b::<'lt>::AssociatedItem"
            (PathExpr [] (Just (QSelf i32 1)) (Path False [ ("a", Parenthesized [i32, i32] Nothing ())
                                                          , ("b", AngleBracketed [Lifetime "lt" ()] [] [] ())
                                                          , ("AssociatedItem", NoParameters ())
                                                          ] ()) ())
  , testP "Point { x: 1, y: 2 }" (Struct [] (Path False [("Point",NoParameters ())] ()) [Field "x" (Lit [] (Int 1 Unsuffixed ()) ()) (), Field "y" (Lit [] (Int 2 Unsuffixed ()) ()) ()] Nothing ())
  , testP "Point { x: 1, y: 2, }" (Struct [] (Path False [("Point",NoParameters ())] ()) [Field "x" (Lit [] (Int 1 Unsuffixed ()) ()) (), Field "y" (Lit [] (Int 2 Unsuffixed ()) ()) ()] Nothing ())
  , testP "Point { x: 1, ..p }" (Struct [] (Path False [("Point",NoParameters ())] ()) [Field "x" (Lit [] (Int 1 Unsuffixed ()) ()) ()] (Just (PathExpr [] Nothing (Path False [ ("p", NoParameters ()) ] ()) ())) ())
  , testP "Point { ..p }" (Struct [] (Path False [("Point",NoParameters ())] ()) [] (Just (PathExpr [] Nothing (Path False [ ("p", NoParameters ()) ] ()) ())) ())
  , testP "Point { }" (Struct [] (Path False [("Point",NoParameters ())] ()) [] Nothing ())
  , testP "if true { 1; }"
            (If [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) 
                Nothing ())
  , testP "if true { 1; } else { 2; }"
            (If [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) 
                (Just (BlockExpr [] (Block [Semi (Lit [] (Int 2 Unsuffixed ()) ()) ()] DefaultBlock ()) ())) ())
  , testP "if true { 1; } else if false { 2; }"
            (If [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) 
                (Just (If [] (Lit [] (Bool False Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 2 Unsuffixed ()) ()) ()] DefaultBlock ())
                    Nothing ())) ())
  , testP "if true { 1; } else if false { 2; } else { 3; }"
            (If [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) 
                (Just (If [] (Lit [] (Bool False Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 2 Unsuffixed ()) ()) ()] DefaultBlock ())
                    (Just (BlockExpr [] (Block [Semi (Lit [] (Int 3 Unsuffixed ()) ()) ()] DefaultBlock ()) ())) ())) ())
  , testP "if let x = true { 1; }"
            (IfLet [] x (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) 
                Nothing ())
  , testP "if let x = true { 1; } else { 2; }"
            (IfLet [] x (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) 
                (Just (BlockExpr [] (Block [Semi (Lit [] (Int 2 Unsuffixed ()) ()) ()] DefaultBlock ()) ())) ())
  , testP "if true { 1; } else if let x = false { 2; }"
            (If [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) 
                (Just (IfLet [] x (Lit [] (Bool False Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 2 Unsuffixed ()) ()) ()] DefaultBlock ())
                    Nothing ())) ())
  , testP "loop { 1; }" (Loop [] (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) Nothing ())
  , testP "'lbl: loop { 1; }" (Loop [] (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) (Just (Lifetime "lbl" ())) ())
  , testP "for x in [1,2,3] { 1; }" (ForLoop [] x (Vec [] [Lit [] (Int 1 Unsuffixed ()) (), Lit [] (Int 2 Unsuffixed ()) (), Lit [] (Int 3 Unsuffixed ()) ()] ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) Nothing ()) 
  , testP "'lbl: for x in [1,2,3] { 1; }" (ForLoop [] x (Vec [] [Lit [] (Int 1 Unsuffixed ()) (), Lit [] (Int 2 Unsuffixed ()) (), Lit [] (Int 3 Unsuffixed ()) ()] ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) (Just (Lifetime "lbl" ())) ()) 
  , testP "while true { 1; }" (While [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) Nothing ())
  , testP "'lbl: while true { 1; }" (While [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) (Just (Lifetime "lbl" ())) ())
  , testP "while let x = true { 1; }" (WhileLet [] x (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) Nothing ())
  , testP "'lbl: while let x = true { 1; }" (WhileLet [] x (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) (Just (Lifetime "lbl" ())) ())
  , testP "match true { }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [] ())
  , testP "match true { _ => 2 }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int 2 Unsuffixed ()) ()) ()] ())
  , testP "match true { _ if true => 2 }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] (Just (Lit [] (Bool True Unsuffixed ()) ())) (Lit [] (Int 2 Unsuffixed ()) ()) ()] ())
  , testP "match true { _ => 2, }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int 2 Unsuffixed ()) ()) ()] ())
  , testP "match true { _ => 2, x | x => 1 }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int 2 Unsuffixed ()) ()) (), Arm [] [x,x] Nothing (Lit [] (Int 1 Unsuffixed ()) ()) ()] ())
  , testP "match true { _ => 2, x | x => { 1; } }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int 2 Unsuffixed ()) ()) (), Arm [] [x,x] Nothing (BlockExpr [] (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) ()) ()] ())
  , testP "match true { _ => 2, x | x => { 1; }, }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int 2 Unsuffixed ()) ()) (), Arm [] [x,x] Nothing (BlockExpr [] (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) ()) ()] ())
  , testP "match true { _ => 2, x | x => { 1; }, _ => 1 }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int 2 Unsuffixed ()) ()) (), Arm [] [x,x] Nothing (BlockExpr [] (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) ()) (), Arm [] [WildP ()] Nothing (Lit [] (Int 1 Unsuffixed ()) ()) ()] ())
  , testP "match true { _ => 2, x | x => { 1; } _ => 1 }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int 2 Unsuffixed ()) ()) (), Arm [] [x,x] Nothing (BlockExpr [] (Block [Semi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) ()) (), Arm [] [WildP ()] Nothing (Lit [] (Int 1 Unsuffixed ()) ()) ()] ())
  , testP "println!()" (MacExpr [] (Mac (Path False [("println",NoParameters ())] ()) [] ()) ()) 
  , testP "1..2" (Range [] (Just (Lit [] (Int 1 Unsuffixed ()) ())) (Just (Lit [] (Int 2 Unsuffixed ()) ())) Closed ())
  , testP "1...2" (Range [] (Just (Lit [] (Int 1 Unsuffixed ()) ())) (Just (Lit [] (Int 2 Unsuffixed ()) ())) HalfOpen ())
  , testP "..2" (Range [] Nothing (Just (Lit [] (Int 2 Unsuffixed ()) ())) Closed ())
  , testP "...2" (Range [] Nothing (Just (Lit [] (Int 2 Unsuffixed ()) ())) HalfOpen ())
  , testP ".." (Range [] Nothing Nothing Closed ())
  , testP "..." (Range [] Nothing Nothing HalfOpen ())
  , testP "x?" (Try [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) ())
  , testP "x.0" (TupField [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) 0 ())
  , testP "x.foo" (FieldAccess [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) "foo" ())
  , testP "x.foo()" (MethodCall [] "foo" [] [PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()] ())
  , testP "x.foo(1)" (MethodCall [] "foo" [] [PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) (), Lit [] (Int 1 Unsuffixed ()) ()] ())
  , testP "x.foo(1,)" (MethodCall [] "foo" [] [PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) (), Lit [] (Int 1 Unsuffixed ()) ()] ())
  , testP "x.foo::<i32>()" (MethodCall [] "foo" [i32] [PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()] ())
  , testP "x.foo::<i32>(1)" (MethodCall [] "foo" [i32] [PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) (), Lit [] (Int 1 Unsuffixed ()) ()] ())
  , testP "x.foo::<i32>(1,)" (MethodCall [] "foo" [i32] [PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) (), Lit [] (Int 1 Unsuffixed ()) ()] ())
  , testP "x[1]" (Index [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x()" (Call [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) [] ())
  , testP "x(1)" (Call [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) [Lit [] (Int 1 Unsuffixed ()) ()] ())
  , testP "x(1,)" (Call [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) [Lit [] (Int 1 Unsuffixed ()) ()] ())
  , testP "x(1,2)" (Call [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) [Lit [] (Int 1 Unsuffixed ()) (), Lit [] (Int 2 Unsuffixed ()) ()] ())
  , testP "x(1,2,)" (Call [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) [Lit [] (Int 1 Unsuffixed ()) (), Lit [] (Int 2 Unsuffixed ()) ()] ())
  , testP "x[1][1]" (Index [] (Index [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "&x?" (AddrOf [] Immutable (Try [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) ()) ())
  , testP "&x" (AddrOf [] Immutable (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) ())
  , testP "&mut x" (AddrOf [] Mutable (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) ())
  , testP "*x" (Unary [] Deref (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) ())
  , testP "!x" (Unary [] Not (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) ())
  , testP "-x" (Unary [] Neg (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) ())
  , testP "box x" (Box [] (PathExpr [] Nothing (Path False [("x", NoParameters ())] ()) ()) ())
  , testP "1 : i32" (TypeAscription [] (Lit [] (Int 1 Unsuffixed ()) ()) i32 ())
  , testP "1 : i32 : i32" (TypeAscription [] (TypeAscription [] (Lit [] (Int 1 Unsuffixed ()) ()) i32 ()) i32 ())
  , testP "1 as i32" (Cast [] (Lit [] (Int 1 Unsuffixed ()) ()) i32 ())
  , testP "1 as i32 : i32" (TypeAscription [] (Cast [] (Lit [] (Int 1 Unsuffixed ()) ()) i32 ()) i32 ())
  , testP "1 : i32 as i32" (Cast [] (TypeAscription [] (Lit [] (Int 1 Unsuffixed ()) ()) i32 ()) i32 ())
  , testP "1 as i32 as i32" (Cast [] (Cast [] (Lit [] (Int 1 Unsuffixed ()) ()) i32 ()) i32 ())
  , testP "!1 as i32" (Cast [] (Unary [] Not (Lit [] (Int 1 Unsuffixed ()) ()) ()) i32 ())
  , testP "!(1 as i32)" (Unary [] Not (ParenExpr [] (Cast [] (Lit [] (Int 1 Unsuffixed ()) ()) i32 ()) ()) ())
  , testP "x = 1" (Assign [] (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x == 1" (Binary [] EqOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x < 1" (Binary [] LtOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x > 1" (Binary [] GtOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x <= 1" (Binary [] LeOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x >= 1" (Binary [] GeOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x >>= 1" (AssignOp [] ShrOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x <<= 1" (AssignOp [] ShlOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x -= 1" (AssignOp [] SubOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x += 1" (AssignOp [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x *= 1" (AssignOp [] MulOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x /= 1" (AssignOp [] DivOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x ^= 1" (AssignOp [] BitXorOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x |= 1" (AssignOp [] BitOrOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x &= 1" (AssignOp [] BitAndOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x %= 1" (AssignOp [] RemOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x && 1" (Binary [] AndOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x || 1" (Binary [] OrOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x + 1" (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x - 1" (Binary [] SubOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x * 1" (Binary [] MulOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x / 1" (Binary [] DivOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x % 1" (Binary [] RemOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x & 1" (Binary [] BitAndOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x | 1" (Binary [] BitOrOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x ^ 1" (Binary [] BitXorOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x << 1" (Binary [] ShlOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "x >> 1" (Binary [] ShrOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())
  , testP "&&&x&&&y" (Binary [] AndOp (AddrOf [] Immutable (AddrOf [] Immutable (AddrOf [] Immutable (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) ()) ()) ()) (AddrOf [] Immutable (PathExpr [] Nothing (Path False [(mkIdent "y", NoParameters ())] ()) ()) ()) ())
  , testP "&[]" (AddrOf [] Immutable (Vec [] [] ()) ()) 
  , testP "for _ in 1..2 { }" (ForLoop [] (WildP ()) (Range [] (Just (Lit [] (Int 1 Unsuffixed ()) ())) (Just (Lit [] (Int 2 Unsuffixed ()) ())) Closed ()) (Block [] DefaultBlock ()) Nothing ())
  , testP "for _ in 1..x { }" (ForLoop [] (WildP ()) (Range [] (Just (Lit [] (Int 1 Unsuffixed ()) ())) (Just (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ())) Closed ()) (Block [] DefaultBlock ()) Nothing ())
  , testP "for _ in 1.. { }" (ForLoop [] (WildP ()) (Range [] (Just (Lit [] (Int 1 Unsuffixed ()) ())) Nothing Closed ()) (Block [] DefaultBlock ()) Nothing ())
  , testP "1 * 1 + 1 * 1" (Binary [] AddOp (Binary [] MulOp (Lit [] (Int 1 Unsuffixed ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ()) (Binary [] MulOp (Lit [] (Int 1 Unsuffixed ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ()) ())
  , testP "1 * - 1 + 1 * 1" (Binary [] AddOp (Binary [] MulOp (Lit [] (Int 1 Unsuffixed ()) ()) (Unary [] Neg (Lit [] (Int 1 Unsuffixed ()) ()) ()) ()) (Binary [] MulOp (Lit [] (Int 1 Unsuffixed ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ()) ())
  , testP "match true { _ => match true { _ => true }, _ => match true { _ => true } }" (Match [] (Lit [] (Bool True Unsuffixed ()) ())
      [ Arm [] [WildP ()] Nothing (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Bool True Unsuffixed ()) ()) ()] ()) ()
      , Arm [] [WildP ()] Nothing (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Bool True Unsuffixed ()) ()) ()] ()) () ] ())
  ]


-- | Test parsing of statements.
parserStatements :: Test
parserStatements = testGroup "parsing statements"
  [ testP "let x: i32 = 1;" (Local x (Just i32) (Just (Lit [] (Int 1 Unsuffixed ()) ())) [] ()) 
  , testP "let x: i32;"     (Local x (Just i32) Nothing                                  [] ())
  , testP "let x = 1;"      (Local x Nothing    (Just (Lit [] (Int 1 Unsuffixed ()) ())) [] ())
  , testP "let x;"          (Local x Nothing    Nothing                                  [] ())
  , testP "x + 1;" (Semi (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ()) ())
  , testP "x + { 1 };" (Semi (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (BlockExpr [] (Block [NoSemi (Lit [] (Int 1 Unsuffixed ()) ()) ()] DefaultBlock ()) ()) ()) ())
  , testP "match true { }" (NoSemi (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [] ()) ())
  , testP "static foo: i32 = 1;" (ItemStmt (Item "foo" [] (Static i32 Immutable (Lit [] (Int 1 Unsuffixed ()) ())) InheritedV ()) ())
  ]


-- | Test parsing of items.
parserItems :: Test
parserItems = testGroup "parsing items"
  [ testP "static foo: i32 = 1;" (Item "foo" [] (Static i32 Immutable (Lit [] (Int 1 Unsuffixed ()) ())) InheritedV ())
  , testP "static mut foo: i32 = 1;" (Item "foo" [] (Static i32 Mutable (Lit [] (Int 1 Unsuffixed ()) ())) InheritedV ())
  , testP "const foo: i32 = 1;" (Item "foo" [] (ConstItem i32 (Lit [] (Int 1 Unsuffixed ()) ())) InheritedV ())
  , testP "type Foo = i32;" (Item "Foo" [] (TyAlias i32 (Generics [] [] (WhereClause [] ()) ())) InheritedV ()) 
  , testP "type Foo<> = i32;" (Item "Foo" [] (TyAlias i32 (Generics [] [] (WhereClause [] ()) ())) InheritedV ()) 
  , testP "extern crate foo;" (Item "foo" [] (ExternCrate Nothing) InheritedV ())
  , testP "extern crate foo as bar;" (Item "foo" [] (ExternCrate (Just "bar")) InheritedV ()) 
  , testP "mod foo;" (Item "foo" [] (Mod []) InheritedV ())
  , testP "struct Point;" (Item "Point" [] (StructItem (StructD [] ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ())
  , testP "struct Point { }" (Item "Point" [] (StructItem (StructD [] ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ())
  , testP "struct Point { x: i32, y: i32 }" (Item "Point" [] (StructItem (StructD [StructField (Just "x") InheritedV i32 [] (), StructField (Just "y") InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ())
  , testP "struct Point { x: i32, y: i32, }" (Item "Point" [] (StructItem (StructD [StructField (Just "x") InheritedV i32 [] (), StructField (Just "y") InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ())
  , testP "union Either { }" (Item "Either" [] (Union (StructD [] ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ())
  , testP "union Either { x: i32, y: i32 }" (Item "Either" [] (Union (StructD [StructField (Just "x") InheritedV i32 [] (), StructField (Just "y") InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ())
  , testP "union Either { x: i32, y: i32, }" (Item "Either" [] (Union (StructD [StructField (Just "x") InheritedV i32 [] (), StructField (Just "y") InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ())
  , testP "use std::math;" (Item "" [] (Use (ViewPathSimple False ["std"] (PathListItem "math" Nothing ()) ())) InheritedV ())
  , testP "use std::math as m;" (Item "" [] (Use (ViewPathSimple False ["std"] (PathListItem "math" (Just "m") ()) ())) InheritedV ())
  , testP "use std::math::*;" (Item "" [] (Use (ViewPathGlob False ["std","math"] ())) InheritedV ())
  , testP "use std::math::{};" (Item "" [] (Use (ViewPathList False ["std","math"] [] ())) InheritedV ()) 
  , testP "use std::math::{sqrt, pi as p};" (Item "" [] (Use (ViewPathList False ["std","math"] [PathListItem "sqrt" Nothing (), PathListItem "pi" (Just "p") ()] ())) InheritedV ()) 
  , testP "use std::math::{sqrt};" (Item "" [] (Use (ViewPathList False ["std","math"] [PathListItem "sqrt" Nothing ()] ())) InheritedV ()) 
  , testP "use std::math::{sqrt, pi,};" (Item "" [] (Use (ViewPathList False ["std","math"] [PathListItem "sqrt" Nothing (), PathListItem "pi" Nothing ()] ())) InheritedV ()) 
  , testP "extern crate foo;" (Item "foo" [] (ExternCrate Nothing) InheritedV ())
  , testP "extern crate foo as bar;" (Item "foo" [] (ExternCrate (Just "bar")) InheritedV ())
  , testP "const unsafe fn foo(x: i32) -> i32 { return x + 1 }" (Item "foo" [] (Fn (FnDecl [Arg (Just x) i32 ()] (Just i32) False ()) Unsafe Const Rust (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())) ()) ()] DefaultBlock ()))  InheritedV ())
  , testP "const fn foo<>(x: i32) -> i32 { return x + 1 }" (Item "foo" [] (Fn (FnDecl [Arg (Just x) i32 ()] (Just i32) False ()) Normal Const Rust (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())) ()) ()] DefaultBlock ()))  InheritedV ())
  , testP "unsafe extern fn foo(x: i32) -> i32 { return x + 1 }" (Item "foo" [] (Fn (FnDecl [Arg (Just x) i32 ()] (Just i32) False ()) Unsafe NotConst C (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())) ()) ()] DefaultBlock ()))  InheritedV ())
  , testP "unsafe extern \"Win64\" fn foo(x: i32) -> i32 { return x + 1 }" (Item "foo" [] (Fn (FnDecl [Arg (Just x) i32 ()] (Just i32) False ()) Unsafe NotConst Win64 (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())) ()) ()] DefaultBlock ()))  InheritedV ())
  , testP "extern \"Win64\" fn foo(x: i32) -> i32 { return x + 1 }" (Item "foo" [] (Fn (FnDecl [Arg (Just x) i32 ()] (Just i32) False ()) Normal NotConst Win64 (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())) ()) ()] DefaultBlock ()))  InheritedV ())
  , testP "fn foo(x: i32) -> i32 { return x + 1 }" (Item "foo" [] (Fn (FnDecl [Arg (Just x) i32 ()] (Just i32) False ()) Normal NotConst Rust (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())) ()) ()] DefaultBlock ()))  InheritedV ())
  , testP "mod foo { }" (Item "foo" [] (Mod []) InheritedV ()) 
  , testP "mod foo { pub fn foo(x: i32) -> i32 { return x + 1 } }" (Item "foo" [] (Mod [Item "foo" [] (Fn (FnDecl [Arg (Just x) i32 ()] (Just i32) False ()) Normal NotConst Rust (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())) ()) ()] DefaultBlock ())) PublicV ()]) InheritedV ())
  , testP "extern { }" (Item (mkIdent "") [] (ForeignMod C []) InheritedV ())
  , testP "extern \"Win64\" { pub static x: i32; static mut y: i32; }" (Item (mkIdent "") [] (ForeignMod Win64 [ForeignItem "x" [] (ForeignStatic i32 False) PublicV (), ForeignItem "y" [] (ForeignStatic i32 True) InheritedV ()]) InheritedV ())
  , testP "enum Option<T> { None, Some(T) }" (Item "Option" [] (Enum [ Variant "None" [] (UnitD ()) Nothing ()
                                                                     , Variant "Some" [] (TupleD [ StructField Nothing InheritedV (PathTy Nothing (Path False [("T", NoParameters ())] ()) ()) [] ()] ()) Nothing () ]
                                                                     (Generics [] [TyParam [] "T" [] Nothing ()] (WhereClause [] ()) ())) InheritedV ())  
  , testP "impl [i32] { }" (Item "" [] (Impl Normal Positive (Generics [] [] (WhereClause [] ()) ()) Nothing (Slice i32 ()) []) InheritedV ())
  , testP "unsafe impl i32 { }" (Item "" [] (Impl Unsafe Positive (Generics [] [] (WhereClause [] ()) ()) Nothing i32 []) InheritedV ())
  , testP "impl (<i32 as a>::b) { }" (Item "" [] (Impl Normal Positive (Generics [] [] (WhereClause [] ()) ()) Nothing (PathTy (Just (QSelf i32 1)) (Path False [("a", AngleBracketed [] [] [] ()), ("b", AngleBracketed [] [] [] ()) ] ()) ()) []) InheritedV ())
  , testP "impl (<i32 as a>::b) { }" (Item "" [] (Impl Normal Positive (Generics [] [] (WhereClause [] ()) ()) Nothing (PathTy (Just (QSelf i32 1)) (Path False [("a", AngleBracketed [] [] [] ()), ("b", AngleBracketed [] [] [] ()) ] ()) ()) []) InheritedV ())
  , testP "impl !Debug for i32 { }" (Item "" [] (Impl Normal Negative (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) i32 []) InheritedV ())
  , testP "impl Debug for i32 { }" (Item "" [] (Impl Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) i32 []) InheritedV ())
  , testP "impl Debug for .. { }" (Item "" [] (DefaultImpl Normal (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) InheritedV ())
  , testP "impl Debug for i32 { type T = i32; }" (Item "" [] (Impl Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) i32 [ImplItem "T" InheritedV Final [] (TypeI i32) ()]) InheritedV ())
  , testP "impl Debug for i32 { pub default type T = i32; }" (Item "" [] (Impl Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) i32 [ImplItem "T" PublicV Default [] (TypeI i32) ()]) InheritedV ())
  , testP "impl Debug for i32 { const x: i32 = 1; }" (Item "" [] (Impl Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) i32 [ImplItem "x" InheritedV Final [] (ConstI i32 (Lit [] (Int 1 Unsuffixed ()) ())) ()]) InheritedV ())
  , testP "impl Debug for i32 { const unsafe fn foo(x: i32) -> i32 { return x + 1 } }" 
  (Item "" [] (Impl Normal Positive
                    (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) i32
                    [ImplItem "foo" InheritedV Final []
                       (MethodI (MethodSig Unsafe Const Rust (FnDecl [Arg (Just x) i32 ()] (Just i32) False ()) (Generics [] [] (WhereClause [] ()) ()))
                                (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())) ()) ()] DefaultBlock ())) ()]) InheritedV ())  
  , testP "impl Debug for i32 { pub default extern \"C\" fn foo(x: i32) -> i32 { return x + 1 } }" 
  (Item "" [] (Impl Normal Positive
                    (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) i32
                    [ImplItem "foo" PublicV Default []
                       (MethodI (MethodSig Normal NotConst C (FnDecl [Arg (Just x) i32 ()] (Just i32) False ()) (Generics [] [] (WhereClause [] ()) ()))
                                (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())) ()) ()] DefaultBlock ())) ()]) InheritedV ())  
  , testP "impl Debug for i32 { fn foo(&self) -> i32 { return x + 1 } }" 
  (Item "" [] (Impl Normal Positive
                    (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) i32
                    [ImplItem "foo" InheritedV Final []
                       (MethodI (MethodSig Normal NotConst Rust (FnDecl [SelfRegion Nothing Immutable ()] (Just i32) False ()) (Generics [] [] (WhereClause [] ()) ()))
                                (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing (Path False [(mkIdent "x", NoParameters ())] ()) ()) (Lit [] (Int 1 Unsuffixed ()) ()) ())) ()) ()] DefaultBlock ())) ()]) InheritedV ()) 
  , testP "trait Trace { }" (Item "Trace" [] (Trait Normal (Generics [] [] (WhereClause [] ()) ()) [] []) InheritedV ()) 
  , testP "unsafe trait Trace { }" (Item "Trace" [] (Trait Unsafe (Generics [] [] (WhereClause [] ()) ()) [] []) InheritedV ()) 
  , testP "trait Trace: Debug { }" (Item "Trace" [] (Trait Normal (Generics [] [] (WhereClause [] ()) ()) [TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [("Debug",NoParameters ())] ()) ()) ()) None] []) InheritedV ()) 
  , testP "unsafe trait Trace: Debug { }" (Item "Trace" [] (Trait Unsafe (Generics [] [] (WhereClause [] ()) ()) [TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [("Debug",NoParameters ())] ()) ()) ()) None] []) InheritedV ()) 
  ] 


toFix :: Test
toFix = testGroup "should pass, but don't block tests for now"
  [ testP "impl Debug for i32 { foo!(x); }" (Item "" [] (Impl Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) i32 [ImplItem "" InheritedV Final [] (MacroI (Mac (Path False [("foo", NoParameters ())] ()) [Token (Span (Position 25 26 1) (Position 26 27 1)) (IdentTok "x")]  ())) ()]) InheritedV ())
  , testP "impl Debug for i32 { foo!{x} }" (Item "" [] (Impl Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [("Debug",NoParameters ())] ()) ())) i32 [ImplItem "" InheritedV Final [] (MacroI (Mac (Path False [("foo", NoParameters ())] ()) [Token (Span (Position 25 26 1) (Position 26 27 1)) (IdentTok "x")]  ())) ()]) InheritedV ())
  , testP "foo!(x)"                 (MacP (Mac (Path False [("foo", NoParameters ())] ()) [Token (Span (Position 5 6 1) (Position 6 7 1)) (IdentTok "x")]  ()) ())
  , testP "foo![x]"                 (MacTy (Mac (Path False [("foo", NoParameters ())] ()) [Token (Span (Position 5 6 1) (Position 6 7 1)) (IdentTok "x")]  ()) ())
  ]


-- Just a common pattern to make the tests above more straightforward
x :: Pat ()
x = IdentP (ByValue Immutable) "x" Nothing ()

-- Just a common type to make the tests above more straightforward
i32 :: Ty ()
i32 = PathTy Nothing (Path False [("i32", AngleBracketed [] [] [] ())] ()) ()
