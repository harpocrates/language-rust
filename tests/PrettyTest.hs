{-# LANGUAGE OverloadedStrings #-}
module PrettyTest (prettySuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Pretty

import Control.Monad
import Control.Monad.Trans.Except
import Text.PrettyPrint.Annotated.WL (Doc, flatten, renderPretty, renderPrettyDefault, display)

prettySuite :: Test
prettySuite = testGroup "pretty suite" [ commonCode, prettyLiterals, prettyTypes ]

-- | This contains some random real-life code fragments. The purpose here is 
-- primarily black-box testing.
commonCode :: Test
commonCode = testGroup "printing common code fragments" []

-- | White-box testing of literals, especially character encoding/escapes.
prettyLiterals :: Test
prettyLiterals = testGroup "printing literals"
  [ testFlatten "b\"hello world\"" (printLit (ByteStr "hello world" Cooked Unsuffixed ()))
  , testFlatten "br###\"hello #\"# world\"###" (printLit (ByteStr "hello #\"# world" (Raw 3) Unsuffixed ()))
  , testFlatten "b\"hello \\n w\\x90rld\"" (printLit (ByteStr "hello \n w\x90rld" Cooked Unsuffixed ()))
  , testFlatten "\"hello world\"" (printLit (Str "hello world" Cooked Unsuffixed ()))
  , testFlatten "r###\"hello #\"# world\"###" (printLit (Str "hello #\"# world" (Raw 3) Unsuffixed ()))
  , testFlatten "\"hello \\n w\\x90\\U00012345rld\"" (printLit (Str "hello \n w\x90\74565rld" Cooked Unsuffixed ()))
  , testFlatten "'f'" (printLit (Char 'f' Unsuffixed ()))
  , testFlatten "'\\t'" (printLit (Char '\t' Unsuffixed ()))
  , testFlatten "'\\x81'" (printLit (Char '\129' Unsuffixed ()))
  , testFlatten "'\\u0123'" (printLit (Char '\291' Unsuffixed ()))
  , testFlatten "'\\U00012345'" (printLit (Char '\74565' Unsuffixed ()))
  , testFlatten "b'f'" (printLit (Byte 102 Unsuffixed ()))
  , testFlatten "b'\\t'" (printLit (Byte 9 Unsuffixed ()))
  , testFlatten "b'\\x81'" (printLit (Byte 129 Unsuffixed ()))
  , testFlatten "123.45f32" (printLit (Float 123.45 F32 ()))
  , testFlatten "123.45f64" (printLit (Float 123.45 F64 ()))
  , testFlatten "123" (printLit (Int 123 Unsuffixed ()))
  , testFlatten "123isize" (printLit (Int 123 Is ()))
  , testFlatten "-12i8" (printLit (Int (-12) I8 ()))
  , testFlatten "123456u64" (printLit (Int 123456 U64 ()))
  , testFlatten "123isize" (printLit (Int 123 Is ()))
  , testFlatten "false" (printLit (Bool False Unsuffixed ()))
  , testFlatten "true" (printLit (Bool True Unsuffixed ()))
  ]

-- | Test pretty-printing of types (flattened). 
prettyTypes :: Test
prettyTypes = testGroup "printing types"
  [ testFlatten "i32" (printType i32)
  , testFlatten "f64" (printType f64)
  , testFlatten "usize" (printType usize)
  , testFlatten "[i32]" (printType (Slice i32 ()))
  , testFlatten "[i32; 16]" (printType (Array i32 (Lit [] (Int 16 Unsuffixed ()) ()) ()))
  , testFlatten "*const i32" (printType (Ptr Immutable i32 ()))
  , testFlatten "*mut i32" (printType (Ptr Mutable i32 ()))
  , testFlatten "&mut i32" (printType (Rptr Nothing Mutable i32 ()))
  , testFlatten "&i32" (printType (Rptr Nothing Immutable i32 ()))
  , testFlatten "&'lt mut i32" (printType (Rptr (Just (Lifetime (Name "lt") ())) Mutable i32 ()))
  , testFlatten "&'lt i32" (printType (Rptr (Just (Lifetime (Name "lt") ())) Immutable i32 ()))
  , testFlatten "!" (printType (Never ()))
  , testFlatten "()" (printType (TupTy [] ()))
  , testFlatten "(i32,)" (printType (TupTy [i32] ()))
  , testFlatten "(i32, f64, usize)" (printType (TupTy [i32,f64,usize] ()))
  , testFlatten "std::vec::Vec<i32>" (printType (PathTy Nothing (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "<i32 as std::vec>::Vec<i32>" (printType (PathTy (Just (QSelf i32 2)) (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "i32 + Debug + 'lt" (printType (ObjectSum i32 [ debug', lt ] ()))
  , testFlatten "Debug + 'lt" (printType (PolyTraitRefTy [ debug', lt ] ()))
  , testFlatten "impl Iterator<Item = i32> + 'lt" (printType (ImplTrait [ iterator, lt ] ()))
  , testFlatten "(i32)" (printType (ParenTy i32 ()))
  , testFlatten "typeof(1i32)" (printType (Typeof (Lit [] (Int 1 I32 ()) ()) ()))
  , testFlatten "_" (printType (Infer ()))
  , testFlatten "Self" (printType (ImplicitSelf ()))
  , testFlatten "HList![&str, bool, Vec<i32>]"
                (printType (MacTy (Mac (Path False [("HList", AngleBracketed [] [] [] ())] ())
                                       [ Delimited mempty NoDelim mempty [ Token mempty Ampersand, Token mempty (IdentTok (mkIdent "str")) ] mempty 
                                       , Token mempty (IdentTok (mkIdent "bool"))
                                       , Delimited mempty NoDelim mempty [ Token mempty (IdentTok (mkIdent "Vec"))
                                                                         , Token mempty Less
                                                                         , Token mempty (IdentTok (mkIdent "i32"))
                                                                         , Token mempty Greater
                                                                         ] mempty
                                       ]
                                       ())
                           ()))
  , testFlatten "fn(i32) -> i32"
                (printType (BareFn Normal Rust [] (FnDecl [Arg i32 (IdentP (ByValue Immutable) (mkIdent "") Nothing ()) ()] (Just i32) False ()) ()))
  , testFlatten "unsafe extern \"C\" fn(i32) -> i32"
                (printType (BareFn Unsafe C [] (FnDecl [Arg i32 (IdentP (ByValue Immutable) (mkIdent "") Nothing ()) ()] (Just i32) False ()) ()))
  ]
  where
    -- Just a common type to make the tests above more straightforward
    i32, f64, usize :: Ty ()
    i32 = PathTy Nothing (Path False [("i32", AngleBracketed [] [] [] ())] ()) ()
    f64 = PathTy Nothing (Path False [("f64", AngleBracketed [] [] [] ())] ()) ()
    usize = PathTy Nothing (Path False [("usize", AngleBracketed [] [] [] ())] ()) ()

    -- Couple path segments
    std = ("std", AngleBracketed [] [] [] ())
    vec = ("vec", AngleBracketed [] [] [] ())
    veci32 = ("Vec", AngleBracketed [] [i32] [] ())
    debug = ("Debug", AngleBracketed [] [] [] ())

    -- ty paramater bounds
    debug' = TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [debug] ()) ()) ()) None
    lt = RegionTyParamBound (Lifetime (Name "lt") ())
    iterator = TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [("Iterator", AngleBracketed [] [] [(mkIdent "Item",i32)] ())] ()) ()) ()) None

testRender :: String -> Doc a -> Test
testRender str doc = testCase str $ str @=? display (renderPrettyDefault doc)

testFlatten :: String -> Doc a -> Test
testFlatten str doc = testCase str $ str @=? display (renderPrettyDefault (flatten doc))


