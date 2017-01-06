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
import Text.PrettyPrint.Annotated.WL (Doc, flatten, renderPretty, renderPrettyDefault, display, renderCompact)

prettySuite :: Test
prettySuite = testGroup "pretty suite" [ commonCode, prettyLiterals, prettyTypes, prettyAttributes, prettyExpressions ]

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

-- | Test pretty-printing of attributes (flattened).
prettyAttributes :: Test
prettyAttributes = testGroup "printing attributes"
  [ testFlatten "#![cgf]" (printAttr (Attribute Inner (Word (mkIdent "cgf") ()) False ()) True)
  , testFlatten "#[cgf]" (printAttr (Attribute Outer (Word (mkIdent "cgf") ()) False ()) True)
  , testFlatten "#[derive(Eq, Ord, 1)]" (printAttr (Attribute Outer (List (mkIdent "derive") 
                                                        [ MetaItem (Word (mkIdent "Eq") ()) ()
                                                        , MetaItem (Word (mkIdent "Ord") ()) () 
                                                        , Literal (Int 1 Unsuffixed ()) ()
                                                        ] ()) False ()) True)
  , testFlatten "#[feature = \"foo\"]" (printAttr (Attribute Outer (NameValue (mkIdent "feature") (Str "foo" Cooked Unsuffixed ()) ()) False ()) True)
  , testFlatten "/*! some comment */" (printAttr (Attribute Outer (NameValue (mkIdent "") (Str "some comment" Cooked Unsuffixed ()) ()) True ()) True)
  ]
  
-- | Test pretty-printing of expressions (flattened). 
prettyExpressions :: Test
prettyExpressions = testGroup "printing expressions"
  [ testFlatten "foo" (printExpr foo) 
  , testFlatten "bar" (printExpr bar) 
  , testFlatten "1" (printExpr _1) 
  , testFlatten "#[cfgo] 2" (printExpr _2) 
  , testFlatten "box 1" (printExpr (Box [] _1 ()))
  , testFlatten "#[cfgo] box #[cfgo] 2" (printExpr (Box [cfgO] _2 ()))
  , testFlatten "#[cfgo] 2 <- 1" (printExpr (InPlace [] _2 _1 ()))
  , testFlatten "[1, 1, 1]" (printExpr (Vec [] [_1,_1,_1] ()))
  , testFlatten "#[cfgo] [#![cfgi] #[cfgo] 2, 1, #[cfgo] 2]" (printExpr (Vec [cfgO,cfgI] [_2,_1,_2] ()))
  , testFlatten "foo(1, bar)" (printExpr (Call [] foo [_1,bar] ()))
  , testFlatten "foo.method::<i32, f64>(1, bar)" (printExpr (MethodCall [] (mkIdent "method") [i32,f64] [foo,_1,bar] ()))
  , testFlatten "foo.method(1, bar)" (printExpr (MethodCall [] (mkIdent "method") [] [foo,_1,bar] ()))
  , testFlatten "()" (printExpr (TupExpr [] [] ()))
  , testFlatten "#[cfgo] (#![cfgi])" (printExpr (TupExpr [cfgO,cfgI] [] ()))
  , testFlatten "(1,)" (printExpr (TupExpr [] [_1] ()))
  , testFlatten "#[cfgo] (#![cfgi] 1,)" (printExpr (TupExpr [cfgO,cfgI] [_1] ()))
  , testFlatten "(1, foo, bar)" (printExpr (TupExpr [] [_1, foo, bar] ()))
  , testFlatten "#[cfgo] (#![cfgi] 1, foo, bar)" (printExpr (TupExpr [cfgO, cfgI] [_1, foo, bar] ()))
  , testFlatten "-foo" (printExpr (Unary [] Neg foo ()))
  , testFlatten "!foo" (printExpr (Unary [] Not foo ()))
  , testFlatten "foo + 1" (printExpr (Binary [] AddOp foo _1 ()))
  , testFlatten "bar * 1" (printExpr (Binary [] MulOp bar _1 ()))
  , testFlatten "foo + bar * 1" (printExpr (Binary [] AddOp foo (Binary [] MulOp bar _1 ()) ()))
  , testFlatten "(foo + bar) * 1" (printExpr (Binary [] MulOp (Binary [] AddOp foo bar ()) _1 ()))
  , testFlatten "1 * (foo + bar)" (printExpr (Binary [] MulOp _1 (Binary [] AddOp foo bar ()) ()))
  , testFlatten "-1 * (foo + *bar)" (printExpr (Binary [] MulOp (Unary [] Neg _1 ()) (Binary [] AddOp foo (Unary [] Deref bar ()) ()) ()))
  , testFlatten "foo as i32" (printExpr (Cast [] foo i32 ()))
  , testFlatten "foo as f64 as i32" (printExpr (Cast [] (Cast [] foo f64 ()) i32 ()))
  , testFlatten "(foo + 1) as i32" (printExpr (Cast [] (Binary [] AddOp foo _1 ()) i32 ()))
  , testFlatten "foo: i32" (printExpr (TypeAscription [] foo i32 ()))
  , testFlatten "if foo { foo = 1 }" (printExpr (If [] foo assBlk Nothing ()))
  , testFlatten "if foo { foo = 1 } else { return 1; }" (printExpr (If [] foo assBlk (Just (BlockExpr [] retBlk ())) ()))
  , testFlatten "if foo { foo = 1 } else if bar { return 1; }" (printExpr (If [] foo assBlk (Just (If [] bar retBlk Nothing ())) ()))
  , testFlatten "if let foo = 1 { return 1; }" (printExpr (IfLet [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) _1 retBlk Nothing ()))
  , testFlatten "if let foo = 1 { return 1; } else { return 1; }" (printExpr (IfLet [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) _1 retBlk (Just (BlockExpr [] retBlk ())) ()))
  , testFlatten "while foo { foo = 1 }" (printExpr (While [] foo assBlk Nothing ()))
  , testFlatten "'lbl: while foo { foo = 1 }" (printExpr (While [] foo assBlk (Just (mkIdent "'lbl")) ()))
  , testFlatten "#[cfgo] while foo { #![cfgi] foo = 1 }" (printExpr (While [cfgI,cfgO] foo assBlk Nothing ()))
  , testFlatten "while let foo = 1 { foo = 1 }" (printExpr (WhileLet [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) _1 assBlk Nothing ()))
  , testFlatten "'lbl: while let foo = 1 { foo = 1 }" (printExpr (WhileLet [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) _1 assBlk (Just (mkIdent "'lbl")) ()))
  , testFlatten "#[cfgo] while let foo = 1 { #![cfgi] foo = 1 }" (printExpr (WhileLet [cfgO,cfgI] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) _1 assBlk Nothing ()))
  , testFlatten "for foo in bar { foo = 1 }" (printExpr (ForLoop [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) bar assBlk Nothing ()))
  , testFlatten "'lbl: for foo in bar { foo = 1 }" (printExpr (ForLoop [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) bar assBlk (Just (mkIdent "'lbl")) ()))
  , testFlatten "#[cfgo] for foo in bar { #![cfgi] foo = 1 }" (printExpr (ForLoop [cfgO,cfgI] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) bar assBlk Nothing ()))
  , testFlatten "loop { foo = 1 }" (printExpr (Loop [] assBlk Nothing ()))
  , testFlatten "'lbl: loop { foo = 1 }" (printExpr (Loop [] assBlk (Just (mkIdent "'lbl")) ()))
  , testFlatten "#[cfgo] loop { #![cfgi] foo = 1 }" (printExpr (Loop [cfgO,cfgI] assBlk Nothing ()))
  , testFlatten "match foo { }" (printExpr (Match [] foo [] ())) 
  , testFlatten "match foo { _ => 1 }" (printExpr (Match [] foo [Arm [] [WildP ()] Nothing _1 ()] ())) 
  , testFlatten "#[cfgo] match foo { #![cfgi] _ => 1 }" (printExpr (Match [cfgI,cfgO] foo [Arm [] [WildP ()] Nothing _1 ()] ())) 
  , testFlatten "match foo { _ => { return 1; } }" (printExpr (Match [] foo [Arm [] [WildP ()] Nothing (BlockExpr [] retBlk ()) ()] ())) 
  , testFlatten "match foo { _ => { return 1; } _ | _ if foo => 1 }" (printExpr (Match [] foo [Arm [] [WildP ()] Nothing (BlockExpr [] retBlk ()) (), Arm [] [WildP (), WildP ()] (Just foo) _1 ()] ())) 
  , testFlatten "move |x: i32| { return 1; }"
                (printExpr (Closure [] Value (FnDecl [Arg i32 (IdentP (ByValue Immutable) (mkIdent "x") Nothing ()) ()] Nothing False ()) 
                                    retBlk ()))
  , testFlatten "|x: i32| -> i32 { return 1; }"
                (printExpr (Closure [] Ref (FnDecl [Arg i32 (IdentP (ByValue Immutable) (mkIdent "x") Nothing ()) ()] (Just i32) False ()) 
                                    retBlk ()))
  , testFlatten "#[cfgo] { #![cfgi] return 1; }" (printExpr (BlockExpr [cfgI,cfgO] retBlk ()))
  , testFlatten "{ return 1; }" (printExpr (BlockExpr [] retBlk ()))
  , testFlatten "foo = 1" (printExpr (Assign [] foo _1 ()))
  , testFlatten "foo += 1" (printExpr (AssignOp [] AddOp foo _1 ()))
  , testFlatten "foo <<= 1" (printExpr (AssignOp [] ShlOp foo _1 ()))
  , testFlatten "foo.bar" (printExpr (FieldAccess [] foo (mkIdent "bar") ()))
  , testFlatten "foo.1" (printExpr (TupField [] foo 1 ()))
  , testFlatten "foo[1]" (printExpr (Index [] foo _1 ()))
  , testFlatten "foo..bar" (printExpr (Range [] (Just foo) (Just bar) HalfOpen ()))
  , testFlatten "foo..." (printExpr (Range [] (Just foo) Nothing Closed ()))
  , testFlatten "..bar" (printExpr (Range [] Nothing (Just bar) HalfOpen ()))
  , testFlatten "..." (printExpr (Range [] Nothing Nothing Closed ()))
  , testFlatten "std::vec::Vec::<i32>" (printExpr (PathExpr [] Nothing (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "<i32 as std::vec>::Vec::<i32>" (printExpr (PathExpr [] (Just (QSelf i32 2)) (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "&foo" (printExpr (AddrOf [] Immutable foo ()))
  , testFlatten "#[cfgo] &mut foo" (printExpr (AddrOf [cfgO] Mutable foo ()))
  , testFlatten "break" (printExpr (Break [] Nothing ()))
  , testFlatten "break 'foo" (printExpr (Break [] (Just (mkIdent "'foo")) ()))
  , testFlatten "continue" (printExpr (Continue [] Nothing ()))
  , testFlatten "continue 'foo" (printExpr (Continue [] (Just (mkIdent "'foo")) ()))
  , testFlatten "return" (printExpr (Ret [] Nothing ()))
  , testFlatten "return foo" (printExpr (Ret [] (Just foo) ()))
  , testFlatten "#[cfgo] asm!(\"mov eax, 2\" : \"={eax}\"(foo) : \"{dx}\"(bar) : \"eax\" : \"volatile\", \"alignstack\")"
                (printExpr (InlineAsmExpr [cfgO] (InlineAsm "mov eax, 2" Cooked [InlineAsmOutput "={eax}" foo False False]
                                                            [("{dx}",bar)] ["eax"] True True Att ()) ()))
  , testFlatten "asm!(\"mov eax, 2\" : \"={eax}\"(foo) : : : \"intel\")"
                (printExpr (InlineAsmExpr [] (InlineAsm "mov eax, 2" Cooked [InlineAsmOutput "={eax}" foo False False] []
                                                        [] False False Intel ()) ()))
  , testFlatten "print!(foo)" (printExpr (MacExpr [] (Mac (Path False [("print", AngleBracketed [] [] [] ())] ())
                                       [ Token mempty (IdentTok (mkIdent "foo")) ] ()) ()))
  , testFlatten "foo { }" (printExpr (Struct [] (Path False [("foo", AngleBracketed [] [] [] ())] ()) [] Nothing ()))
  , testFlatten "foo { x: 1 }" (printExpr (Struct [] (Path False [("foo", AngleBracketed [] [] [] ())] ()) [Field (mkIdent "x") _1 ()] Nothing ()))
  , testFlatten "#[cfgo] foo { #![cfgi] x: 1 }" (printExpr (Struct [cfgO,cfgI] (Path False [("foo", AngleBracketed [] [] [] ())] ()) [Field (mkIdent "x") _1 ()] Nothing ()))
  , testFlatten "foo { x: 1, y: 1 }" (printExpr (Struct [] (Path False [("foo", AngleBracketed [] [] [] ())] ()) [Field (mkIdent "x") _1 (), Field (mkIdent "y") _1 ()] Nothing ()))
  , testFlatten "foo { x: 1, y: 1, ..bar }" (printExpr (Struct [] (Path False [("foo", AngleBracketed [] [] [] ())] ()) [Field (mkIdent "x") _1 (), Field (mkIdent "y") _1 ()] (Just bar) ()))
  , testFlatten "#[cfgo] foo { #![cfgi] x: 1, y: 1, ..bar }" (printExpr (Struct [cfgO,cfgI] (Path False [("foo", AngleBracketed [] [] [] ())] ()) [Field (mkIdent "x") _1 (), Field (mkIdent "y") _1 ()] (Just bar) ()))
  , testFlatten "[foo; 1]" (printExpr (Repeat [] foo _1 ()))
  , testFlatten "#[cfgo] [#![cfgi] foo; 1]" (printExpr (Repeat [cfgI, cfgO] foo _1 ()))
  , testFlatten "(foo?)" (printExpr (ParenExpr [] (Try [] foo ()) ()))
  , testFlatten "foo?" (printExpr (Try [] foo ()))
  ]
  where
    -- Literal expressionsi
    _1, _2, foo, bar :: Expr ()
    _1 = Lit [] (Int 1 Unsuffixed ()) ()
    _2 = Lit [cfgO] (Int 2 Unsuffixed ()) ()
    foo = PathExpr [] Nothing (Path False [("foo", AngleBracketed [] [] [] ())] ()) ()
    bar = PathExpr [] Nothing (Path False [("bar", AngleBracketed [] [] [] ())] ()) ()

    -- Attributes
    cfgI, cfgO :: Attribute ()
    cfgI = Attribute Inner (Word (mkIdent "cfgi") ()) False ()
    cfgO = Attribute Outer (Word (mkIdent "cfgo") ()) False ()

    -- Types
    i32, f64 :: Ty ()
    i32 = PathTy Nothing (Path False [("i32", AngleBracketed [] [] [] ())] ()) ()
    f64 = PathTy Nothing (Path False [("f64", AngleBracketed [] [] [] ())] ()) ()
    
    -- Path segments
    std = ("std", AngleBracketed [] [] [] ())
    vec = ("vec", AngleBracketed [] [] [] ())
    veci32 = ("Vec", AngleBracketed [] [i32] [] ())
    debug = ("Debug", AngleBracketed [] [] [] ())

    -- Blocks
    assBlk = Block [ NoSemi (Assign [] foo _1 ()) () ] DefaultBlock ()
    retBlk = Block [ Semi (Ret [] (Just _1) ()) () ] DefaultBlock ()


testRender :: String -> Doc a -> Test
testRender str doc = testCase str $ str @=? display (renderPrettyDefault doc)

testFlatten :: String -> Doc a -> Test
testFlatten str doc = testCase str $ str @=? display (renderPretty 0.5 1000 (flatten doc))


