{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module PrettyTest (prettySuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Pretty

import Control.Monad
import Text.PrettyPrint.Annotated.WL (Doc, flatten, renderPretty, renderPrettyDefault, display, renderCompact)

prettySuite :: Test
prettySuite = testGroup "pretty suite"
  [ prettyLiterals
  , prettyPatterns
  , prettyTypes
  , prettyAttributes
  , prettyExpressions
  , prettyItems
  , prettyStatements
  ]


-- | Common types to make tests more straightforward
i32, f64, usize :: Ty ()
i32 = PathTy Nothing (Path False [("i32", NoParameters ())] ()) ()
f64 = PathTy Nothing (Path False [("f64", NoParameters ())] ()) ()
usize = PathTy Nothing (Path False [("usize", NoParameters ())] ()) ()

-- | Common path segments to make tests more straightforward
std = ("std", NoParameters ())
vec = ("vec", NoParameters ())
veci32 = ("Vec", AngleBracketed [] [i32] [] ())
debug = ("Debug", NoParameters ())
println = ("println", NoParameters ())

-- | Type parameter bounds to make tests more straightforward
debug' = TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [debug] ())) ()) None
lt = RegionTyParamBound (Lifetime "lt" ())
iterator = TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [("Iterator", AngleBracketed [] [] [(mkIdent "Item",i32)] ())] ())) ()) None

-- | Short expressions to make tests more straightforward
_1, _2, foo, bar :: Expr ()
_1 = Lit [] (Int 1 Unsuffixed ()) ()
_2 = Lit [cfgO] (Int 2 Unsuffixed ()) ()
foo = PathExpr [] Nothing (Path False [("foo", NoParameters ())] ()) ()
bar = PathExpr [] Nothing (Path False [("bar", NoParameters ())] ()) ()

-- | Attributes to make tests more straightforward
cfgI, cfgO :: Attribute ()
cfgI = Attribute Inner (Word (mkIdent "cfgi") ()) False ()
cfgO = Attribute Outer (Word (mkIdent "cfgo") ()) False ()

-- | Blocks to make tests more straightforward
assBlk = Block [ NoSemi (Assign [] foo _1 ()) () ] Normal ()
retBlk = Block [ Semi (Ret [] (Just _1) ()) () ] Normal ()

-- | Short patterns to make tests more straightforward
x :: Pat ()
x = IdentP (ByValue Immutable) "x" Nothing ()


-- | White-box testing of literals, especially character encoding/escapes.
prettyLiterals :: Test
prettyLiterals = testGroup "printing literals"
  [ testFlatten "b\"hello world\"" (pretty (ByteStr "hello world" Cooked Unsuffixed ()))
  , testFlatten "br###\"hello #\"# world\"###" (pretty (ByteStr "hello #\"# world" (Raw 3) Unsuffixed ()))
  , testFlatten "b\"hello \\n w\\x90rld\"" (pretty (ByteStr "hello \n w\x90rld" Cooked Unsuffixed ()))
  , testFlatten "\"hello world\"" (pretty (Str "hello world" Cooked Unsuffixed ()))
  , testFlatten "r###\"hello #\"# world\"###" (pretty (Str "hello #\"# world" (Raw 3) Unsuffixed ()))
  , testFlatten "\"hello \\n w\\x90\\U00012345rld\"" (pretty (Str "hello \n w\x90\74565rld" Cooked Unsuffixed ()))
  , testFlatten "'f'" (pretty (Char 'f' Unsuffixed ()))
  , testFlatten "'\\t'" (pretty (Char '\t' Unsuffixed ()))
  , testFlatten "'\\x81'" (pretty (Char '\129' Unsuffixed ()))
  , testFlatten "'\\u0123'" (pretty (Char '\291' Unsuffixed ()))
  , testFlatten "'\\U00012345'" (pretty (Char '\74565' Unsuffixed ()))
  , testFlatten "b'f'" (pretty (Byte 102 Unsuffixed ()))
  , testFlatten "b'\\t'" (pretty (Byte 9 Unsuffixed ()))
  , testFlatten "b'\\x81'" (pretty (Byte 129 Unsuffixed ()))
  , testFlatten "123.45f32" (pretty (Float 123.45 F32 ()))
  , testFlatten "123.45f64" (pretty (Float 123.45 F64 ()))
  , testFlatten "123" (pretty (Int 123 Unsuffixed ()))
  , testFlatten "123isize" (pretty (Int 123 Is ()))
  , testFlatten "-12i8" (pretty (Int (-12) I8 ()))
  , testFlatten "123456u64" (pretty (Int 123456 U64 ()))
  , testFlatten "123isize" (pretty (Int 123 Is ()))
  , testFlatten "false" (pretty (Bool False Unsuffixed ()))
  , testFlatten "true" (pretty (Bool True Unsuffixed ()))
  ]

-- | Test pretty-printing of patterns (flattened).
prettyPatterns :: Test
prettyPatterns = testGroup "printing patterns"
  [ testFlatten "_" (pretty (WildP ()))
  , testFlatten "x" (pretty x)
  , testFlatten "x @ _" (pretty (IdentP (ByValue Immutable) (mkIdent "x") (Just (WildP ())) ()))
  , testFlatten "ref x" (pretty (IdentP (ByRef Immutable) (mkIdent "x") Nothing ()))
  , testFlatten "mut x" (pretty (IdentP (ByValue Mutable) (mkIdent "x") Nothing ()))
  , testFlatten "ref mut x" (pretty (IdentP (ByRef Mutable) (mkIdent "x") Nothing ()))
  , testFlatten "Point { .. }" (pretty (StructP (Path False [("Point", NoParameters ())] ()) [] True ()))
  , testFlatten "Point { x, y: y1 }" (pretty (StructP (Path False [("Point", NoParameters ())] ())
                                               [ FieldPat Nothing x ()
                                               , FieldPat (Just "y") (IdentP (ByValue Immutable) "y1" Nothing ()) () ]
                                               False ()))
  , testFlatten "Point { x, .. }" (pretty (StructP (Path False [("Point", NoParameters ())] ())
                                               [ FieldPat Nothing x () ]
                                               True ())) 
  , testFlatten "Point(x)" (pretty (TupleStructP (Path False [("Point", NoParameters ())] ())
                                              [ x ] Nothing ()))
  , testFlatten "Point()" (pretty (TupleStructP (Path False [("Point", NoParameters ())] ())
                                              [] Nothing ()))
  , testFlatten "Point(..)" (pretty (TupleStructP (Path False [("Point", NoParameters ())] ())
                                              [] (Just 0) ()))
  , testFlatten "Point(x, ..)" (pretty (TupleStructP (Path False [("Point", NoParameters ())] ())
                                              [ x ] (Just 1) ()))
  , testFlatten "Point(.., x)" (pretty (TupleStructP (Path False [("Point", NoParameters ())] ())
                                              [ x ] (Just 0) ()))
  , testFlatten "Point(x, _, .., _, x)" (pretty (TupleStructP (Path False [("Point", NoParameters ())] ())
                                              [ x, WildP (), WildP (), x ] (Just 2) ()))
  , testFlatten "math::PI" (pretty (PathP Nothing (Path False [ ("math", NoParameters ())
                                                                , ("PI", NoParameters ()) ] ()) ()))
  -- I don't understand qpaths. The test below fails, and I have no idea whether it should.
  , testFlatten "<i32 as a(i32, i32)>::b::<'lt>::AssociatedItem"
                (pretty (PathP (Just (QSelf i32 1)) (Path False [ ("a", Parenthesized [i32, i32] Nothing ())
                                                                  , ("b", AngleBracketed [Lifetime  "lt" ()] [] [] ())
                                                                  , ("AssociatedItem", NoParameters ())
                                                                  ] ()) ()))
  , testFlatten "(x, ref mut y, box z)" (pretty (TupleP [ x
                                                          , IdentP (ByRef Mutable) "y" Nothing ()
                                                          , BoxP (IdentP (ByValue Immutable) "z" Nothing ()) ()
                                                          ] 
                                                          Nothing ()))
  , testFlatten "ref mut y @ (x, x)" (pretty (IdentP (ByRef Mutable) "y" (Just (TupleP [ x, x ] Nothing ())) ()))
  , testFlatten "(1, 2, .., 3)" (pretty (TupleP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) ()
                                               , LitP (Lit [] (Int 2 Unsuffixed ()) ()) ()
                                               , LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ]
                                               (Just 2) ()))

  , testFlatten "box x" (pretty (BoxP x ()))
  , testFlatten "1 ... 2" (pretty (RangeP (Lit [] (Int 1 Unsuffixed ()) ()) (Lit [] (Int 2 Unsuffixed ()) ()) ()))
  , testFlatten "&x" (pretty (RefP x Immutable ()))
  , testFlatten "&mut x" (pretty (RefP x Mutable ()))
  , testFlatten "true" (pretty (LitP (Lit [] (Bool True Unsuffixed ()) ()) ()))
  , testFlatten "-123" (pretty (LitP (Unary [] Neg (Lit [] (Int 123 Unsuffixed ()) ()) ()) ()))
  , testFlatten "[1, 2]" (pretty (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) ()
                                          , LitP (Lit [] (Int 2 Unsuffixed ()) ()) () ] 
                                          Nothing [] ()))
  , testFlatten "[1, .., 3]" (pretty (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ]
                                             (Just (WildP ()))
                                             [ LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ] ()))
  , testFlatten "[1, x.., 3]" (pretty (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ]
                                              (Just x)
                                              [ LitP (Lit [] (Int 3 Unsuffixed ()) ()) () ] ()))
  , testFlatten "[1, ..]" (pretty (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ] (Just (WildP ())) [] ()))
  , testFlatten "[1, x..]" (pretty (SliceP [ LitP (Lit [] (Int 1 Unsuffixed ()) ()) () ] (Just x) [] ()))

  , testFlatten "vecPat!(foo)" (pretty (MacP (Mac (Path False [("vecPat", NoParameters ())] ())
                                                   [ Token mempty (IdentTok (mkIdent "foo")) ] ()) ()))
  ]

-- | Test pretty-printing of types (flattened). 
prettyTypes :: Test
prettyTypes = testGroup "printing types"
  [ testFlatten "i32" (pretty i32)
  , testFlatten "f64" (pretty f64)
  , testFlatten "usize" (pretty usize)
  , testFlatten "[i32]" (pretty (Slice i32 ()))
  , testFlatten "[i32; 16]" (pretty (Array i32 (Lit [] (Int 16 Unsuffixed ()) ()) ()))
  , testFlatten "*const i32" (pretty (Ptr Immutable i32 ()))
  , testFlatten "*mut i32" (pretty (Ptr Mutable i32 ()))
  , testFlatten "&mut i32" (pretty (Rptr Nothing Mutable i32 ()))
  , testFlatten "&i32" (pretty (Rptr Nothing Immutable i32 ()))
  , testFlatten "&'lt mut i32" (pretty (Rptr (Just (Lifetime "lt" ())) Mutable i32 ()))
  , testFlatten "&'lt i32" (pretty (Rptr (Just (Lifetime "lt" ())) Immutable i32 ()))
  , testFlatten "!" (pretty (Never ()))
  , testFlatten "()" (pretty (TupTy [] ()))
  , testFlatten "(i32,)" (pretty (TupTy [i32] ()))
  , testFlatten "(i32, f64, usize)" (pretty (TupTy [i32,f64,usize] ()))
  , testFlatten "std::vec::Vec<i32>" (pretty (PathTy Nothing (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "<i32 as std::vec>::Vec<i32>" (pretty (PathTy (Just (QSelf i32 2)) (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "i32 + Debug + 'lt" (pretty (ObjectSum i32 [ debug', lt ] ()))
  , testFlatten "Debug + 'lt" (pretty (TraitObject [ debug', lt ] ()))
  , testFlatten "impl Iterator<Item = i32> + 'lt" (pretty (ImplTrait [ iterator, lt ] ()))
  , testFlatten "(i32)" (pretty (ParenTy i32 ()))
  , testFlatten "typeof(1i32)" (pretty (Typeof (Lit [] (Int 1 I32 ()) ()) ()))
  , testFlatten "_" (pretty (Infer ()))
  , testFlatten "HList![&str , bool , Vec<i32>]"
                (pretty (MacTy (Mac (Path False [("HList", NoParameters ())] ())
                                       [ Delimited mempty NoDelim mempty [ Token mempty Ampersand, Token mempty (IdentTok (mkIdent "str")) ] mempty 
                                       , Token mempty Comma
                                       , Token mempty (IdentTok (mkIdent "bool"))
                                       , Token mempty Comma
                                       , Delimited mempty NoDelim mempty [ Token mempty (IdentTok (mkIdent "Vec"))
                                                                         , Token mempty Less
                                                                         , Token mempty (IdentTok (mkIdent "i32"))
                                                                         , Token mempty Greater
                                                                         ] mempty
                                       ]
                                       ())
                           ()))
  , testFlatten "fn(i32) -> i32"
                (pretty (BareFn Normal Rust [] (FnDecl [Arg Nothing i32 ()] (Just i32) False ()) ()))
  , testFlatten "unsafe extern \"C\" fn(i32) -> i32"
                (pretty (BareFn Unsafe C [] (FnDecl [Arg Nothing i32 ()] (Just i32) False ()) ()))
  ]
    
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
  , testFlatten "/** some comment */" (printAttr (Attribute Outer (NameValue (mkIdent "") (Str "some comment" Cooked Unsuffixed ()) ()) True ()) True)
  ]
  
-- | Test pretty-printing of expressions (flattened). 
prettyExpressions :: Test
prettyExpressions = testGroup "printing expressions"
  [ testFlatten "foo" (pretty foo) 
  , testFlatten "bar" (pretty bar) 
  , testFlatten "1" (pretty _1) 
  , testFlatten "#[cfgo] 2" (pretty _2) 
  , testFlatten "box 1" (pretty (Box [] _1 ()))
  , testFlatten "#[cfgo] box #[cfgo] 2" (pretty (Box [cfgO] _2 ()))
  , testFlatten "#[cfgo] 2 <- 1" (pretty (InPlace [] _2 _1 ()))
  , testFlatten "[1, 1, 1]" (pretty (Vec [] [_1,_1,_1] ()))
  , testFlatten "#[cfgo] [#![cfgi] #[cfgo] 2, 1, #[cfgo] 2]" (pretty (Vec [cfgO,cfgI] [_2,_1,_2] ()))
  , testFlatten "foo(1, bar)" (pretty (Call [] foo [_1,bar] ()))
  , testFlatten "foo.method::<i32, f64>(1, bar)" (pretty (MethodCall [] foo (mkIdent "method") (Just [i32,f64]) [_1,bar] ()))
  , testFlatten "foo.method::<>(1, bar)" (pretty (MethodCall [] foo (mkIdent "method") (Just []) [_1,bar] ()))
  , testFlatten "foo.method(1, bar)" (pretty (MethodCall [] foo (mkIdent "method") Nothing [_1,bar] ()))
  , testFlatten "()" (pretty (TupExpr [] [] ()))
  , testFlatten "#[cfgo] (#![cfgi])" (pretty (TupExpr [cfgO,cfgI] [] ()))
  , testFlatten "(1,)" (pretty (TupExpr [] [_1] ()))
  , testFlatten "#[cfgo] (#![cfgi] 1,)" (pretty (TupExpr [cfgO,cfgI] [_1] ()))
  , testFlatten "(1, foo, bar)" (pretty (TupExpr [] [_1, foo, bar] ()))
  , testFlatten "#[cfgo] (#![cfgi] 1, foo, bar)" (pretty (TupExpr [cfgO, cfgI] [_1, foo, bar] ()))
  , testFlatten "-foo" (pretty (Unary [] Neg foo ()))
  , testFlatten "!foo" (pretty (Unary [] Not foo ()))
  , testFlatten "foo + 1" (pretty (Binary [] AddOp foo _1 ()))
  , testFlatten "bar * 1" (pretty (Binary [] MulOp bar _1 ()))
  , testFlatten "foo + bar * 1" (pretty (Binary [] AddOp foo (Binary [] MulOp bar _1 ()) ()))
  , testFlatten "(foo + bar) * 1" (pretty (Binary [] MulOp (Binary [] AddOp foo bar ()) _1 ()))
  , testFlatten "1 * (foo + bar)" (pretty (Binary [] MulOp _1 (Binary [] AddOp foo bar ()) ()))
  , testFlatten "-1 * (foo + *bar)" (pretty (Binary [] MulOp (Unary [] Neg _1 ()) (Binary [] AddOp foo (Unary [] Deref bar ()) ()) ()))
  , testFlatten "foo as i32" (pretty (Cast [] foo i32 ()))
  , testFlatten "foo as f64 as i32" (pretty (Cast [] (Cast [] foo f64 ()) i32 ()))
  , testFlatten "(foo + 1) as i32" (pretty (Cast [] (Binary [] AddOp foo _1 ()) i32 ()))
  , testFlatten "foo: i32" (pretty (TypeAscription [] foo i32 ()))
  , testFlatten "if foo { foo = 1 }" (pretty (If [] foo assBlk Nothing ()))
  , testFlatten "if foo { foo = 1 } else { return 1; }" (pretty (If [] foo assBlk (Just (BlockExpr [] retBlk ())) ()))
  , testFlatten "if foo { foo = 1 } else if bar { return 1; }" (pretty (If [] foo assBlk (Just (If [] bar retBlk Nothing ())) ()))
  , testFlatten "if let foo = 1 { return 1; }" (pretty (IfLet [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) _1 retBlk Nothing ()))
  , testFlatten "if let foo = 1 { return 1; } else { return 1; }" (pretty (IfLet [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) _1 retBlk (Just (BlockExpr [] retBlk ())) ()))
  , testFlatten "while foo { foo = 1 }" (pretty (While [] foo assBlk Nothing ()))
  , testFlatten "'lbl: while foo { foo = 1 }" (pretty (While [] foo assBlk (Just (Lifetime "lbl" ())) ()))
  , testFlatten "#[cfgo] while foo { #![cfgi] foo = 1 }" (pretty (While [cfgI,cfgO] foo assBlk Nothing ()))
  , testFlatten "while let foo = 1 { foo = 1 }" (pretty (WhileLet [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) _1 assBlk Nothing ()))
  , testFlatten "'lbl: while let foo = 1 { foo = 1 }" (pretty (WhileLet [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) _1 assBlk (Just (Lifetime "lbl" ())) ()))
  , testFlatten "#[cfgo] while let foo = 1 { #![cfgi] foo = 1 }" (pretty (WhileLet [cfgO,cfgI] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) _1 assBlk Nothing ()))
  , testFlatten "for foo in bar { foo = 1 }" (pretty (ForLoop [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) bar assBlk Nothing ()))
  , testFlatten "'lbl: for foo in bar { foo = 1 }" (pretty (ForLoop [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) bar assBlk (Just (Lifetime "lbl" ())) ()))
  , testFlatten "#[cfgo] for foo in bar { #![cfgi] foo = 1 }" (pretty (ForLoop [cfgO,cfgI] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) bar assBlk Nothing ()))
  , testFlatten "loop { foo = 1 }" (pretty (Loop [] assBlk Nothing ()))
  , testFlatten "'lbl: loop { foo = 1 }" (pretty (Loop [] assBlk (Just (Lifetime "lbl" ())) ()))
  , testFlatten "#[cfgo] loop { #![cfgi] foo = 1 }" (pretty (Loop [cfgO,cfgI] assBlk Nothing ()))
  , testFlatten "match foo { }" (pretty (Match [] foo [] ())) 
  , testFlatten "match foo { _ => 1 }" (pretty (Match [] foo [Arm [] [WildP ()] Nothing _1 ()] ())) 
  , testFlatten "#[cfgo] match foo { #![cfgi] _ => 1 }" (pretty (Match [cfgI,cfgO] foo [Arm [] [WildP ()] Nothing _1 ()] ())) 
  , testFlatten "match foo {\n  _ => { return 1; }\n}" (pretty (Match [] foo [Arm [] [WildP ()] Nothing (BlockExpr [] retBlk ()) ()] ())) 
  , testFlatten "match foo {\n  _ => { return 1; }\n  _ | _ if foo => 1\n}" (pretty (Match [] foo [Arm [] [WildP ()] Nothing (BlockExpr [] retBlk ()) (), Arm [] [WildP (), WildP ()] (Just foo) _1 ()] ())) 
  , testFlatten "move |x: i32| { return 1; }"
                (pretty (Closure [] Value (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing False ()) 
                                    (BlockExpr [] retBlk ()) ()))
  , testFlatten "|x: i32| -> i32 { return 1; }"
                (pretty (Closure [] Ref (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] (Just i32) False ()) 
                                    (BlockExpr [] retBlk ()) ()))
  , testFlatten "#[cfgo] { #![cfgi] return 1; }" (pretty (BlockExpr [cfgI,cfgO] retBlk ()))
  , testFlatten "{ return 1; }" (pretty (BlockExpr [] retBlk ()))
  , testFlatten "foo = 1" (pretty (Assign [] foo _1 ()))
  , testFlatten "foo += 1" (pretty (AssignOp [] AddOp foo _1 ()))
  , testFlatten "foo <<= 1" (pretty (AssignOp [] ShlOp foo _1 ()))
  , testFlatten "foo.bar" (pretty (FieldAccess [] foo (mkIdent "bar") ()))
  , testFlatten "foo.1" (pretty (TupField [] foo 1 ()))
  , testFlatten "foo[1]" (pretty (Index [] foo _1 ()))
  , testFlatten "foo..bar" (pretty (Range [] (Just foo) (Just bar) HalfOpen ()))
  , testFlatten "foo..." (pretty (Range [] (Just foo) Nothing Closed ()))
  , testFlatten "..bar" (pretty (Range [] Nothing (Just bar) HalfOpen ()))
  , testFlatten "..." (pretty (Range [] Nothing Nothing Closed ()))
  , testFlatten "std::vec::Vec::<i32>" (pretty (PathExpr [] Nothing (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "<i32 as std::vec>::Vec::<i32>" (pretty (PathExpr [] (Just (QSelf i32 2)) (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "&foo" (pretty (AddrOf [] Immutable foo ()))
  , testFlatten "#[cfgo] &mut foo" (pretty (AddrOf [cfgO] Mutable foo ()))
  , testFlatten "break" (pretty (Break [] Nothing Nothing ()))
  , testFlatten "break 1" (pretty (Break [] Nothing (Just (Lit [] (Int 1 Unsuffixed ()) ())) ()))
  , testFlatten "break 'foo" (pretty (Break [] (Just (Lifetime "foo" ())) Nothing ()))
  , testFlatten "break 'foo 1" (pretty (Break [] (Just (Lifetime "foo" ())) (Just (Lit [] (Int 1 Unsuffixed ()) ())) ()))
  , testFlatten "continue" (pretty (Continue [] Nothing ()))
  , testFlatten "continue 'foo" (pretty (Continue [] (Just (Lifetime "foo" ())) ()))
  , testFlatten "return" (pretty (Ret [] Nothing ()))
  , testFlatten "return foo" (pretty (Ret [] (Just foo) ()))
  , testFlatten "#[cfgo] asm!(\"mov eax, 2\" : \"={eax}\"(foo) : \"{dx}\"(bar) : \"eax\" : \"volatile\", \"alignstack\")"
                (pretty (InlineAsmExpr [cfgO] (InlineAsm "mov eax, 2" Cooked [InlineAsmOutput "={eax}" foo False False]
                                                            [("{dx}",bar)] ["eax"] True True Att ()) ()))
  , testFlatten "asm!(\"mov eax, 2\" : \"={eax}\"(foo) : : : \"intel\")"
                (pretty (InlineAsmExpr [] (InlineAsm "mov eax, 2" Cooked [InlineAsmOutput "={eax}" foo False False] []
                                                        [] False False Intel ()) ()))
  , testFlatten "print!(foo)" (pretty (MacExpr [] (Mac (Path False [("print", NoParameters ())] ())
                                       [ Token mempty (IdentTok (mkIdent "foo")) ] ()) ()))
  , testFlatten "foo { }" (pretty (Struct [] (Path False [("foo", NoParameters ())] ()) [] Nothing ()))
  , testFlatten "foo { x: 1 }" (pretty (Struct [] (Path False [("foo", NoParameters ())] ()) [Field (mkIdent "x") _1 ()] Nothing ()))
  , testFlatten "#[cfgo] foo { #![cfgi] x: 1 }" (pretty (Struct [cfgO,cfgI] (Path False [("foo", NoParameters ())] ()) [Field (mkIdent "x") _1 ()] Nothing ()))
  , testFlatten "foo { x: 1, y: 1 }" (pretty (Struct [] (Path False [("foo", NoParameters ())] ()) [Field (mkIdent "x") _1 (), Field (mkIdent "y") _1 ()] Nothing ()))
  , testFlatten "foo { x: 1, y: 1, ..bar }" (pretty (Struct [] (Path False [("foo", NoParameters ())] ()) [Field (mkIdent "x") _1 (), Field (mkIdent "y") _1 ()] (Just bar) ()))
  , testFlatten "#[cfgo] foo { #![cfgi] x: 1, y: 1, ..bar }" (pretty (Struct [cfgO,cfgI] (Path False [("foo", NoParameters ())] ()) [Field (mkIdent "x") _1 (), Field (mkIdent "y") _1 ()] (Just bar) ()))
  , testFlatten "[foo; 1]" (pretty (Repeat [] foo _1 ()))
  , testFlatten "#[cfgo] [#![cfgi] foo; 1]" (pretty (Repeat [cfgI, cfgO] foo _1 ()))
  , testFlatten "(foo?)" (pretty (ParenExpr [] (Try [] foo ()) ()))
  , testFlatten "foo?" (pretty (Try [] foo ()))
  ]

-- | Test pretty-printing of items (flattened)
prettyItems :: Test
prettyItems = testGroup "printing items"
  [ testFlatten "extern crate \"rustc-serialize\" as rustc_serialize;" (pretty (Item (mkIdent "rustc_serialize") [] (ExternCrate (Just (mkIdent "rustc-serialize"))) InheritedV ()))
  , testFlatten "pub extern crate rustc_serialize;" (pretty (Item (mkIdent "rustc_serialize") [] (ExternCrate Nothing) PublicV ()))
  , testFlatten "#[cfgo] pub extern crate rustc_serialize;" (pretty (Item (mkIdent "rustc_serialize") [cfgO] (ExternCrate Nothing) PublicV ()))
  , testFlatten "use std::vec as baz;" (pretty (Item (mkIdent "") [] (Use (ViewPathSimple False ["std"] (PathListItem "vec" (Just "baz") ()) ())) InheritedV ()))
  , testFlatten "use std::vec::*;" (pretty (Item (mkIdent "") [] (Use (ViewPathGlob False ["std","vec"] ())) InheritedV ()))
  , testFlatten "use std::vec::{a as b, c};" (pretty (Item (mkIdent "") [] (Use (ViewPathList False ["std","vec"] [PathListItem (mkIdent "a") (Just (mkIdent "b")) (), PathListItem (mkIdent "c") Nothing ()] ())) InheritedV ()))
  , testFlatten "static mut foo: i32 = 1;" (pretty (Item (mkIdent "foo") [] (Static i32 Mutable _1) InheritedV ()))
  , testFlatten "static foo: i32 = 1;" (pretty (Item (mkIdent "foo") [] (Static i32 Immutable _1) InheritedV ()))
  , testFlatten "const foo: i32 = 1;" (pretty (Item (mkIdent "foo") [] (ConstItem i32 _1) InheritedV ()))
  , testFlatten "fn foo(x: i32) -> i32 { return 1; }" (pretty (Item (mkIdent "foo") [] (Fn (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] (Just i32) False ()) Normal NotConst Rust (Generics [] [] (WhereClause [] ()) ()) retBlk) InheritedV ()))
  , testFlatten "unsafe fn foo(x: i32, ...) { return 1; }" (pretty (Item (mkIdent "foo") [] (Fn (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing True ()) Unsafe NotConst Rust (Generics [] [] (WhereClause [] ()) ()) retBlk) InheritedV ()))
  , testFlatten "const unsafe extern \"C\" fn foo(x: i32) { return 1; }" (pretty (Item (mkIdent "foo") [] (Fn (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing False ()) Unsafe Const C (Generics [] [] (WhereClause [] ()) ()) retBlk) InheritedV ()))
  , testFlatten "fn foo<'lt: 'foo + 'bar, T, U: Debug + 'lt = i32>(x: i32) { return 1; }" (pretty (Item (mkIdent "foo") [] (Fn (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing False ()) Normal NotConst Rust (Generics [LifetimeDef [] (Lifetime "lt" ()) [Lifetime "foo" (), Lifetime "bar" ()] ()] [TyParam [] (mkIdent "T") [] Nothing (), TyParam [] (mkIdent "U") [debug', lt] (Just i32) ()] (WhereClause [] ()) ()) retBlk) InheritedV ()))
  , testFlatten "fn foo<T, U: Debug + 'lt = i32>(x: i32) where for<'lt> i32: Debug, 'foo: 'bar, vec::std = i32 { return 1; }" (pretty (Item (mkIdent "foo") [] (Fn (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing False ()) Normal NotConst Rust (Generics [] [TyParam [] (mkIdent "T") [] Nothing (), TyParam [] (mkIdent "U") [debug', lt] (Just i32) ()] (WhereClause [BoundPredicate [LifetimeDef [] (Lifetime "lt" ()) [] ()] i32 [debug'] (), RegionPredicate (Lifetime "foo" ()) [Lifetime "bar" ()] (), EqPredicate (Path False [vec,std] ()) i32 ()] ()) ()) retBlk) InheritedV ()))
  , testFlatten "mod serialize { }" (pretty (Item (mkIdent "serialize") [] (Mod []) InheritedV ()))
  , testFlatten "mod serialize { const foo: i32 = 1; }" (pretty (Item (mkIdent "serialize") [] (Mod [Item (mkIdent "foo") [] (ConstItem i32 _1) InheritedV ()]) InheritedV ()))
  , testFlatten "#[cfgo] mod serialize {\n  #![cfgi]\n  const foo: i32 = 1;\n}" (pretty (Item (mkIdent "serialize") [cfgO,cfgI] (Mod [Item (mkIdent "foo") [] (ConstItem i32 _1) InheritedV ()]) InheritedV ()))
  , testFlatten "extern \"C\" { }" (pretty (Item (mkIdent "") [] (ForeignMod C []) InheritedV ()))
  , testFlatten "extern \"C\" { static mut foo: i32; }" (pretty (Item (mkIdent "") [] (ForeignMod C [ForeignItem (mkIdent "foo") [] (ForeignStatic i32 True) InheritedV ()]) InheritedV ()))
  , testFlatten "#[cfgo] extern \"C\" {\n  #![cfgi]\n  fn foo(x: i32) -> i32;\n}" (pretty (Item (mkIdent "") [cfgO,cfgI] (ForeignMod C [ForeignItem (mkIdent "foo") [] (ForeignFn (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] (Just i32) False ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ()]) InheritedV ()))
  , testFlatten "#[cfgo] extern \"C\" {\n  #![cfgi]\n  static foo: i32;\n}" (pretty (Item (mkIdent "") [cfgO,cfgI] (ForeignMod C [ForeignItem (mkIdent "foo") [] (ForeignStatic i32 False) InheritedV ()]) InheritedV ()))
  , testFlatten "type Vec<T> = i32;" (pretty (Item (mkIdent "Vec") [] (TyAlias i32 (Generics [] [TyParam [] (mkIdent "T") [] Nothing ()] (WhereClause [] ()) ())) InheritedV ()))
  , testFlatten "enum foo<T> { }" (pretty (Item (mkIdent "foo") [] (Enum [] (Generics [] [TyParam [] (mkIdent "T") [] Nothing ()] (WhereClause [] ()) ())) InheritedV ()))
  , testFlatten "enum color {\n  #[cfgo]\n  red { i32 } = 1,\n  blue(i32),\n  green,\n}" (pretty (Item (mkIdent "color") [] (Enum
       [ Variant (mkIdent "red") [cfgO] (StructD [StructField Nothing InheritedV i32 [] ()] ()) (Just _1) ()
       , Variant (mkIdent "blue") [] (TupleD [StructField Nothing InheritedV i32 [] ()] ()) Nothing ()
       , Variant (mkIdent "green") [] (UnitD ()) Nothing ()]
       (Generics [] [] (WhereClause [] ()) ())) InheritedV ()))
  , testFlatten "struct red { x: i32 }" (pretty (Item (mkIdent "red") [] (StructItem (StructD [StructField (Just (mkIdent "x")) InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ()))
  , testFlatten "union red { x: i32 }" (pretty (Item (mkIdent "red") [] (Union (StructD [StructField (Just (mkIdent "x")) InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ()))
  , testFlatten "union red { x: i32 }" (pretty (Item (mkIdent "red") [] (Union (StructD [StructField (Just (mkIdent "x")) InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ())) InheritedV ()))
  , testFlatten "impl std::Debug for .. { }" (pretty (Item (mkIdent "") [] (DefaultImpl Normal (TraitRef (Path False [std,debug] ()))) InheritedV ()))
  , testFlatten "unsafe impl Debug for .. { }" (pretty (Item (mkIdent "") [] (DefaultImpl Unsafe (TraitRef (Path False [debug] ()))) InheritedV ()))
  , testFlatten "impl Debug for i32 { }" (pretty (Item (mkIdent "") [] (Impl Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [debug] ()))) i32 []) InheritedV ()))
  , testFlatten "pub impl !Debug for i32 where 'lt: 'gt { }" (pretty (Item (mkIdent "") [] (Impl Normal Negative (Generics [] [] (WhereClause [RegionPredicate (Lifetime "lt" ()) [Lifetime "gt" ()] ()] ()) ()) (Just (TraitRef (Path False [debug] ()))) i32 []) PublicV ()))
  , testFlatten "impl <T> GenVal<T> {\n  fn value(&mut self) -> &T { return 1; }\n}" 
                (pretty (Item (mkIdent "") [] (Impl Normal Positive
                      (Generics [] [TyParam [] (mkIdent "T") [] Nothing ()] (WhereClause [] ()) ())
                      Nothing
                      (PathTy Nothing (Path False [("GenVal", AngleBracketed [] [PathTy Nothing (Path False [(mkIdent "T", NoParameters ())] ()) ()] [] ())] ()) ())
                      [ ImplItem (mkIdent "value") InheritedV Final []
                          (MethodI (MethodSig Normal NotConst Rust
                                      (FnDecl [SelfRegion Nothing Mutable ()]
                                              (Just (Rptr Nothing Immutable (PathTy Nothing (Path False [(mkIdent "T", NoParameters ())] ()) ()) ())) 
                                              False ())
                                      (Generics [] [] (WhereClause [] ()) ())) 
                                      retBlk)
                          ()
                      ]) InheritedV ()))
  , testFlatten "#[cfgo] impl i32 {\n  #![cfgi]\n  fn value(&self) -> i32 { return 1; }\n  pub const pi: i32 = 1;\n  default type Size = i32;\n}" 
                (pretty (Item (mkIdent "") [cfgI,cfgO] (Impl Normal Positive
                      (Generics [] [] (WhereClause [] ()) ())
                      Nothing
                      i32
                      [ ImplItem (mkIdent "value") InheritedV Final []
                          (MethodI (MethodSig Normal NotConst Rust
                                      (FnDecl [SelfRegion Nothing Immutable ()]
                                              (Just i32) 
                                              False ())
                                      (Generics [] [] (WhereClause [] ()) ())) 
                                      retBlk)
                          ()
                      , ImplItem (mkIdent "pi") PublicV Final [] (ConstI i32 _1) ()
                      , ImplItem (mkIdent "Size") InheritedV Default [] (TypeI i32) ()
                      ]) InheritedV ()))
  , testFlatten "unsafe trait Show { }" (pretty (Item (mkIdent "Show") [] (Trait Unsafe (Generics [] [] (WhereClause [] ()) ()) [] []) InheritedV ()))
  , testFlatten "trait Show<T> : 'l1 + for<'l3: 'l1 + 'l2> Debug + 'l2 { }" (pretty (Item (mkIdent "Show") []
                                       (Trait Normal
                                              (Generics [] [TyParam [] (mkIdent "T") [] Nothing ()] (WhereClause [] ()) ())
                                              [ RegionTyParamBound (Lifetime "l1" ())
                                              , TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime "l3" ()) [Lifetime "l1" (), Lifetime "l2" ()] ()] (TraitRef (Path False [debug] ())) ())  None
                                              , RegionTyParamBound (Lifetime "l2" ())]
                                              []) InheritedV ()))
  , testFlatten "pub trait Show {\n  fn value(&mut self) -> i32 ;\n  const pi: i32 = 1;\n  const e: i32;\n  type Size = i32;\n  type Length : 'l3;\n  type SomeType : 'l1 = f64;\n}"
                (pretty (Item (mkIdent "Show") [] (Trait Normal (Generics [] [] (WhereClause [] ()) ()) []
                      [ TraitItem (mkIdent "value") []
                          (MethodT (MethodSig Normal NotConst Rust
                                      (FnDecl [SelfRegion Nothing Mutable ()]
                                              (Just i32) 
                                              False ())
                                      (Generics [] [] (WhereClause [] ()) ())) 
                                      Nothing)
                          ()
                      , TraitItem (mkIdent "pi") [] (ConstT i32 (Just _1)) ()
                      , TraitItem (mkIdent "e") [] (ConstT i32 Nothing) ()
                      , TraitItem (mkIdent "Size") [] (TypeT [] (Just i32)) ()
                      , TraitItem (mkIdent "Length") [] (TypeT [RegionTyParamBound (Lifetime "l3" ())] Nothing) ()
                      , TraitItem (mkIdent "SomeType") [] (TypeT [RegionTyParamBound (Lifetime "l1" ())] (Just f64)) ()
                      ]) PublicV ()))
  ]

-- | Test pretty-printing of statements (flattened). 
prettyStatements :: Test
prettyStatements = testGroup "printing statements"
  [ testFlatten "#[cfgo] let _;" (pretty (Local (WildP ()) Nothing Nothing [cfgO] ()))
  , testFlatten "let _: i32;" (pretty (Local (WildP ()) (Just i32) Nothing [] ()))
  , testFlatten "let _: i32 = 1;" (pretty (Local (WildP ()) (Just i32) (Just _1) [] ()))
  , testFlatten "extern crate rustc_serialize;" (pretty (ItemStmt (Item (mkIdent "rustc_serialize") [] (ExternCrate Nothing) InheritedV ()) ())) 
  , testFlatten "if foo { foo = 1 }" (pretty (NoSemi (If [] foo assBlk Nothing ()) ()))
  , testFlatten "{ return 1; }" (pretty (NoSemi (BlockExpr [] retBlk ()) ()))
  , testFlatten "foo;" (pretty (NoSemi foo ()))
  , testFlatten "1;" (pretty (NoSemi _1 ()))
  , testFlatten "if foo { foo = 1 };" (pretty (Semi (If [] foo assBlk Nothing ()) ()))
  , testFlatten "{ return 1; };" (pretty (Semi (BlockExpr [] retBlk ()) ()))
  , testFlatten "foo;" (pretty (Semi foo ()))
  , testFlatten "1;" (pretty (Semi _1 ()))
  , testFlatten "#[cfgo] println!(foo);" (pretty (MacStmt (Mac (Path False [println] ()) [ Token mempty (IdentTok (mkIdent "foo")) ] ()) SemicolonMac [cfgO] ()))
  , testFlatten "println!(foo);" (pretty (MacStmt (Mac (Path False [println] ()) [ Token mempty (IdentTok (mkIdent "foo")) ] ()) SemicolonMac [] ()))
  , testFlatten "println!{ foo }" (pretty (MacStmt (Mac (Path False [println] ()) [ Token mempty (IdentTok (mkIdent "foo")) ] ()) BracesMac [] ()))
  ]
  
-- | Default pretty-printing
testRender :: String -> Doc a -> Test
testRender str doc = testCase (escapeNewlines str) $ str @=? display (renderPrettyDefault doc)

-- | This tries to make it so that the `Doc` gets rendered onto only one line.
testFlatten :: String -> Doc a -> Test
testFlatten str doc = testCase (escapeNewlines str) $ str @=? display (renderPretty 0.5 1000 (flatten doc))

-- | Utility function for escaping newlines (and only newlines)
escapeNewlines :: String -> String
escapeNewlines = concatMap (\c -> if c == '\n' then "\\n" else [c])


