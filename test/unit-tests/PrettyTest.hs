{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
module PrettyTest (prettySuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, Path, Label)

import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Syntax
import Language.Rust.Pretty.Internal

import Data.Text.Prettyprint.Doc (Doc, layoutPretty, LayoutOptions(..), PageWidth(..))
import Data.Text.Prettyprint.Doc.Render.String (renderShowS)

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
i32 = PathTy Nothing (Path False [PathSegment "i32" Nothing ()] ()) ()
f64 = PathTy Nothing (Path False [PathSegment "f64" Nothing ()] ()) ()
usize = PathTy Nothing (Path False [PathSegment "usize" Nothing ()] ()) ()

-- | Common path segments to make tests more straightforward
std, vec, veci32, debug, println :: PathSegment ()
std = PathSegment "std" Nothing ()
vec = PathSegment "vec" Nothing ()
veci32 = PathSegment "Vec" (Just (AngleBracketed [] [i32] [] ())) ()
debug = PathSegment "Debug" Nothing ()
println = PathSegment "println" Nothing ()

-- | Common paths
point :: Path ()
point = Path False [PathSegment "Point" Nothing ()] ()

-- | Type parameter bounds to make tests more straightforward
debug', lt, iterator :: TyParamBound ()
debug' = TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [debug] ())) ()) None ()
lt = RegionTyParamBound (Lifetime "lt" ()) ()
iterator = TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [PathSegment "Iterator" (Just (AngleBracketed [] [] [(mkIdent "Item",i32)] ())) ()] ())) ()) None ()

-- | Short expressions to make tests more straightforward
_1, _2, foo, bar :: Expr ()
_1 = Lit [] (Int Dec 1 Unsuffixed ()) ()
_2 = Lit [cfgO] (Int Dec 2 Unsuffixed ()) ()
foo = PathExpr [] Nothing (Path False [PathSegment "foo" Nothing ()] ()) ()
bar = PathExpr [] Nothing (Path False [PathSegment "bar" Nothing ()] ()) ()

-- | Attributes to make tests more straightforward
cfgI, cfgO :: Attribute ()
cfgI = Attribute Inner (Path False [PathSegment "cfgi" Nothing ()] ()) (Stream []) ()
cfgO = Attribute Outer (Path False [PathSegment "cfgo" Nothing ()] ()) (Stream []) ()


-- | Blocks to make tests more straightforward
assBlk, retBlk :: Block ()
assBlk = Block [ NoSemi (Assign [] foo _1 ()) () ] Normal ()
retBlk = Block [ Semi (Ret [] (Just _1) ()) () ] Normal ()

-- | Short patterns to make tests more straightforward
x :: Pat ()
x = IdentP (ByValue Immutable) "x" Nothing ()


-- | White-box testing of literals, especially character encoding/escapes.
prettyLiterals :: Test
prettyLiterals = testGroup "printing literals"
  [ testFlatten "b\"hello world\"" (printLit (byteStr "hello world" Cooked Unsuffixed ()))
  , testFlatten "br###\"hello #\"# world\"###" (printLit (byteStr "hello #\"# world" (Raw 3) Unsuffixed ()))
  , testFlatten "b\"hello \\n w\\x7frld\"" (printLit (byteStr "hello \n w\x7frld" Cooked Unsuffixed ()))
  , testFlatten "\"hello world\"" (printLit (Str "hello world" Cooked Unsuffixed ()))
  , testFlatten "r###\"hello #\"# world\"###" (printLit (Str "hello #\"# world" (Raw 3) Unsuffixed ()))
  , testFlatten "\"hello \\n w\\x7f\\u{012345}rld\"" (printLit (Str "hello \n w\x7f\74565rld" Cooked Unsuffixed ()))
  , testFlatten "'f'" (printLit (Char 'f' Unsuffixed ()))
  , testFlatten "'\\t'" (printLit (Char '\t' Unsuffixed ()))
  , testFlatten "'\\x7f'" (printLit (Char '\x7f' Unsuffixed ()))
  , testFlatten "'\\u{0123}'" (printLit (Char '\291' Unsuffixed ()))
  , testFlatten "'\\u{012345}'" (printLit (Char '\74565' Unsuffixed ()))
  , testFlatten "b'f'" (printLit (Byte 102 Unsuffixed ()))
  , testFlatten "b'\\t'" (printLit (Byte 9 Unsuffixed ()))
  , testFlatten "b'\\x81'" (printLit (Byte 129 Unsuffixed ()))
  , testFlatten "123.45f32" (printLit (Float 123.45 F32 ()))
  , testFlatten "123.45f64" (printLit (Float 123.45 F64 ()))
  , testFlatten "123" (printLit (Int Dec 123 Unsuffixed ()))
  , testFlatten "123isize" (printLit (Int Dec 123 Is ()))
  , testFlatten "-12i8" (printLit (Int Dec (-12) I8 ()))
  , testFlatten "123456u64" (printLit (Int Dec 123456 U64 ()))
  , testFlatten "123isize" (printLit (Int Dec 123 Is ()))
  , testFlatten "false" (printLit (Bool False Unsuffixed ()))
  , testFlatten "true" (printLit (Bool True Unsuffixed ()))
  ]

-- | Test pretty printing of patterns (flattened).
prettyPatterns :: Test
prettyPatterns = testGroup "printing patterns"
  [ testFlatten "_" (printPat (WildP ()))
  , testFlatten "x" (printPat x)
  , testFlatten "x @ _" (printPat (IdentP (ByValue Immutable) (mkIdent "x") (Just (WildP ())) ()))
  , testFlatten "ref x" (printPat (IdentP (ByRef Immutable) (mkIdent "x") Nothing ()))
  , testFlatten "mut x" (printPat (IdentP (ByValue Mutable) (mkIdent "x") Nothing ()))
  , testFlatten "ref mut x" (printPat (IdentP (ByRef Mutable) (mkIdent "x") Nothing ()))
  , testFlatten "Point { .. }" (printPat (StructP point [] True ()))
  , testFlatten "Point { x, y: y1 }" (printPat (StructP point
                                               [ FieldPat Nothing x ()
                                               , FieldPat (Just "y") (IdentP (ByValue Immutable) "y1" Nothing ()) () ]
                                               False ()))
  , testFlatten "Point { x, .. }" (printPat (StructP point
                                               [ FieldPat Nothing x () ]
                                               True ())) 
  , testFlatten "Point(x)" (printPat (TupleStructP point
                                              [ x ] Nothing ()))
  , testFlatten "Point()" (printPat (TupleStructP point
                                              [] Nothing ()))
  , testFlatten "Point(..)" (printPat (TupleStructP point
                                              [] (Just 0) ()))
  , testFlatten "Point(x, ..)" (printPat (TupleStructP point
                                              [ x ] (Just 1) ()))
  , testFlatten "Point(.., x)" (printPat (TupleStructP point
                                              [ x ] (Just 0) ()))
  , testFlatten "Point(x, _, .., _, x)" (printPat (TupleStructP point
                                              [ x, WildP (), WildP (), x ] (Just 2) ()))
  , testFlatten "math::PI" (printPat (PathP Nothing (Path False [ PathSegment "math" Nothing ()
                                                                , PathSegment "PI" Nothing () ] ()) ()))
  , testFlatten "<i32 as a(i32, i32)>::b::<'lt>::AssociatedItem"
                (printPat (PathP (Just (QSelf i32 1)) (Path False [ PathSegment "a" (Just (Parenthesized [i32, i32] Nothing ())) ()
                                                                  , PathSegment "b" (Just (AngleBracketed [Lifetime  "lt" ()] [] [] ())) ()
                                                                  , PathSegment "AssociatedItem" Nothing ()
                                                                  ] ()) ()))
  , testFlatten "(x, ref mut y, box z)" (printPat (TupleP [ x
                                                          , IdentP (ByRef Mutable) "y" Nothing ()
                                                          , BoxP (IdentP (ByValue Immutable) "z" Nothing ()) ()
                                                          ] 
                                                          Nothing ()))
  , testFlatten "ref mut y @ (x, x)" (printPat (IdentP (ByRef Mutable) "y" (Just (TupleP [ x, x ] Nothing ())) ()))
  , testFlatten "(1, 2, .., 3)" (printPat (TupleP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()
                                               , LitP (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()
                                               , LitP (Lit [] (Int Dec 3 Unsuffixed ()) ()) () ]
                                               (Just 2) ()))

  , testFlatten "box x" (printPat (BoxP x ()))
  , testFlatten "1 ..= 2" (printPat (RangeP (Lit [] (Int Dec 1 Unsuffixed ()) ()) (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()))
  , testFlatten "&x" (printPat (RefP x Immutable ()))
  , testFlatten "&mut x" (printPat (RefP x Mutable ()))
  , testFlatten "true" (printPat (LitP (Lit [] (Bool True Unsuffixed ()) ()) ()))
  , testFlatten "-123" (printPat (LitP (Unary [] Neg (Lit [] (Int Dec 123 Unsuffixed ()) ()) ()) ()))
  , testFlatten "[1, 2]" (printPat (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()
                                          , LitP (Lit [] (Int Dec 2 Unsuffixed ()) ()) () ] 
                                          Nothing [] ()))
  , testFlatten "[1, .., 3]" (printPat (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) () ]
                                             (Just (WildP ()))
                                             [ LitP (Lit [] (Int Dec 3 Unsuffixed ()) ()) () ] ()))
  , testFlatten "[1, x.., 3]" (printPat (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) () ]
                                              (Just x)
                                              [ LitP (Lit [] (Int Dec 3 Unsuffixed ()) ()) () ] ()))
  , testFlatten "[1, ..]" (printPat (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) () ] (Just (WildP ())) [] ()))
  , testFlatten "[1, x..]" (printPat (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) () ] (Just x) [] ()))

  , testFlatten "vec!(foo)" (printPat (MacP (Mac (Path False [vec] ())
                                                 (Stream [ Tree (Token mempty (IdentTok (mkIdent "foo"))) ]) ()) ()))
  ]

-- | Test pretty printing of types (flattened). 
prettyTypes :: Test
prettyTypes = testGroup "printing types"
  [ testFlatten "i32" (printType i32)
  , testFlatten "f64" (printType f64)
  , testFlatten "usize" (printType usize)
  , testFlatten "[i32]" (printType (Slice i32 ()))
  , testFlatten "[i32; 16]" (printType (Array i32 (Lit [] (Int Dec 16 Unsuffixed ()) ()) ()))
  , testFlatten "*const i32" (printType (Ptr Immutable i32 ()))
  , testFlatten "*mut i32" (printType (Ptr Mutable i32 ()))
  , testFlatten "&mut i32" (printType (Rptr Nothing Mutable i32 ()))
  , testFlatten "&i32" (printType (Rptr Nothing Immutable i32 ()))
  , testFlatten "&'lt mut i32" (printType (Rptr (Just (Lifetime "lt" ())) Mutable i32 ()))
  , testFlatten "&'lt i32" (printType (Rptr (Just (Lifetime "lt" ())) Immutable i32 ()))
  , testFlatten "!" (printType (Never ()))
  , testFlatten "()" (printType (TupTy [] ()))
  , testFlatten "(i32,)" (printType (TupTy [i32] ()))
  , testFlatten "(i32, f64, usize)" (printType (TupTy [i32,f64,usize] ()))
  , testFlatten "std::vec::Vec<i32>" (printType (PathTy Nothing (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "<i32 as std::vec>::Vec<i32>" (printType (PathTy (Just (QSelf i32 2)) (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "Debug + 'lt" (printType (TraitObject [ debug', lt ] ()))
  , testFlatten "impl Iterator<Item = i32> + 'lt" (printType (ImplTrait [ iterator, lt ] ()))
  , testFlatten "(i32)" (printType (ParenTy i32 ()))
  , testFlatten "typeof(1i32)" (printType (Typeof (Lit [] (Int Dec 1 I32 ()) ()) ()))
  , testFlatten "_" (printType (Infer ()))
  , testFlatten "HList![ &str, bool, Vec<i32> ]"
                (printType (MacTy (Mac (Path False [PathSegment "HList" Nothing ()] ()) (Stream
                                       [ Tree (Token mempty Ampersand)
                                       , Tree (Token mempty (IdentTok (mkIdent "str")))
                                       , Tree (Token mempty Comma)
                                       , Tree (Token mempty (IdentTok (mkIdent "bool")))
                                       , Tree (Token mempty Comma)
                                       , Tree (Token mempty (IdentTok (mkIdent "Vec")))
                                       , Tree (Token mempty Less)
                                       , Tree (Token mempty (IdentTok (mkIdent "i32")))
                                       , Tree (Token mempty Greater)
                                       ])
                                       ())
                           ()))
  , testFlatten "HList![ &str, bool, Vec<i32> ]"
                (printType (MacTy (Mac (Path False [PathSegment "HList" Nothing ()] ()) (Stream
                                       [ Tree (Token (Span (Position 8 1 8) (Position 9 1 9)) Ampersand)
                                       , Tree (Token (Span (Position 9 1 9) (Position 12 1 12)) (IdentTok "str"))
                                       , Tree (Token (Span (Position 12 1 12) (Position 13 1 13)) Comma)
                                       , Tree (Token (Span (Position 14 1 14) (Position 18 1 18)) (IdentTok "bool"))
                                       , Tree (Token (Span (Position 18 1 18) (Position 19 1 19)) Comma)
                                       , Tree (Token (Span (Position 20 1 20) (Position 23 1 23)) (IdentTok "Vec"))
                                       , Tree (Token (Span (Position 23 1 23) (Position 24 1 24)) Less)
                                       , Tree (Token (Span (Position 24 1 24) (Position 27 1 27)) (IdentTok "i32"))
                                       , Tree (Token (Span (Position 27 1 27) (Position 28 1 28)) Greater)
                                       ])
                                       ())
                           ()))               
  , testFlatten "fn(i32) -> i32"
                (printType (BareFn Normal Rust [] (FnDecl [Arg Nothing i32 ()] (Just i32) False ()) ()))
  , testFlatten "unsafe extern \"C\" fn(i32) -> i32"
                (printType (BareFn Unsafe C [] (FnDecl [Arg Nothing i32 ()] (Just i32) False ()) ()))
  ]
    
-- | Test pretty printing of attributes (flattened).
prettyAttributes :: Test
prettyAttributes = testGroup "printing attributes"
  [ testFlatten "#![cfgi]" (printAttr cfgI True) 
  , testFlatten "#[cfgo]" (printAttr cfgO True)
  , testFlatten "#[derive(Eq, Ord, 1)]" (printAttr (Attribute Outer (Path False [PathSegment "derive" Nothing ()] ()) (Tree (Delimited mempty Paren (Stream
                                                          [ Tree (Token mempty (IdentTok "Eq"))
                                                          , Tree (Token mempty Comma)
                                                          , Tree (Token mempty (IdentTok "Ord"))
                                                          , Tree (Token mempty Comma)
                                                          , Tree (Token mempty (LiteralTok (IntegerTok "1") Nothing))
                                                          ]))) ()) True)
  , testFlatten "#[feature = \"foo\"]" (printAttr (Attribute Outer (Path False [PathSegment "feature" Nothing ()] ()) (Stream
                                                          [ Tree (Token mempty Equal)
                                                          , Tree (Token mempty (LiteralTok (StrTok "foo") Nothing))
                                                          ]) ()) True)
  , testFlatten "/** some comment */" (printAttr (SugaredDoc Outer True " some comment " ()) True)
  ]
  
-- | Test pretty printing of expressions (flattened). 
prettyExpressions :: Test
prettyExpressions = testGroup "printing expressions"
  [ testFlatten "foo" (printExpr foo) 
  , testFlatten "bar" (printExpr bar) 
  , testFlatten "1" (printExpr _1) 
  , testFlatten "#[cfgo] 2" (printExpr _2) 
  , testFlatten "box 1" (printExpr (Box [] _1 ()))
  , testFlatten "#[cfgo] box #[cfgo] 2" (printExpr (Box [cfgO] _2 ()))
  , testFlatten "#[cfgo] 2 <- 1" (printExpr (InPlace [] _2 _1 ()))
  , testFlatten "[ 1, 1, 1 ]" (printExpr (Vec [] [_1,_1,_1] ()))
  , testFlatten "#[cfgo] [ #![cfgi] #[cfgo] 2, 1, #[cfgo] 2 ]" (printExpr (Vec [cfgO,cfgI] [_2,_1,_2] ()))
  , testFlatten "foo(1, bar)" (printExpr (Call [] foo [_1,bar] ()))
  , testFlatten "foo.method::<i32, f64>(1, bar)" (printExpr (MethodCall [] foo (mkIdent "method") (Just [i32,f64]) [_1,bar] ()))
  , testFlatten "foo.method::<>(1, bar)" (printExpr (MethodCall [] foo (mkIdent "method") (Just []) [_1,bar] ()))
  , testFlatten "foo.method(1, bar)" (printExpr (MethodCall [] foo (mkIdent "method") Nothing [_1,bar] ()))
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
  , testFlatten "(foo + bar) * 1" (printExpr (Binary [] MulOp (ParenExpr [] (Binary [] AddOp foo bar ()) ()) _1 ()))
  , testFlatten "1 * (foo + bar)" (printExpr (Binary [] MulOp _1 (ParenExpr [] (Binary [] AddOp foo bar ()) ()) ()))
  , testFlatten "-1 * (foo + *bar)" (printExpr (Binary [] MulOp (Unary [] Neg _1 ()) (ParenExpr [] (Binary [] AddOp foo (Unary [] Deref bar ()) ()) ()) ()))
  , testFlatten "foo as i32" (printExpr (Cast [] foo i32 ()))
  , testFlatten "foo as f64 as i32" (printExpr (Cast [] (Cast [] foo f64 ()) i32 ()))
  , testFlatten "(foo + 1) as i32" (printExpr (Cast [] (ParenExpr [] (Binary [] AddOp foo _1 ()) ()) i32 ()))
  , testFlatten "foo: i32" (printExpr (TypeAscription [] foo i32 ()))
  , testFlatten "if foo { foo = 1 }" (printExpr (If [] foo assBlk Nothing ()))
  , testFlatten "if foo { foo = 1 } else { return 1; }" (printExpr (If [] foo assBlk (Just (BlockExpr [] retBlk ())) ()))
  , testFlatten "if foo { foo = 1 } else if bar { return 1; }" (printExpr (If [] foo assBlk (Just (If [] bar retBlk Nothing ())) ()))
  , testFlatten "if let foo = 1 { return 1; }" (printExpr (IfLet [] [IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()] _1 retBlk Nothing ()))
  , testFlatten "if let foo = 1 { return 1; } else { return 1; }" (printExpr (IfLet [] [IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()] _1 retBlk (Just (BlockExpr [] retBlk ())) ()))
  , testFlatten "while foo { foo = 1 }" (printExpr (While [] foo assBlk Nothing ()))
  , testFlatten "'lbl: while foo { foo = 1 }" (printExpr (While [] foo assBlk (Just (Label "lbl" ())) ()))
  , testFlatten "#[cfgo] while foo { #![cfgi] foo = 1 }" (printExpr (While [cfgI,cfgO] foo assBlk Nothing ()))
  , testFlatten "while let foo = 1 { foo = 1 }" (printExpr (WhileLet [] [IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()] _1 assBlk Nothing ()))
  , testFlatten "'lbl: while let foo = 1 { foo = 1 }" (printExpr (WhileLet [] [IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()] _1 assBlk (Just (Label "lbl" ())) ()))
  , testFlatten "#[cfgo] while let foo = 1 { #![cfgi] foo = 1 }" (printExpr (WhileLet [cfgO,cfgI] [IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()] _1 assBlk Nothing ()))
  , testFlatten "for foo in bar { foo = 1 }" (printExpr (ForLoop [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) bar assBlk Nothing ()))
  , testFlatten "'lbl: for foo in bar { foo = 1 }" (printExpr (ForLoop [] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) bar assBlk (Just (Label "lbl" ())) ()))
  , testFlatten "#[cfgo] for foo in bar { #![cfgi] foo = 1 }" (printExpr (ForLoop [cfgO,cfgI] (IdentP (ByValue Immutable) (mkIdent "foo") Nothing ()) bar assBlk Nothing ()))
  , testFlatten "loop { foo = 1 }" (printExpr (Loop [] assBlk Nothing ()))
  , testFlatten "'lbl: loop { foo = 1 }" (printExpr (Loop [] assBlk (Just (Label "lbl" ())) ()))
  , testFlatten "#[cfgo] loop { #![cfgi] foo = 1 }" (printExpr (Loop [cfgO,cfgI] assBlk Nothing ()))
  , testFlatten "match foo { }" (printExpr (Match [] foo [] ())) 
  , testFlatten "match foo { _ => 1 }" (printExpr (Match [] foo [Arm [] [WildP ()] Nothing _1 ()] ())) 
  , testFlatten "#[cfgo] match foo { #![cfgi] _ => 1 }" (printExpr (Match [cfgI,cfgO] foo [Arm [] [WildP ()] Nothing _1 ()] ())) 
  , testFlatten "match foo { _ => { return 1; } }" (printExpr (Match [] foo [Arm [] [WildP ()] Nothing (BlockExpr [] retBlk ()) ()] ())) 
  , testFlatten "match foo { _ => { return 1; }, _ | _ if foo => 1 }" (printExpr (Match [] foo [Arm [] [WildP ()] Nothing (BlockExpr [] retBlk ()) (), Arm [] [WildP (), WildP ()] (Just foo) _1 ()] ())) 
  , testFlatten "move |x: i32| { return 1; }"
                (printExpr (Closure [] Movable Value (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing False ()) 
                                    (BlockExpr [] retBlk ()) ()))
  , testFlatten "static move |x: i32| { return 1; }"
                (printExpr (Closure [] Immovable Value (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing False ()) 
                                    (BlockExpr [] retBlk ()) ()))
  , testFlatten "|x: i32| -> i32 { return 1; }"
                (printExpr (Closure [] Movable Ref (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] (Just i32) False ()) 
                                    (BlockExpr [] retBlk ()) ()))
  , testFlatten "static |x: i32| -> i32 { return 1; }"
                (printExpr (Closure [] Immovable Ref (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] (Just i32) False ()) 
                                    (BlockExpr [] retBlk ()) ()))
  , testFlatten "#[cfgo] { #![cfgi] return 1; }" (printExpr (BlockExpr [cfgI,cfgO] retBlk ()))
  , testFlatten "{ return 1; }" (printExpr (BlockExpr [] retBlk ()))
  , testFlatten "do catch { return 1; }" (printExpr (Catch [] retBlk ()))
  , testFlatten "foo = 1" (printExpr (Assign [] foo _1 ()))
  , testFlatten "foo += 1" (printExpr (AssignOp [] AddOp foo _1 ()))
  , testFlatten "foo <<= 1" (printExpr (AssignOp [] ShlOp foo _1 ()))
  , testFlatten "foo.bar" (printExpr (FieldAccess [] foo (mkIdent "bar") ()))
  , testFlatten "foo.1" (printExpr (TupField [] foo 1 ()))
  , testFlatten "foo[1]" (printExpr (Index [] foo _1 ()))
  , testFlatten "foo .. bar" (printExpr (Range [] (Just foo) (Just bar) HalfOpen ()))
  , testFlatten "foo ..=" (printExpr (Range [] (Just foo) Nothing Closed ()))
  , testFlatten ".. bar" (printExpr (Range [] Nothing (Just bar) HalfOpen ()))
  , testFlatten "..=" (printExpr (Range [] Nothing Nothing Closed ()))
  , testFlatten "std::vec::Vec::<i32>" (printExpr (PathExpr [] Nothing (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "<i32 as std::vec>::Vec::<i32>" (printExpr (PathExpr [] (Just (QSelf i32 2)) (Path False [ std, vec, veci32 ] ()) ()))
  , testFlatten "&foo" (printExpr (AddrOf [] Immutable foo ()))
  , testFlatten "#[cfgo] &mut foo" (printExpr (AddrOf [cfgO] Mutable foo ()))
  , testFlatten "break" (printExpr (Break [] Nothing Nothing ()))
  , testFlatten "break 1" (printExpr (Break [] Nothing (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) ()))
  , testFlatten "break 'foo" (printExpr (Break [] (Just (Label "foo" ())) Nothing ()))
  , testFlatten "break 'foo 1" (printExpr (Break [] (Just (Label "foo" ())) (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) ()))
  , testFlatten "continue" (printExpr (Continue [] Nothing ()))
  , testFlatten "continue 'foo" (printExpr (Continue [] (Just (Label "foo" ())) ()))
  , testFlatten "return" (printExpr (Ret [] Nothing ()))
  , testFlatten "return foo" (printExpr (Ret [] (Just foo) ()))
  , testFlatten "print!(foo)" (printExpr (MacExpr [] (Mac (Path False [PathSegment "print" Nothing ()] ())
                                       (Stream [ Tree (Token mempty (IdentTok (mkIdent "foo"))) ]) ()) ()))
  , testFlatten "foo { }" (printExpr (Struct [] (Path False [PathSegment "foo" Nothing ()] ()) [] Nothing ()))
  , testFlatten "foo { x: 1 }" (printExpr (Struct [] (Path False [PathSegment "foo" Nothing ()] ()) [Field (mkIdent "x") (Just _1) ()] Nothing ()))
  , testFlatten "#[cfgo] foo { #![cfgi] x: 1 }" (printExpr (Struct [cfgO,cfgI] (Path False [PathSegment "foo" Nothing ()] ()) [Field (mkIdent "x") (Just _1) ()] Nothing ()))
  , testFlatten "foo { x: 1, y: 1 }" (printExpr (Struct [] (Path False [PathSegment "foo" Nothing ()] ()) [Field (mkIdent "x") (Just _1) (), Field (mkIdent "y") (Just _1) ()] Nothing ()))
  , testFlatten "foo { x: 1, y: 1, ..bar }" (printExpr (Struct [] (Path False [PathSegment "foo" Nothing ()] ()) [Field (mkIdent "x") (Just _1) (), Field (mkIdent "y") (Just _1) ()] (Just bar) ()))
  , testFlatten "#[cfgo] foo { #![cfgi] x, y, ..bar }" (printExpr (Struct [cfgO,cfgI] (Path False [PathSegment "foo" Nothing ()] ()) [Field (mkIdent "x") Nothing (), Field (mkIdent "y") Nothing ()] (Just bar) ()))
  , testFlatten "[foo; 1]" (printExpr (Repeat [] foo _1 ()))
  , testFlatten "#[cfgo] [#![cfgi] foo; 1]" (printExpr (Repeat [cfgI, cfgO] foo _1 ()))
  , testFlatten "(foo?)" (printExpr (ParenExpr [] (Try [] foo ()) ()))
  , testFlatten "foo?" (printExpr (Try [] foo ()))
  ]

-- | Test pretty printing of items (flattened)
prettyItems :: Test
prettyItems = testGroup "printing items"
  [ testFlatten "extern crate \"rustc-serialize\" as rustc_serialize;" (printItem (ExternCrate [] InheritedV (mkIdent "rustc_serialize") (Just (mkIdent "rustc-serialize")) ()))
  , testFlatten "pub extern crate rustc_serialize;" (printItem (ExternCrate [] PublicV (mkIdent "rustc_serialize") Nothing ()))
  , testFlatten "#[cfgo] pub extern crate rustc_serialize;" (printItem (ExternCrate [cfgO] PublicV (mkIdent "rustc_serialize") Nothing ()))
  , testFlatten "use std::vec as baz;" (printItem (Use [] InheritedV (UseTreeSimple (Path False [std,vec] ()) (Just "baz") ()) ()))
  , testFlatten "use std::vec::*;" (printItem (Use [] InheritedV (UseTreeGlob (Path False [std,vec] ()) ()) ()))
  , testFlatten "use std::vec::{a as b, c};" (printItem (Use [] InheritedV (UseTreeNested (Path False [std,vec] ())
                                                                                          [ UseTreeSimple (Path False [PathSegment "a" Nothing ()] ()) (Just "b") ()
                                                                                          , UseTreeSimple (Path False [PathSegment "c" Nothing ()] ()) Nothing ()
                                                                                          ] ()) ()))
  , testFlatten "static mut foo: i32 = 1;" (printItem (Static [] InheritedV (mkIdent "foo") i32 Mutable _1 ()))
  , testFlatten "static foo: i32 = 1;" (printItem (Static [] InheritedV (mkIdent "foo") i32 Immutable _1 ()))
  , testFlatten "const foo: i32 = 1;" (printItem (ConstItem [] InheritedV (mkIdent "foo") i32 _1 ()))
  , testFlatten "fn foo(x: i32) -> i32 { return 1; }" (printItem (Fn [] InheritedV (mkIdent "foo") (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] (Just i32) False ()) Normal NotConst Rust (Generics [] [] (WhereClause [] ()) ()) retBlk ()))
  , testFlatten "unsafe fn foo(x: i32, ...) { return 1; }" (printItem (Fn [] InheritedV (mkIdent "foo") (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing True ()) Unsafe NotConst Rust (Generics [] [] (WhereClause [] ()) ()) retBlk ()))
  , testFlatten "const unsafe extern \"C\" fn foo(x: i32) { return 1; }" (printItem (Fn [] InheritedV (mkIdent "foo") (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing False ()) Unsafe Const C (Generics [] [] (WhereClause [] ()) ()) retBlk ()))
  , testFlatten "fn foo<'lt: 'foo + 'bar, T, U: Debug + 'lt = i32>(x: i32) { return 1; }" (printItem (Fn [] InheritedV (mkIdent "foo") (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing False ()) Normal NotConst Rust (Generics [LifetimeDef [] (Lifetime "lt" ()) [Lifetime "foo" (), Lifetime "bar" ()] ()] [TyParam [] (mkIdent "T") [] Nothing (), TyParam [] (mkIdent "U") [debug', lt] (Just i32) ()] (WhereClause [] ()) ()) retBlk ()))
  , testFlatten "fn foo<T, U: Debug + 'lt = i32>(x: i32) where for<'lt> i32: Debug, 'foo: 'bar, vec::std = i32 { return 1; }" (printItem (Fn [] InheritedV (mkIdent "foo") (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] Nothing False ()) Normal NotConst Rust (Generics [] [TyParam [] (mkIdent "T") [] Nothing (), TyParam [] (mkIdent "U") [debug', lt] (Just i32) ()] (WhereClause [BoundPredicate [LifetimeDef [] (Lifetime "lt" ()) [] ()] i32 [debug'] (), RegionPredicate (Lifetime "foo" ()) [Lifetime "bar" ()] (), EqPredicate (PathTy Nothing (Path False [vec,std] ()) ()) i32 ()] ()) ()) retBlk ()))
  , testFlatten "mod serialize;" (printItem (Mod [] InheritedV (mkIdent "serialize") Nothing ()))
  , testFlatten "mod serialize { }" (printItem (Mod [] InheritedV (mkIdent "serialize") (Just []) ()))
  , testFlatten "mod serialize { const foo: i32 = 1; }" (printItem (Mod [] InheritedV (mkIdent "serialize") (Just [ConstItem [] InheritedV (mkIdent "foo") i32 _1 ()]) ()))
  , testFlatten "#[cfgo] mod serialize { #![cfgi] const foo: i32 = 1; }" (printItem (Mod [cfgO,cfgI] InheritedV (mkIdent "serialize") (Just [ConstItem [] InheritedV (mkIdent "foo") i32 _1 ()]) ()))
  , testFlatten "extern \"C\" { }" (printItem (ForeignMod [] InheritedV C [] ()))
  , testFlatten "extern \"C\" { static mut foo: i32; }" (printItem (ForeignMod [] InheritedV C [ForeignStatic [] InheritedV (mkIdent "foo") i32 Mutable ()] ()))
  , testFlatten "#[cfgo] extern \"C\" { #![cfgi] fn foo(x: i32) -> i32; }" (printItem (ForeignMod [cfgO,cfgI] InheritedV C [ForeignFn [] InheritedV (mkIdent "foo") (FnDecl [Arg (Just (IdentP (ByValue Immutable) (mkIdent "x") Nothing ())) i32 ()] (Just i32) False ()) (Generics [] [] (WhereClause [] ()) ()) ()] ()))
  , testFlatten "#[cfgo] extern \"C\" { #![cfgi] static foo: i32; }" (printItem (ForeignMod [cfgO,cfgI] InheritedV C [ForeignStatic [] InheritedV (mkIdent "foo") i32 Immutable ()] ()))
  , testFlatten "type Vec<T> = i32;" (printItem (TyAlias [] InheritedV (mkIdent "Vec") i32 (Generics [] [TyParam [] (mkIdent "T") [] Nothing ()] (WhereClause [] ()) ()) ()))
  , testFlatten "enum foo<T> { }" (printItem (Enum [] InheritedV (mkIdent "foo") [] (Generics [] [TyParam [] (mkIdent "T") [] Nothing ()] (WhereClause [] ()) ()) ()))
  , testFlatten "enum color { #[cfgo] red { i32 } = 1, blue(i32), green }" (printItem (Enum [] InheritedV (mkIdent "color")
       [ Variant (mkIdent "red") [cfgO] (StructD [StructField Nothing InheritedV i32 [] ()] ()) (Just _1) ()
       , Variant (mkIdent "blue") [] (TupleD [StructField Nothing InheritedV i32 [] ()] ()) Nothing ()
       , Variant (mkIdent "green") [] (UnitD ()) Nothing ()]
       (Generics [] [] (WhereClause [] ()) ()) ()))
  , testFlatten "struct red { x: i32 }" (printItem (StructItem [] InheritedV (mkIdent "red") (StructD [StructField (Just (mkIdent "x")) InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ()) ()))
  , testFlatten "union red { x: i32 }" (printItem (Union [] InheritedV (mkIdent "red") (StructD [StructField (Just (mkIdent "x")) InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ()) ()))
  , testFlatten "union red { x: i32 }" (printItem (Union [] InheritedV (mkIdent "red") (StructD [StructField (Just (mkIdent "x")) InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ()) ()))
  , testFlatten "impl Debug for i32 { }" (printItem (Impl [] InheritedV Final Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [debug] ()))) i32 [] ()))
  , testFlatten "default impl Debug for i32 { }" (printItem (Impl [] InheritedV Default Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef (Path False [debug] ()))) i32 [] ()))
  , testFlatten "pub impl !Debug for i32 where 'lt: 'gt { }" (printItem (Impl [] PublicV Final Normal Negative (Generics [] [] (WhereClause [RegionPredicate (Lifetime "lt" ()) [Lifetime "gt" ()] ()] ()) ()) (Just (TraitRef (Path False [debug] ()))) i32 [] ()))
  , testFlatten "impl<T> GenVal<T> { fn value(&mut self) -> &T { return 1; } }" 
                (printItem (Impl [] InheritedV Final Normal Positive
                      (Generics [] [TyParam [] (mkIdent "T") [] Nothing ()] (WhereClause [] ()) ())
                      Nothing
                      (PathTy Nothing (Path False [PathSegment "GenVal" (Just (AngleBracketed [] [PathTy Nothing (Path False [PathSegment "T" Nothing ()] ()) ()] [] ())) ()] ()) ())
                      [ MethodI [] InheritedV Final (mkIdent "value")
                          (Generics [] [] (WhereClause [] ()) ())
                          (MethodSig Normal NotConst Rust
                                      (FnDecl [SelfRegion Nothing Mutable ()]
                                              (Just (Rptr Nothing Immutable (PathTy Nothing (Path False [PathSegment "T" Nothing ()] ()) ()) ())) 
                                              False ())) 
                          retBlk
                          ()
                      ]
                      ()))
  , testFlatten "#[cfgo] impl i32 { #![cfgi] fn value(&self) -> i32 { return 1; } pub const pi: i32 = 1; default type Size = i32; }" 
                (printItem (Impl [cfgI,cfgO] InheritedV Final Normal Positive
                      (Generics [] [] (WhereClause [] ()) ())
                      Nothing
                      i32
                      [ MethodI [] InheritedV Final (mkIdent "value")
                          (Generics [] [] (WhereClause [] ()) ())
                          (MethodSig Normal NotConst Rust
                                      (FnDecl [SelfRegion Nothing Immutable ()]
                                              (Just i32) 
                                              False ())) 
                          retBlk
                          ()
                      , ConstI [] PublicV Final (mkIdent "pi") i32 _1 ()
                      , TypeI [] InheritedV Default (mkIdent "Size") i32 ()
                      ]
                      ()))
  , testFlatten "unsafe trait Show { }" (printItem (Trait [] InheritedV (mkIdent "Show") False Unsafe (Generics [] [] (WhereClause [] ()) ()) [] [] ()))
  , testFlatten "trait Show<T>: 'l1 + for<'l3: 'l1 + 'l2> Debug + 'l2 { }" (printItem (Trait [] InheritedV (mkIdent "Show") False Normal
                                              (Generics [] [TyParam [] (mkIdent "T") [] Nothing ()] (WhereClause [] ()) ())
                                              [ RegionTyParamBound (Lifetime "l1" ()) ()
                                              , TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime "l3" ()) [Lifetime "l1" (), Lifetime "l2" ()] ()] (TraitRef (Path False [debug] ())) ())  None ()
                                              , RegionTyParamBound (Lifetime "l2" ()) ()]
                                              []
                                              ()))
  , testFlatten "pub trait Show { fn value(&mut self) -> i32; const pi: i32 = 1; const e: i32; type Size = i32; type Length: 'l3; type SomeType: 'l1 = f64; }"
                (printItem (Trait [] PublicV (mkIdent "Show") False Normal (Generics [] [] (WhereClause [] ()) ()) []
                      [ MethodT [] (mkIdent "value")
                          (Generics [] [] (WhereClause [] ()) ())
                          (MethodSig Normal NotConst Rust
                                      (FnDecl [SelfRegion Nothing Mutable ()]
                                              (Just i32) 
                                              False ())) 
                          Nothing
                          ()
                      , ConstT [] (mkIdent "pi") i32 (Just _1) ()
                      , ConstT [] (mkIdent "e") i32 Nothing ()
                      , TypeT [] (mkIdent "Size") [] (Just i32) ()
                      , TypeT [] (mkIdent "Length") [RegionTyParamBound (Lifetime "l3" ()) ()] Nothing ()
                      , TypeT [] (mkIdent "SomeType") [RegionTyParamBound (Lifetime "l1" ()) ()] (Just f64) ()
                      ]
                      ()))
  ]

-- | Test pretty printing of statements (flattened). 
prettyStatements :: Test
prettyStatements = testGroup "printing statements"
  [ testFlatten "#[cfgo] let _;" (printStmt (Local (WildP ()) Nothing Nothing [cfgO] ()))
  , testFlatten "let _: i32;" (printStmt (Local (WildP ()) (Just i32) Nothing [] ()))
  , testFlatten "let _: i32 = 1;" (printStmt (Local (WildP ()) (Just i32) (Just _1) [] ()))
  , testFlatten "extern crate rustc_serialize;" (printStmt (ItemStmt (ExternCrate [] InheritedV (mkIdent "rustc_serialize") Nothing ()) ())) 
  , testFlatten "if foo { foo = 1 }" (printStmt (NoSemi (If [] foo assBlk Nothing ()) ()))
  , testFlatten "{ return 1; }" (printStmt (NoSemi (BlockExpr [] retBlk ()) ()))
  , testFlatten "foo;" (printStmt (NoSemi foo ()))
  , testFlatten "1;" (printStmt (NoSemi _1 ()))
  , testFlatten "if foo { foo = 1 };" (printStmt (Semi (If [] foo assBlk Nothing ()) ()))
  , testFlatten "{ return 1; };" (printStmt (Semi (BlockExpr [] retBlk ()) ()))
  , testFlatten "foo;" (printStmt (Semi foo ()))
  , testFlatten "1;" (printStmt (Semi _1 ()))
  , testFlatten "#[cfgo] println!(foo);" (printStmt (MacStmt (Mac (Path False [println] ()) (Stream [ Tree (Token mempty (IdentTok (mkIdent "foo"))) ]) ()) SemicolonMac [cfgO] ()))
  , testFlatten "println!(foo);" (printStmt (MacStmt (Mac (Path False [println] ()) (Stream [ Tree (Token mempty (IdentTok (mkIdent "foo"))) ]) ()) SemicolonMac [] ()))
  , testFlatten "println!{ foo }" (printStmt (MacStmt (Mac (Path False [println] ()) (Stream [ Tree (Token mempty (IdentTok (mkIdent "foo"))) ]) ()) BracesMac [] ()))
  ]
  
-- | This tries to make it so that the `Doc` gets rendered onto only one line.
testFlatten :: String -> Doc a -> Test
testFlatten str doc = testCase (escapeNewlines str) $ str @=? renderShowS (layoutPretty (LayoutOptions Unbounded) (flatten doc)) ""



-- | Utility function for escaping newlines (and only newlines)
escapeNewlines :: String -> String
escapeNewlines = concatMap (\c -> if c == '\n' then "\\n" else [c])


