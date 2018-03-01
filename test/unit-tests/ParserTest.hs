{-
The name of this module is very slightly misleading - on top of just asserting correct parses, it
also checks that calling 'resolve' on the parsed output is a NOP and that re-parsing the printed
output is the same. This doesn't fully check that 'resolve' works - but it does provide some checks
that it isn't grossly broken. Plus it adds some more pretty-printing checks!
-}
{-# LANGUAGE OverloadedStrings, OverloadedLists, UnicodeSyntax, FlexibleContexts, ScopedTypeVariables #-}
module ParserTest (parserSuite) where

import Test.Framework (testGroup, Test, TestName)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test, Path, Label)

import Language.Rust.Parser
import Language.Rust.Pretty
import Language.Rust.Syntax
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Data.InputStream

import Control.Monad
import Data.Maybe (catMaybes)

import Data.Typeable
import Data.Data

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
testP :: forall f. 
         (Parse (f Span), Pretty (f ()), Resolve (f ()), Functor f, Show (f ()), Eq (f ()), Data (f Span)) =>
         TestName -> f () -> Test
testP inp y = testCase inp $ do
    -- parse test
    Right y @=? parseNoSpans parser inps
  
    -- resolve test
    Right y @=? either (\(ResolveFail _ msg) -> Left msg) Right (resolve y)
  
    -- re-parse the result of printing resolve
    Right y @=? case pretty y of
                  Left (ResolveFail _ msg) -> Left msg
                  Right inp' -> parseNoSpans parser (inputStreamFromString (show inp')) 
  
    -- check that the sub-spans re-parse correctly
    let Right y' = parse inps
    checkSubterms inps (y' :: f Span)
  where
  inps = inputStreamFromString inp


-- | Given the initial input stream and a value, check if that value is one of the ones we test for.
-- If that is the case /and/ the value is locatable, try extracting the appropriate slice from the
-- input stream and check that it parses to the same thing.
--
-- NOTE: statements are a problem since the last statement in a block can be an expression which, by
-- itself, is not a statement.
checkTerm :: Typeable a => InputStream -> a -> IO ()
checkTerm inp y = sequence_ $ catMaybes tests
  where
  tests = [ checkTerm' (Proxy :: Proxy Lit) inp <$> cast y
          , checkTerm' (Proxy :: Proxy Attribute) inp <$> cast y
          , checkTerm' (Proxy :: Proxy Ty) inp <$> cast y
          , checkTerm' (Proxy :: Proxy Pat) inp <$> cast y
          , checkTerm' (Proxy :: Proxy Expr) inp <$> cast y
       -- , checkTerm' (Proxy :: Proxy Stmt) inp <$> cast y  
          , checkTerm' (Proxy :: Proxy Item) inp <$> cast y
       -- , checkTerm' (Proxy :: Proxy TyParamBound) inp <$> cast y
          , checkTerm' (Proxy :: Proxy TyParam) inp <$> cast y
          , checkTerm' (Proxy :: Proxy TraitItem) inp <$> cast y
          , checkTerm' (Proxy :: Proxy ImplItem) inp <$> cast y
          , checkTerm' (Proxy :: Proxy LifetimeDef) inp <$> cast y
          , checkTerm' (Proxy :: Proxy Block) inp <$> cast y
          ]

  -- | Check that a given term slice re-parses properly
  checkTerm' :: ( Functor f
                , Show (f ()), Eq (f ())
                , Pretty (f Span), Parse (f Span), Located (f Span)
                ) => Proxy f -> InputStream -> f Span -> IO ()
  checkTerm' Proxy inp' y' = case slice (spanOf y') (inputStreamToString inp') of
                               Nothing -> pure ()
                               Just inp'' -> Right (void y') @=? parseNoSpans parser (inputStreamFromString inp'')

  -- | Take a sub-slice of an input string
  slice :: Span -> String -> Maybe String
  slice (Span (Position s _ _) (Position e _ _)) str = Just . take (e - s) . drop s $ str
  slice _ _ = Nothing

 
checkSubterms :: Data a => InputStream -> a -> IO ()
checkSubterms inp y = checkTerm inp y *> gmapQl (*>) (pure ()) (checkSubterms inp) y
 

-- | Turn an InputStream into either an error or a parse.
parseNoSpans :: Functor f => P (f Span) -> InputStream -> Either String (f ())
parseNoSpans p inp = case void <$> execParser p inp initPos of
                       Left (ParseFail _ msg) -> Left msg
                       Right y -> Right y


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
  , testP "123" (Int Dec 123 Unsuffixed ())
  , testP "123i32" (Int Dec 123 I32 ())
  , testP "0b1100_1101" (Int Bin 205 Unsuffixed ())
  , testP "0b1100_1101isize" (Int Bin 205 Is ())
  , testP "0o3170" (Int Oct 1656 Unsuffixed ())
  , testP "0o3170i64" (Int Oct 1656 I64 ())
  , testP "0xAFAC" (Int Hex 44972 Unsuffixed ())
  , testP "0xAFACu32" (Int Hex 44972 U32 ())
  -- float's
  , testP "123." (Float 123.0 Unsuffixed ())
  , testP "123.1" (Float 123.1 Unsuffixed ())
  , testP "123.0f32" (Float 123.0 F32 ())
  , testP "123.1f32" (Float 123.1 F32 ())
  , testP "123e-9f32" (Float 123e-9 F32 ())
  -- string's
  , testP "\"hello \\n world!\"" (Str "hello \n world!" Cooked Unsuffixed ())
  , testP "r\"hello \n world!\"" (Str "hello \n world!" (Raw 0) Unsuffixed ()) 
  , testP "r##\"hello \"#\n world!\"##" (Str "hello \"#\n world!" (Raw 2) Unsuffixed ())
  -- bytestring's
  , testP "b\"hello \\n world!\"" (byteStr "hello \n world!" Cooked Unsuffixed ())
  , testP "br\"hello \n world!\"" (byteStr "hello \n world!" (Raw 0) Unsuffixed ())
  , testP "br##\"hello \"#\n world!\"##" (byteStr "hello \"#\n world!" (Raw 2) Unsuffixed ())
  -- multiline strings
  , testP "\"hello \\\n     world!\"" (Str "hello world!" Cooked Unsuffixed ())
  , testP "b\"hello \\\n     world!\"" (byteStr "hello world!" Cooked Unsuffixed ())
  ]

cfg, cfgi, cfgo, derive, feature, inline, self, debug, send, clone, point, x, t :: Path ()
cfg = Path False [PathSegment "cfg" Nothing ()] ()
cfgi = Path False [PathSegment "cfgi" Nothing ()] ()
cfgo = Path False [PathSegment "cfgo" Nothing ()] ()
derive = Path False [PathSegment "derive" Nothing ()] ()
feature = Path False [PathSegment "feature" Nothing ()] ()
inline = Path False [PathSegment "inline" Nothing ()] ()
self = Path False [PathSegment "self" Nothing ()] ()
debug = Path False [PathSegment "Debug" Nothing ()] ()
send = Path False [PathSegment "Send" Nothing ()] ()
clone = Path False [PathSegment "Clone" Nothing ()] ()
point = Path False [PathSegment "Point" Nothing ()] ()
x = Path False [PathSegment "x" Nothing ()] ()
t = Path False [PathSegment "T" Nothing ()] ()


-- | Test parsing of (inner and outer) attributes.
parserAttributes :: Test
parserAttributes = testGroup "parsing attributes" 
  [ testP "#![cfgi]" (Attribute Inner cfgi (Stream []) ())
  , testP "#[cfgo]" (Attribute Outer cfgo (Stream []) ())
  , testP "#[derive(Eq, Ord, 1)]" (Attribute Outer derive (Tree (Delimited (Span (Position 8 1 8) (Position 20 1 20)) Paren (Stream
                                                          [ Tree (Token (Span (Position 9 1 9) (Position 11 1 11)) (IdentTok "Eq"))
                                                          , Tree (Token (Span (Position 11 1 11) (Position 12 1 12)) Comma)
                                                          , Tree (Token (Span (Position 13 1 13) (Position 16 1 16)) (IdentTok "Ord"))
                                                          , Tree (Token (Span (Position 16 1 16) (Position 17 1 17)) Comma)
                                                          , Tree (Token (Span (Position 18 1 18) (Position 19 1 19)) (LiteralTok (IntegerTok "1") Nothing))
                                                          ]))) ())
  , testP "#[feature = \"foo\"]" (Attribute Outer feature (Stream
                                                          [ Tree (Token (Span (Position 10 1 10) (Position 11 1 11)) Equal)
                                                          , Tree (Token (Span (Position 12 1 12) (Position 17 1 17)) (LiteralTok (StrTok "foo") Nothing))
                                                          ]) ())
  , testP "/** some comment */" (SugaredDoc Outer True " some comment " ())
  , testP "#[test]" (Attribute Outer (Path False [PathSegment "test" Nothing ()] ()) (Stream []) ())
  , testP "#[inline(always)]" (Attribute Outer inline (Tree (Delimited (Span (Position 8 1 8) (Position 16 1 16)) Paren
                                                          (Tree (Token (Span (Position 9 1 9) (Position 15 1 15)) (IdentTok "always"))
                                                          ))) ())
  , testP "#[inline(always,)]" (Attribute Outer inline (Tree (Delimited (Span (Position 8 1 8) (Position 17 1 17)) Paren (Stream
                                                          [ Tree (Token (Span (Position 9 1 9) (Position 15 1 15)) (IdentTok "always"))
                                                          , Tree (Token (Span (Position 15 1 15) (Position 16 1 16)) Comma)
                                                          ]))) ())
  , testP "#[inline()]" (Attribute Outer inline (Tree (Delimited (Span (Position 8 1 8) (Position 10 1 10)) Paren (Stream
                                                          [
                                                          ]))) ())
  , testP "#[inline(always,sometimes)]" (Attribute Outer inline (Tree (Delimited (Span (Position 8 1 8) (Position 26 1 26)) Paren (Stream
                                                          [ Tree (Token (Span (Position 9 1 9) (Position 15 1 15)) (IdentTok "always"))
                                                          , Tree (Token (Span (Position 15 1 15) (Position 16 1 16)) Comma)
                                                          , Tree (Token (Span (Position 16 1 16) (Position 25 1 25)) (IdentTok "sometimes"))
                                                          ]))) ())
  , testP "#[self(if, default)]" (Attribute Outer self (Tree (Delimited (Span (Position 6 1 6) (Position 19 1 19)) Paren (Stream
                                                          [ Tree (Token (Span (Position 7 1 7) (Position 9 1 9)) (IdentTok "if"))
                                                          , Tree (Token (Span (Position 9 1 9) (Position 10 1 10)) Comma)
                                                          , Tree (Token (Span (Position 11 1 11) (Position 18 1 18)) (IdentTok "default"))
                                                          ]))) ())
  , testP "#[cfg(target_os = \"mac_os\", 1)]" (Attribute Outer cfg (Tree (Delimited (Span (Position 5 1 5) (Position 30 1 30)) Paren (Stream
                                                          [ Tree (Token (Span (Position 6 1 6) (Position 15 1 15)) (IdentTok "target_os"))
                                                          , Tree (Token (Span (Position 16 1 16) (Position 17 1 17)) Equal)
                                                          , Tree (Token (Span (Position 18 1 18) (Position 26 1 26)) (LiteralTok (StrTok "mac_os") Nothing))
                                                          , Tree (Token (Span (Position 26 1 26) (Position 27 1 27)) Comma)
                                                          , Tree (Token (Span (Position 28 1 28) (Position 29 1 29)) (LiteralTok (IntegerTok "1") Nothing))
                                                          ]))) ())
  ]


-- | Test parsing of types.
parserTypes :: Test
parserTypes = testGroup "parsing types"
  [ testP "_" (Infer ())
  , testP "!" (Never ())
  , testP "i32" i32
  , testP "Self" (PathTy Nothing (Path False [PathSegment "Self" Nothing ()] ()) ())
  , testP "?Debug" (TraitObject [TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) Maybe ()] ())
  , testP "Debug + ?Send + 'a" (TraitObject [ TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) None ()
                                            , TraitTyParamBound (PolyTraitRef [] (TraitRef send) ()) Maybe ()
                                            , RegionTyParamBound (Lifetime "a" ()) ()
                                            ] ())
  , testP "?for<'a> Debug" (TraitObject [TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime "a" ()) [] ()] (TraitRef debug) ()) Maybe ()] ())
  , testP "?for<'a> Debug + 'a" (TraitObject [ TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime "a" ()) [] ()] (TraitRef debug) ()) Maybe ()
                                             , RegionTyParamBound (Lifetime "a" ()) ()
                                             ] ())
  , testP "Send + ?for<'a> Debug + 'a" (TraitObject
                                             [ TraitTyParamBound (PolyTraitRef [] (TraitRef send) ()) None ()
                                             , TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime "a" ()) [] ()] (TraitRef debug) ()) Maybe ()
                                             , RegionTyParamBound (Lifetime "a" ()) ()
                                             ] ())
  , testP "(i32,)" (TupTy [i32] ())
  , testP "(i32,())" (TupTy [i32, TupTy [] ()] ())
  , testP "()" (TupTy [] ())
  , testP "[_]" (Slice (Infer ()) ())
  , testP "[i32]" (Slice i32 ())
  , testP "[i32; 17]" (Array i32 (Lit [] (Int Dec 17 Unsuffixed ()) ()) ())
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
  , testP "typeof(123)" (Typeof (Lit [] (Int Dec 123 Unsuffixed ()) ()) ())
  , testP "Vec<i32>" (PathTy Nothing (Path False [PathSegment "Vec" (Just (AngleBracketed [] [i32] [] ())) ()] ()) ())
  , testP "Vec<i32>" (PathTy Nothing (Path False [PathSegment "Vec" (Just (AngleBracketed [] [i32] [] ())) ()] ()) ())
  , testP "Vec<<i32 as a>::b,i32>" (PathTy Nothing (Path False [PathSegment "Vec" (Just (AngleBracketed [] [ PathTy (Just (QSelf i32 1))
                                                                                                   (Path False [ PathSegment "a" Nothing ()
                                                                                                               , PathSegment "b" Nothing ()
                                                                                                               ] ()) ()
                                                                                          , i32] [] ())) ()] ()) ())
  , testP "Vec< <i32 as a>::b,i32>" (PathTy Nothing (Path False [PathSegment "Vec" (Just (AngleBracketed [] [ PathTy (Just (QSelf i32 1))
                                                                                                   (Path False [ PathSegment "a" Nothing ()
                                                                                                               , PathSegment "b" Nothing ()
                                                                                                               ] ()) ()
                                                                                          , i32] [] ())) ()] ()) ())
  , testP "std::vec::Vec<T>" (PathTy Nothing (Path False [ PathSegment "std" Nothing () 
                                                         , PathSegment "vec" Nothing () 
                                                         , PathSegment "Vec" (Just (AngleBracketed [] [PathTy Nothing t ()] [] ())) ()
                                                         ] ()) ())
  , testP "foo::baz<'a,T,B=!>" (PathTy Nothing (Path False [ PathSegment "foo" Nothing ()
                                                           , PathSegment "baz" (Just (AngleBracketed [Lifetime "a" ()] 
                                                                                            [PathTy Nothing t () ]
                                                                                            [(mkIdent "B", Never ())] ())) ()
                                                           ] ()) ())
  , testP "Foo(!,!)"       (PathTy Nothing (Path False [ PathSegment "Foo" (Just (Parenthesized [Never (), Never ()] Nothing ())) () ] ()) ())
  , testP "Foo(!,!,)"      (PathTy Nothing (Path False [ PathSegment "Foo" (Just (Parenthesized [Never (), Never ()] Nothing ())) () ] ()) ())
  , testP "Foo(!,!) -> !"  (PathTy Nothing (Path False [ PathSegment "Foo" (Just (Parenthesized [Never (), Never ()] (Just (Never ())) ())) () ] ()) ())
  , testP "Foo(!,!,) -> !" (PathTy Nothing (Path False [ PathSegment "Foo" (Just (Parenthesized [Never (), Never ()] (Just (Never ())) ())) () ] ()) ())
  , testP "<i32 as a>::b"
             (PathTy (Just (QSelf i32 1)) (Path False [ PathSegment "a" Nothing ()
                                                      , PathSegment "b" Nothing ()
                                                      ] ()) ())
  , testP "<i32 as a::b::Trait>::AssociatedItem"
             (PathTy (Just (QSelf i32 3)) (Path False [ PathSegment "a" Nothing ()
                                                      , PathSegment "b" Nothing ()
                                                      , PathSegment "Trait" Nothing ()
                                                      , PathSegment "AssociatedItem" Nothing ()
                                                      ] ()) ())
  , testP "< <i32 as a::b::Trait>::AssociatedItem as x>::Another"
             (PathTy (Just (QSelf (PathTy (Just (QSelf i32 3)) (Path False [ PathSegment "a" Nothing ()
                                                      , PathSegment "b" Nothing ()
                                                      , PathSegment "Trait" Nothing ()
                                                      , PathSegment "AssociatedItem" Nothing ()
                                                      ] ()) ()) 1))
                     (Path False [ PathSegment "x" Nothing ()
                                 , PathSegment "Another" Nothing ()
                                 ] ())
                     ())
  , testP "<<i32 as a::b::Trait>::AssociatedItem as x>::Another"
             (PathTy (Just (QSelf (PathTy (Just (QSelf i32 3)) (Path False [ PathSegment "a" Nothing ()
                                                      , PathSegment "b" Nothing ()
                                                      , PathSegment "Trait" Nothing ()
                                                      , PathSegment "AssociatedItem" Nothing ()
                                                      ] ()) ()) 1))
                     (Path False [ PathSegment "x" Nothing ()
                                 , PathSegment "Another" Nothing ()
                                 ] ())
                     ())
  , testP "< <Debug + 'static as a::b::Trait>::AssociatedItem as x>::Another"
             (PathTy (Just (QSelf (PathTy (Just (QSelf
                         (TraitObject [ TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) None ()
                                      , RegionTyParamBound (Lifetime "static" ()) ()
                                      ] ()) 3)) 
                                                (Path False [ PathSegment "a" Nothing ()
                                                      , PathSegment "b" Nothing ()
                                                      , PathSegment "Trait" Nothing ()
                                                      , PathSegment "AssociatedItem" Nothing ()
                                                      ] ()) ()) 1))
                     (Path False [ PathSegment "x" Nothing ()
                                 , PathSegment "Another" Nothing ()
                                 ] ())
                     ())
  , testP "<<Debug + 'static as a::b::Trait>::AssociatedItem as x>::Another"
             (PathTy (Just (QSelf (PathTy (Just (QSelf
                         (TraitObject [ TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) None ()
                                      , RegionTyParamBound (Lifetime "static" ()) ()
                                      ] ()) 3)) 
                                                (Path False [ PathSegment "a" Nothing ()
                                                      , PathSegment "b" Nothing ()
                                                      , PathSegment "Trait" Nothing ()
                                                      , PathSegment "AssociatedItem" Nothing ()
                                                      ] ()) ()) 1))
                     (Path False [ PathSegment "x" Nothing ()
                                 , PathSegment "Another" Nothing ()
                                 ] ())
                     ())
  , testP "<i32 as a(i32, i32)::b<'lt>::Trait(i32) -> i32>::AssociatedItem"
             (PathTy (Just (QSelf i32 3)) (Path False [ PathSegment "a" (Just (Parenthesized [i32, i32] Nothing ())) ()
                                                      , PathSegment "b" (Just (AngleBracketed [Lifetime "lt" ()] [] [] ())) ()
                                                      , PathSegment "Trait" (Just (Parenthesized [i32] (Just i32) ())) ()
                                                      , PathSegment "AssociatedItem" Nothing ()
                                                      ] ()) ())
  , testP "extern fn(i32,...)"
             (BareFn Normal C [] (FnDecl [Arg Nothing i32 ()] Nothing True ()) ())
  , testP "fn(i32) -> i32"
             (BareFn Normal Rust [] (FnDecl [Arg Nothing i32 ()] (Just i32) False ()) ())
  , testP "fn(i32,) -> i32"
             (BareFn Normal Rust [] (FnDecl [Arg Nothing i32 ()] (Just i32) False ()) ())
  , testP "fn(_: i32) -> i32"
             (BareFn Normal Rust [] (FnDecl [Arg (Just (WildP ())) i32 ()] (Just i32) False ()) ())
  , testP "unsafe extern \"C\" fn(_: i32)"
             (BareFn Unsafe C [] (FnDecl [Arg (Just (WildP ())) i32 ()] Nothing False ()) ())
  , testP "fn(i32) -> impl Debug + Clone"
             (BareFn Normal Rust [] (FnDecl [Arg Nothing i32 ()] (Just (ImplTrait
                 [ TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) None ()
                 , TraitTyParamBound (PolyTraitRef [] (TraitRef clone) ()) None ()
                 ] ())) False ()) ())
  , testP "PResult<'a, P<i32>>"
             (PathTy Nothing (Path False [PathSegment "PResult" (Just (AngleBracketed [ Lifetime "a" () ]
                                                                     [ PathTy Nothing (Path False [PathSegment "P" (Just (AngleBracketed [] [ i32 ] [] ())) ()] ()) () ]
                                                                     [] ())) ()] ()) ())
  , testP "for<'l1: 'l2 + 'l3, 'l4: 'l5 +,> fn(_: Trait + 'l1 +) -> i32"
             (BareFn Normal Rust
                            [ LifetimeDef [] (Lifetime "l1" ()) [Lifetime "l2" (), Lifetime "l3" ()] ()
                            , LifetimeDef [] (Lifetime "l4" ()) [Lifetime "l5" ()] () ]
                            (FnDecl [Arg (Just (WildP ())) (TraitObject [TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [PathSegment "Trait" Nothing ()] ())) ()) None (), RegionTyParamBound (Lifetime "l1" ()) ()] ()) ()] 
                                    (Just i32) False ()) ())
  , testP "for <'a> Foo<&'a T>"
             (TraitObject
                [TraitTyParamBound
                   (PolyTraitRef [LifetimeDef [] (Lifetime "a" ()) [] ()]
                                 (TraitRef (Path False [PathSegment "Foo" (Just (AngleBracketed [] [Rptr (Just (Lifetime "a" ()))
                                                                                        Immutable
                                                                                        (PathTy Nothing t ())
                                                                                        ()]
                                                                                  [] ())) ()] ())) ()) None ()] ())
  , testP "for <'a,> Debug + for <'b> Send + for <'c> Sync"
             (TraitObject
               [ TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime "a" ()) [] ()]
                                                (TraitRef debug) ()) None ()
               , TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime "b" ()) [] ()]
                                                (TraitRef send) ()) None ()
               , TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime "c" ()) [] ()]
                                                (TraitRef (Path False [PathSegment "Sync" Nothing ()] ())) ()) None ()
               ] ())
  , testP "&(Debug + Send)"
           (Rptr Nothing Immutable (ParenTy
              (TraitObject [ TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) None ()
                           , TraitTyParamBound (PolyTraitRef [] (TraitRef send) ()) None ()
                           ] ()) ()) ())
 , testP "&(for<'a> Tr<'a> + Send)"
           (Rptr Nothing Immutable (ParenTy
              (TraitObject [ TraitTyParamBound (PolyTraitRef [LifetimeDef [] (Lifetime "a" ()) [] ()] (TraitRef (Path False [PathSegment "Tr" (Just (AngleBracketed [Lifetime "a" ()] [] [] ())) ()] ())) ()) None ()
                           , TraitTyParamBound (PolyTraitRef [] (TraitRef send) ()) None ()
                           ] ())
              ()) ())
  , testP "Fn() -> &(Object+Send)"
           (PathTy Nothing (Path False [PathSegment "Fn" (Just (Parenthesized [] (Just (Rptr Nothing Immutable (ParenTy (TraitObject [ TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [PathSegment "Object" Nothing ()] ())) ()) None ()
             , TraitTyParamBound (PolyTraitRef [] (TraitRef send) ()) None ()] ()) ()) ())) ())) ()] ()) ())
  , testP "foo![ x ]" (MacTy (Mac (Path False [PathSegment "foo" Nothing ()] ()) (Tree (Token (Span (Position 6 1 6) (Position 7 1 7)) (IdentTok "x")))  ()) ())
  , testP "impl Debug + Clone"
             (ImplTrait
                 [ TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) None ()
                 , TraitTyParamBound (PolyTraitRef [] (TraitRef clone) ()) None ()
                 ] ())
  , testP "dyn Debug + Clone"
             (TraitObject
                 [ TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) None ()
                 , TraitTyParamBound (PolyTraitRef [] (TraitRef clone) ()) None ()
                 ] ()) 
  ]


-- | Test parsing of patterns.
parserPatterns :: Test
parserPatterns = testGroup "parsing patterns"
  [ testP "_"                       (WildP ())
  , testP "x"                       x'
  , testP "ref mut x"               (IdentP (ByRef Mutable) "x" Nothing ())
  , testP "&x"                      (RefP x' Immutable ())
  , testP "&mut x"                  (RefP x' Mutable ())
  , testP "(x,)"                    (TupleP [ x' ] Nothing ())
  , testP "(..)"                    (TupleP [] (Just 0) ())
  , testP "(x, x)"                  (TupleP [ x', x' ] Nothing ())
  , testP "(x,.., x)"               (TupleP [ x', x' ] (Just 1) ())
  , testP "(..,x)"                  (TupleP [ x' ] (Just 0) ())
  , testP "(..,x,)"                 (TupleP [ x' ] (Just 0) ())
  , testP "(x,..)"                  (TupleP [ x' ] (Just 1) ())
  , testP "(x,x,)"                  (TupleP [ x', x' ] Nothing ())
  , testP "(x, ref mut y, box z)"   (TupleP [ x'
                                            , IdentP (ByRef Mutable) "y" Nothing ()
                                            , BoxP (IdentP (ByValue Immutable) "z" Nothing ()) ()
                                            ] 
                                            Nothing ())
  , testP "true"                    (LitP (Lit [] (Bool True Unsuffixed ()) ()) ())
  , testP "-123"                    (LitP (Unary [] Neg (Lit [] (Int Dec 123 Unsuffixed ()) ()) ()) ())
  , testP "Point { .. }"            (StructP point [] True ())
  , testP "Point { x, y: y1 }"      (StructP point
                                               [ FieldPat Nothing (IdentP (ByValue Immutable) "x" Nothing ()) ()
                                               , FieldPat (Just "y") (IdentP (ByValue Immutable) "y1" Nothing ()) () ]
                                               False ()) 
  , testP "Point { x, .. }"         (StructP point
                                               [ FieldPat Nothing (IdentP (ByValue Immutable) "x" Nothing ()) () ]
                                               True ())
  , testP "math"                    (IdentP (ByValue Immutable) "math" Nothing ())
  , testP "math::PI"                (PathP Nothing (Path False [ PathSegment "math" Nothing ()
                                                               , PathSegment "PI" Nothing () ] ()) ())
  , testP "math::<i32>"             (PathP Nothing (Path False [ PathSegment "math" (Just (AngleBracketed [] [i32] [] ())) () ] ()) ())
  , testP "math::<i32>::PI"         (PathP Nothing (Path False [ PathSegment "math" (Just (AngleBracketed [] [i32] [] ())) ()
                                                               , PathSegment "PI" Nothing () ] ()) ())
  , testP "1...2"                   (RangeP (Lit [] (Int Dec 1 Unsuffixed ()) ()) (Lit [] (Int Dec 2 Unsuffixed ()) ()) ())
  , testP "1..=2"                   (RangeP (Lit [] (Int Dec 1 Unsuffixed ()) ()) (Lit [] (Int Dec 2 Unsuffixed ()) ()) ())
  , testP "ref mut y@(x,x)"         (IdentP (ByRef Mutable) "y" (Just (TupleP [ x', x' ] Nothing ())) ())
  , testP "ref mut y@_"         (IdentP (ByRef Mutable) "y" (Just (WildP ())) ())
  , testP "(1,2,..,3)"              (TupleP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int Dec 3 Unsuffixed ()) ()) () ]
                                              (Just 2) ())
  , testP "Point(x)"                (TupleStructP point
                                              [ x' ] Nothing ())
  , testP "Point(x,)"               (TupleStructP point 
                                              [ x' ] Nothing ())
  , testP "<i32 as a(i32, i32)>::b::<'lt>::AssociatedItem"
            (PathP (Just (QSelf i32 1)) (Path False [ PathSegment "a" (Just (Parenthesized [i32, i32] Nothing ())) ()
                                                    , PathSegment "b" (Just (AngleBracketed [Lifetime "lt" ()] [] [] ())) ()
                                                    , PathSegment "AssociatedItem" Nothing ()
                                                    ] ()) ())
  , testP "[1,2]"                   (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int Dec 2 Unsuffixed ()) ()) () ] 
                                              Nothing [] ())
  , testP "[1,2,]"                  (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()
                                              , LitP (Lit [] (Int Dec 2 Unsuffixed ()) ()) () ] 
                                              Nothing [] ())
  , testP "[1,..,3]"                (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) () ]
                                              (Just (WildP ()))
                                              [ LitP (Lit [] (Int Dec 3 Unsuffixed ()) ()) () ] ())
  , testP "[1,..,3,]"               (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) () ]
                                              (Just (WildP ()))
                                              [ LitP (Lit [] (Int Dec 3 Unsuffixed ()) ()) () ] ())
  , testP "[1,x..,3]"               (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) () ]
                                              (Just x')
                                              [ LitP (Lit [] (Int Dec 3 Unsuffixed ()) ()) () ] ())
  , testP "[1,..]"                  (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) () ] (Just (WildP ())) [] ())
  , testP "[1,x..]"                 (SliceP [ LitP (Lit [] (Int Dec 1 Unsuffixed ()) ()) () ] (Just x') [] ())
  , testP "[x]"                     (SliceP [ x' ] Nothing [] ())
  , testP "[x,]"                    (SliceP [ x' ] Nothing [] ())
  , testP "[x..]"                   (SliceP [] (Just x') [] ())
  , testP "[..]"                    (SliceP [] (Just (WildP ())) [] ())
  , testP "foo!(x)"                 (MacP (Mac (Path False [PathSegment "foo" Nothing ()] ()) (Tree (Token (Span (Position 5 1 5) (Position 6 1 6)) (IdentTok "x")))  ()) ())
  ]

  
-- | Test parsing of expressions.
parserExpressions :: Test
parserExpressions = testGroup "parsing expressions"
  [ testP "123" (Lit [] (Int Dec 123 Unsuffixed ()) ())
  , testP "()" (TupExpr [] [] ()) 
  , testP "(1,)" (TupExpr [] [Lit [] (Int Dec 1 Unsuffixed ()) ()] ())
  , testP "(1,2)" (TupExpr [] [Lit [] (Int Dec 1 Unsuffixed ()) (), Lit [] (Int Dec 2 Unsuffixed ()) ()] ())
  , testP "|| 1" (Closure [] Movable Ref (FnDecl [] Nothing False ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "|_: ()| 1" (Closure [] Movable Ref (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] Nothing False ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "|_| 1" (Closure [] Movable Ref (FnDecl [Arg (Just (WildP ())) (Infer ()) ()] Nothing False ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "static |_| 1" (Closure [] Immovable Ref (FnDecl [Arg (Just (WildP ())) (Infer ()) ()] Nothing False ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "|_: ()| -> () { (); }" (Closure [] Movable Ref (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] (Just (TupTy [] ())) False ()) (BlockExpr [] (Block [Semi (TupExpr [] [] ()) ()] Normal ()) ()) ())
  , testP "move || 1" (Closure [] Movable Value (FnDecl [] Nothing False ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "static move || 1" (Closure [] Immovable Value (FnDecl [] Nothing False ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "move |_: ()| 1" (Closure [] Movable Value (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] Nothing False ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "move |_| 1" (Closure [] Movable Value (FnDecl [Arg (Just (WildP ())) (Infer ()) ()] Nothing False ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "move |_: ()| -> () { (); }" (Closure [] Movable Value (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] (Just (TupTy [] ())) False ()) (BlockExpr [] (Block [Semi (TupExpr [] [] ()) ()] Normal ()) ()) ())
  , testP "|_: ()| -> () { () }" (Closure [] Movable Ref (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] (Just (TupTy [] ())) False ()) (BlockExpr [] (Block [NoSemi (TupExpr [] [] ()) ()] Normal ()) ()) ())
  , testP "move |_: ()| -> () { () }" (Closure [] Movable Value (FnDecl [Arg (Just (WildP ())) (TupTy [] ()) ()] (Just (TupTy [] ())) False ()) (BlockExpr [] (Block [NoSemi (TupExpr [] [] ()) ()] Normal ()) ()) ())
  , testP "[(); 512]" (Repeat [] (TupExpr [] [] ()) (Lit [] (Int Dec 512 Unsuffixed ()) ()) ())
  , testP "[]" (Vec [] [] ())
  , testP "[1]" (Vec [] [Lit [] (Int Dec 1 Unsuffixed ()) ()] ())
  , testP "[1,]" (Vec [] [Lit [] (Int Dec 1 Unsuffixed ()) ()] ())
  , testP "[1,2]" (Vec [] [Lit [] (Int Dec 1 Unsuffixed ()) (), Lit [] (Int Dec 2 Unsuffixed ()) ()] ())
  , testP "{ 1; 2 }" (BlockExpr [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) (), NoSemi (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] Normal ()) ())
  , testP "unsafe { 1; 2 }" (BlockExpr [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) (), NoSemi (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] Unsafe ()) ())
  , testP "{ 1; 2; }" (BlockExpr [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) (), Semi (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] Normal ()) ())
  , testP "{ }" (BlockExpr [] (Block [] Normal ()) ())
  , testP "unsafe { 1; 2; }" (BlockExpr [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) (), Semi (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] Unsafe ()) ())
  , testP "{ ;;; }" (BlockExpr [] (Block [] Normal ()) ())
  , testP "{ 1;; 2;; }" (BlockExpr [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) (), Semi (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] Normal ()) ())
  , testP "return" (Ret [] Nothing ())
  , testP "return 1" (Ret [] (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) ())
  , testP "continue" (Continue [] Nothing ())
  , testP "break" (Break [] Nothing Nothing ())
  , testP "break 1" (Break [] Nothing (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) ())
  , testP "continue 'lbl" (Continue [] (Just (Label "lbl" ())) ())
  , testP "break 'lbl" (Break [] (Just (Label "lbl" ())) Nothing ())
  , testP "break 'lbl 1" (Break [] (Just (Label "lbl" ())) (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) ())
  , testP "math"            (PathExpr [] Nothing (Path False [ PathSegment "math" Nothing () ] ()) ())
  , testP "math::PI"        (PathExpr [] Nothing (Path False [ PathSegment "math" Nothing ()
                                                             , PathSegment "PI" Nothing () ] ()) ())
  , testP "math::<i32>"     (PathExpr [] Nothing (Path False [ PathSegment "math" (Just (AngleBracketed [] [i32] [] ())) () ] ()) ())
  , testP "math::<i32>::PI" (PathExpr [] Nothing (Path False [ PathSegment "math" (Just (AngleBracketed [] [i32] [] ())) ()
                                                             , PathSegment "PI" Nothing () ] ()) ())
  , testP "<i32 as a(i32, i32)>::b::<'lt>::AssociatedItem"
            (PathExpr [] (Just (QSelf i32 1)) (Path False [ PathSegment "a" (Just (Parenthesized [i32, i32] Nothing ())) ()
                                                          , PathSegment "b" (Just (AngleBracketed [Lifetime "lt" ()] [] [] ())) ()
                                                          , PathSegment "AssociatedItem" Nothing ()
                                                          ] ()) ())
  , testP "Point { x: 1, y: 2 }" (Struct [] point [Field "x" (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) (), Field "y" (Just (Lit [] (Int Dec 2 Unsuffixed ()) ())) ()] Nothing ())
  , testP "Point { x, y }" (Struct [] point [Field "x" Nothing (), Field "y" Nothing ()] Nothing ())
  , testP "Point { x: 1, y: 2, }" (Struct [] point [Field "x" (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) (), Field "y" (Just (Lit [] (Int Dec 2 Unsuffixed ()) ())) ()] Nothing ())
  , testP "Point { x: 1, ..p }" (Struct [] point [Field "x" (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) ()] (Just (PathExpr [] Nothing (Path False [ PathSegment "p" Nothing () ] ()) ())) ())
  , testP "Point { ..p }" (Struct [] point [] (Just (PathExpr [] Nothing (Path False [ PathSegment "p" Nothing () ] ()) ())) ())
  , testP "Point { }" (Struct [] point [] Nothing ())
  , testP "if true { 1; }"
            (If [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) 
                Nothing ())
  , testP "if true { 1; } else { 2; }"
            (If [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) 
                (Just (BlockExpr [] (Block [Semi (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] Normal ()) ())) ())
  , testP "if true { 1; } else if false { 2; }"
            (If [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) 
                (Just (If [] (Lit [] (Bool False Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] Normal ())
                    Nothing ())) ())
  , testP "if true { 1; } else if false { 2; } else { 3; }"
            (If [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) 
                (Just (If [] (Lit [] (Bool False Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] Normal ())
                    (Just (BlockExpr [] (Block [Semi (Lit [] (Int Dec 3 Unsuffixed ()) ()) ()] Normal ()) ())) ())) ())
  , testP "if let x = true { 1; }"
            (IfLet [] [x'] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) 
                Nothing ())
  , testP "if let x = true { 1; } else { 2; }"
            (IfLet [] [x'] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) 
                (Just (BlockExpr [] (Block [Semi (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] Normal ()) ())) ())
  , testP "if true { 1; } else if let x = false { 2; }"
            (If [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) 
                (Just (IfLet [] [x'] (Lit [] (Bool False Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] Normal ())
                    Nothing ())) ())
  , testP "do catch { 1; }" (Catch [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) ())
  , testP "loop { 1; }" (Loop [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) Nothing ())
  , testP "'lbl: loop { 1; }" (Loop [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) (Just (Label "lbl" ())) ())
  , testP "for x in [1,2,3] { 1; }" (ForLoop [] x' (Vec [] [Lit [] (Int Dec 1 Unsuffixed ()) (), Lit [] (Int Dec 2 Unsuffixed ()) (), Lit [] (Int Dec 3 Unsuffixed ()) ()] ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) Nothing ()) 
  , testP "'lbl: for x in [1,2,3] { 1; }" (ForLoop [] x' (Vec [] [Lit [] (Int Dec 1 Unsuffixed ()) (), Lit [] (Int Dec 2 Unsuffixed ()) (), Lit [] (Int Dec 3 Unsuffixed ()) ()] ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) (Just (Label "lbl" ())) ()) 
  , testP "while true { 1; }" (While [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) Nothing ())
  , testP "'lbl: while true { 1; }" (While [] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) (Just (Label "lbl" ())) ())
  , testP "while let x = true { 1; }" (WhileLet [] [x'] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) Nothing ())
  , testP "'lbl: while let x = true { 1; }" (WhileLet [] [x'] (Lit [] (Bool True Unsuffixed ()) ()) (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) (Just (Label "lbl" ())) ())
  , testP "match true { }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [] ())
  , testP "match true { _ => 2 }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] ())
  , testP "match true { _ if true => 2 }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] (Just (Lit [] (Bool True Unsuffixed ()) ())) (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] ())
  , testP "match true { _ => 2, }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int Dec 2 Unsuffixed ()) ()) ()] ())
  , testP "match true { _ => 2, x | x => 1 }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int Dec 2 Unsuffixed ()) ()) (), Arm [] [x',x'] Nothing (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] ())
  , testP "match true { _ => 2, x | x => { 1; } }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int Dec 2 Unsuffixed ()) ()) (), Arm [] [x',x'] Nothing (BlockExpr [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) ()) ()] ())
  , testP "match true { _ => 2, x | x => { 1; }, }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int Dec 2 Unsuffixed ()) ()) (), Arm [] [x',x'] Nothing (BlockExpr [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) ()) ()] ())
  , testP "match true { _ => 2, x | x => { 1; }, _ => 1 }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int Dec 2 Unsuffixed ()) ()) (), Arm [] [x',x'] Nothing (BlockExpr [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) ()) (), Arm [] [WildP ()] Nothing (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] ())
  , testP "match true { _ => 2, x | x => { 1; } _ => 1 }" (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Int Dec 2 Unsuffixed ()) ()) (), Arm [] [x',x'] Nothing (BlockExpr [] (Block [Semi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) ()) (), Arm [] [WildP ()] Nothing (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] ())
  , testP "println!()" (MacExpr [] (Mac (Path False [PathSegment "println" Nothing ()] ()) (Stream []) ()) ()) 
  , testP "1..2" (Range [] (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) (Just (Lit [] (Int Dec 2 Unsuffixed ()) ())) HalfOpen ())
  , testP "1...2" (Range [] (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) (Just (Lit [] (Int Dec 2 Unsuffixed ()) ())) Closed ())
  , testP "1..=2" (Range [] (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) (Just (Lit [] (Int Dec 2 Unsuffixed ()) ())) Closed ())
  , testP "1.." (Range [] (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) Nothing HalfOpen ())
  , testP "..2" (Range [] Nothing (Just (Lit [] (Int Dec 2 Unsuffixed ()) ())) HalfOpen ())
  , testP "...2" (Range [] Nothing (Just (Lit [] (Int Dec 2 Unsuffixed ()) ())) Closed ())
  , testP "..=2" (Range [] Nothing (Just (Lit [] (Int Dec 2 Unsuffixed ()) ())) Closed ())
  , testP ".." (Range [] Nothing Nothing HalfOpen ())
  , testP "x?" (Try [] (PathExpr [] Nothing x ()) ())
  , testP "x.0" (TupField [] (PathExpr [] Nothing x ()) 0 ())
  , testP "x.foo" (FieldAccess [] (PathExpr [] Nothing x ()) "foo" ())
  , testP "x.foo()" (MethodCall [] (PathExpr [] Nothing x ()) "foo" Nothing [] ())
  , testP "x.foo(1)" (MethodCall [] (PathExpr [] Nothing x ()) "foo" Nothing [Lit [] (Int Dec 1 Unsuffixed ()) ()] ())
  , testP "x.foo(1,)" (MethodCall [] (PathExpr [] Nothing x ()) "foo" Nothing [Lit [] (Int Dec 1 Unsuffixed ()) ()] ())
  , testP "x.foo::<>(1,)" (MethodCall [] (PathExpr [] Nothing x ()) "foo" (Just []) [Lit [] (Int Dec 1 Unsuffixed ()) ()] ())
  , testP "x.foo::<i32>()" (MethodCall [] (PathExpr [] Nothing x ()) "foo" (Just [i32]) [] ())
  , testP "x.foo::<i32,>(1)" (MethodCall [] (PathExpr [] Nothing x ()) "foo" (Just [i32]) [Lit [] (Int Dec 1 Unsuffixed ()) ()] ())
  , testP "x.foo::<i32>(1,)" (MethodCall [] (PathExpr [] Nothing x ()) "foo" (Just [i32]) [Lit [] (Int Dec 1 Unsuffixed ()) ()] ())
  , testP "self" (PathExpr [] Nothing (Path False [PathSegment "self" Nothing ()] ()) ())
  , testP "x[1]" (Index [] (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x()" (Call [] (PathExpr [] Nothing x ()) [] ())
  , testP "x(1)" (Call [] (PathExpr [] Nothing x ()) [Lit [] (Int Dec 1 Unsuffixed ()) ()] ())
  , testP "x(1,)" (Call [] (PathExpr [] Nothing x ()) [Lit [] (Int Dec 1 Unsuffixed ()) ()] ())
  , testP "x(1,2)" (Call [] (PathExpr [] Nothing x ()) [Lit [] (Int Dec 1 Unsuffixed ()) (), Lit [] (Int Dec 2 Unsuffixed ()) ()] ())
  , testP "x(1,2,)" (Call [] (PathExpr [] Nothing x ()) [Lit [] (Int Dec 1 Unsuffixed ()) (), Lit [] (Int Dec 2 Unsuffixed ()) ()] ())
  , testP "x[1][1]" (Index [] (Index [] (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "&x?" (AddrOf [] Immutable (Try [] (PathExpr [] Nothing x ()) ()) ())
  , testP "&x" (AddrOf [] Immutable (PathExpr [] Nothing x ()) ())
  , testP "&mut x" (AddrOf [] Mutable (PathExpr [] Nothing x ()) ())
  , testP "*x" (Unary [] Deref (PathExpr [] Nothing x ()) ())
  , testP "!x" (Unary [] Not (PathExpr [] Nothing x ()) ())
  , testP "-x" (Unary [] Neg (PathExpr [] Nothing x ()) ())
  , testP "box x" (Box [] (PathExpr [] Nothing x ()) ())
  , testP "1 : i32" (TypeAscription [] (Lit [] (Int Dec 1 Unsuffixed ()) ()) i32 ())
  , testP "1 : i32 : i32" (TypeAscription [] (TypeAscription [] (Lit [] (Int Dec 1 Unsuffixed ()) ()) i32 ()) i32 ())
  , testP "1 as i32" (Cast [] (Lit [] (Int Dec 1 Unsuffixed ()) ()) i32 ())
  , testP "1 as i32 : i32" (TypeAscription [] (Cast [] (Lit [] (Int Dec 1 Unsuffixed ()) ()) i32 ()) i32 ())
  , testP "1 : i32 as i32" (Cast [] (TypeAscription [] (Lit [] (Int Dec 1 Unsuffixed ()) ()) i32 ()) i32 ())
  , testP "1 as i32 as i32" (Cast [] (Cast [] (Lit [] (Int Dec 1 Unsuffixed ()) ()) i32 ()) i32 ())
  , testP "!1 as i32" (Cast [] (Unary [] Not (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()) i32 ())
  , testP "!(1 as i32)" (Unary [] Not (ParenExpr [] (Cast [] (Lit [] (Int Dec 1 Unsuffixed ()) ()) i32 ()) ()) ())
  , testP "x = 1" (Assign [] (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x == 1" (Binary [] EqOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x < 1" (Binary [] LtOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x > 1" (Binary [] GtOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x <= 1" (Binary [] LeOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x >= 1" (Binary [] GeOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x >>= 1" (AssignOp [] ShrOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x <<= 1" (AssignOp [] ShlOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x -= 1" (AssignOp [] SubOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x += 1" (AssignOp [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x *= 1" (AssignOp [] MulOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x /= 1" (AssignOp [] DivOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x ^= 1" (AssignOp [] BitXorOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x |= 1" (AssignOp [] BitOrOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x &= 1" (AssignOp [] BitAndOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x %= 1" (AssignOp [] RemOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x && 1" (Binary [] AndOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x && x()" (Binary [] AndOp (PathExpr [] Nothing x ())
                                      (Call [] (PathExpr [] Nothing x ()) [] ()) ())
  , testP "x || 1" (Binary [] OrOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x || x()" (Binary [] OrOp (PathExpr [] Nothing x ())
                                     (Call [] (PathExpr [] Nothing x ()) [] ()) ())
  , testP "x + 1" (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x - 1" (Binary [] SubOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x * 1" (Binary [] MulOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x / 1" (Binary [] DivOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x % 1" (Binary [] RemOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x & 1" (Binary [] BitAndOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x | 1" (Binary [] BitOrOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x ^ 1" (Binary [] BitXorOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x << 1" (Binary [] ShlOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "x >> 1" (Binary [] ShrOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())
  , testP "&&&x&&&x" (Binary [] AndOp (AddrOf [] Immutable (AddrOf [] Immutable (AddrOf [] Immutable (PathExpr [] Nothing x ()) ()) ()) ()) (AddrOf [] Immutable (PathExpr [] Nothing x ()) ()) ())
  , testP "&[]" (AddrOf [] Immutable (Vec [] [] ()) ()) 
  , testP "for _ in 1..2 { }" (ForLoop [] (WildP ()) (Range [] (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) (Just (Lit [] (Int Dec 2 Unsuffixed ()) ())) HalfOpen ()) (Block [] Normal ()) Nothing ())
  , testP "for _ in 1..x { }" (ForLoop [] (WildP ()) (Range [] (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) (Just (PathExpr [] Nothing x ())) HalfOpen ()) (Block [] Normal ()) Nothing ())
  , testP "for _ in 1.. { }" (ForLoop [] (WildP ()) (Range [] (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) Nothing HalfOpen ()) (Block [] Normal ()) Nothing ())
  , testP "1 * 1 + 1 * 1" (Binary [] AddOp (Binary [] MulOp (Lit [] (Int Dec 1 Unsuffixed ()) ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()) (Binary [] MulOp (Lit [] (Int Dec 1 Unsuffixed ()) ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()) ())
  , testP "1 * - 1 + 1 * 1" (Binary [] AddOp (Binary [] MulOp (Lit [] (Int Dec 1 Unsuffixed ()) ()) (Unary [] Neg (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()) ()) (Binary [] MulOp (Lit [] (Int Dec 1 Unsuffixed ()) ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()) ())
  , testP "match true { _ => match true { _ => true }, _ => match true { _ => true } }" (Match [] (Lit [] (Bool True Unsuffixed ()) ())
      [ Arm [] [WildP ()] Nothing (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Bool True Unsuffixed ()) ()) ()] ()) ()
      , Arm [] [WildP ()] Nothing (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [Arm [] [WildP ()] Nothing (Lit [] (Bool True Unsuffixed ()) ()) ()] ()) () ] ())
  ]


-- | Test parsing of statements.
parserStatements :: Test
parserStatements = testGroup "parsing statements"
  [ testP "let x: i32 = 1;" (Local x' (Just i32) (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) [] ()) 
  , testP "let x: i32;"     (Local x' (Just i32) Nothing                                  [] ())
  , testP "let x = 1;"      (Local x' Nothing    (Just (Lit [] (Int Dec 1 Unsuffixed ()) ())) [] ())
  , testP "let x;"          (Local x' Nothing    Nothing                                  [] ())
  , testP "x + 1;" (Semi (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()) ())
  , testP "x + { 1 };" (Semi (Binary [] AddOp (PathExpr [] Nothing x ()) (BlockExpr [] (Block [NoSemi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) ()) ()) ())
  , testP "match true { };" (Semi (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [] ()) ())
  , testP "match true { }" (NoSemi (Match [] (Lit [] (Bool True Unsuffixed ()) ()) [] ()) ())
  , testP "static foo: i32 = 1;" (ItemStmt (Static [] InheritedV "foo" i32 Immutable (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()) ())
  , testP "unsafe { 1 };" (Semi (BlockExpr [] (Block [NoSemi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Unsafe ()) ()) ())
  , testP "unsafe { 1 }" (NoSemi (BlockExpr [] (Block [NoSemi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Unsafe ()) ()) ())
  , testP "{ 1 };" (Semi (BlockExpr [] (Block [NoSemi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) ()) ())
  , testP "{ 1 }" (NoSemi (BlockExpr [] (Block [NoSemi (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] Normal ()) ()) ())
  ]


-- | Test parsing of items.
parserItems :: Test
parserItems = testGroup "parsing items"
  [ testP "static mut foo: i32 = 1;" (Static [] InheritedV (mkIdent "foo") i32 Mutable _1 ())
  , testP "static foo: i32 = 1;" (Static [] InheritedV (mkIdent "foo") i32 Immutable _1 ())
  , testP "const foo: i32 = 1;" (ConstItem [] InheritedV (mkIdent "foo") i32 _1 ())
  , testP "type Foo = i32;" (TyAlias [] InheritedV "Foo" i32 (Generics [] [] (WhereClause [] ()) ()) ()) 
  , testP "type Foo where i32 = i32 = i32;" (TyAlias [] InheritedV "Foo" i32 (Generics [] [] (WhereClause [EqPredicate i32 i32 ()] ()) ()) ()) 
  , testP "type Foo<> = i32;" (TyAlias [] InheritedV "Foo" i32 (Generics [] [] (WhereClause [] ()) ()) ()) 
  , testP "extern crate foo;" (ExternCrate [] InheritedV (mkIdent "foo") Nothing ())
  , testP "extern crate foo as bar;" (ExternCrate [] InheritedV (mkIdent "bar") (Just "foo") ())
  , testP "mod foo;" (Mod [] InheritedV "foo" Nothing ())
  , testP "struct Point;" (StructItem [] InheritedV "Point" (UnitD ()) (Generics [] [] (WhereClause [] ()) ()) ())
  , testP "struct Point { }" (StructItem [] InheritedV "Point" (StructD [] ()) (Generics [] [] (WhereClause [] ()) ()) ())
  , testP "struct Point { x: i32, y: i32 }" (StructItem [] InheritedV "Point" (StructD [StructField (Just "x") InheritedV i32 [] (), StructField (Just "y") InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ()) ())
  , testP "struct Point { x: i32, y: i32, }" (StructItem [] InheritedV "Point" (StructD [StructField (Just "x") InheritedV i32 [] (), StructField (Just "y") InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ()) ())
  , testP "union Either;" (Union [] InheritedV "Either" (UnitD ()) (Generics [] [] (WhereClause [] ()) ()) ())
  , testP "union Either{ }" (Union [] InheritedV "Either" (StructD [] ()) (Generics [] [] (WhereClause [] ()) ()) ())
  , testP "union Either { x: i32, y: i32 }" (Union [] InheritedV "Either" (StructD [StructField (Just "x") InheritedV i32 [] (), StructField (Just "y") InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ()) ())
  , testP "union Either { x: i32, y: i32, }" (Union [] InheritedV "Either" (StructD [StructField (Just "x") InheritedV i32 [] (), StructField (Just "y") InheritedV i32 [] ()] ()) (Generics [] [] (WhereClause [] ()) ()) ())
  , testP "use std::math;" (Use [] InheritedV (UseTreeSimple (Path False [ PathSegment "std" Nothing (), PathSegment "math" Nothing () ] ()) Nothing ()) ())
  , testP "use std::math as m;" (Use [] InheritedV (UseTreeSimple (Path False [ PathSegment "std" Nothing (), PathSegment "math" Nothing () ] ()) (Just "m") ()) ())
  , testP "use std::math::*;" (Use [] InheritedV (UseTreeGlob (Path False [ PathSegment "std" Nothing (), PathSegment "math" Nothing () ] ()) ()) ())
  , testP "use *;" (Use [] InheritedV (UseTreeGlob (Path False [ ] ()) ()) ())
  , testP "use ::*;" (Use [] InheritedV (UseTreeGlob (Path True [ ] ()) ()) ())
  , testP "use std::math::{};" (Use [] InheritedV (UseTreeNested (Path False [ PathSegment "std" Nothing (), PathSegment "math" Nothing () ] ()) [] ()) ())
  , testP "use std::math::{sqrt, pi as p};" (Use [] InheritedV (UseTreeNested (Path False [ PathSegment "std" Nothing (), PathSegment "math" Nothing () ] ()) [ UseTreeSimple (Path False [ PathSegment "sqrt" Nothing () ] ()) Nothing (), UseTreeSimple (Path False [ PathSegment "pi" Nothing () ] ()) (Just "p") () ] ()) ())
  , testP "use std::math::{sqrt};" (Use [] InheritedV (UseTreeNested (Path False [ PathSegment "std" Nothing (), PathSegment "math" Nothing () ] ()) [ UseTreeSimple (Path False [ PathSegment "sqrt" Nothing () ] ()) Nothing () ] ()) ())
  , testP "use std::math::{sqrt, pi,};" (Use [] InheritedV (UseTreeNested (Path False [ PathSegment "std" Nothing (), PathSegment "math" Nothing () ] ()) [ UseTreeSimple (Path False [ PathSegment "sqrt" Nothing () ] ()) Nothing (), UseTreeSimple (Path False [ PathSegment "pi" Nothing () ] ()) Nothing () ] ()) ())
  , testP "const unsafe fn foo(x: i32) -> i32 { return x + 1 }" (Fn [] InheritedV "foo" (FnDecl [Arg (Just x') i32 ()] (Just i32) False ()) Unsafe Const Rust (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ())
  , testP "fn bar<T, K>(x: T, y: K) where T: Clone, K: Clone + Debug { return x + 1 }" (Fn [] InheritedV "bar" (FnDecl [ Arg (Just (IdentP (ByValue Immutable) "x" Nothing ())) (PathTy Nothing t ()) ()
                     , Arg (Just (IdentP (ByValue Immutable) "y" Nothing ())) (PathTy Nothing (Path False [PathSegment "K" Nothing ()] ()) ()) ()
                     ]
            Nothing
            False
            ())
    Normal NotConst Rust
    (Generics [] [TyParam [] "T" [] Nothing (), TyParam [] "K" [] Nothing ()]
              (WhereClause [ BoundPredicate [] (PathTy Nothing t ()) [TraitTyParamBound (PolyTraitRef [] (TraitRef clone) ()) None ()] ()
                           , BoundPredicate [] (PathTy Nothing (Path False [PathSegment "K" Nothing ()] ()) ()) [TraitTyParamBound (PolyTraitRef [] (TraitRef clone) ()) None (), TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) None ()] ()
                           ] ()) ())
    (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ())
    ())

  , testP "fn inverse<T,>(x: i32) -> T where i32: ConvertTo<T>, { return x + 1 }" (Fn [] InheritedV "inverse" (FnDecl [ Arg (Just (IdentP (ByValue Immutable) "x" Nothing ())) (PathTy Nothing (Path False [PathSegment "i32" Nothing ()] ()) ()) ()
                     ]
            (Just (PathTy Nothing t ())) 
            False
            ())
    Normal NotConst Rust
    (Generics [] [TyParam [] "T" [] Nothing ()]
              (WhereClause [ BoundPredicate [] (PathTy Nothing (Path False [ PathSegment "i32" Nothing () ] ()) ()) [TraitTyParamBound (PolyTraitRef [] (TraitRef (Path False [ PathSegment "ConvertTo" (Just (AngleBracketed [] [PathTy Nothing t ()] [] ())) () ] ())) ()) None ()] ()
                           ] ()) ())
    (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ())
    ())

  , testP "const fn foo<>(x: i32) -> i32 { return x + 1 }" (Fn [] InheritedV "foo" (FnDecl [Arg (Just x') i32 ()] (Just i32) False ()) Normal Const Rust (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ())
  , testP "unsafe extern fn foo(x: i32) -> i32 { return x + 1 }" (Fn [] InheritedV "foo" (FnDecl [Arg (Just x') i32 ()] (Just i32) False ()) Unsafe NotConst C (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ())
  , testP "unsafe extern \"win64\" fn foo(x: i32) -> i32 { return x + 1 }" (Fn [] InheritedV "foo" (FnDecl [Arg (Just x') i32 ()] (Just i32) False ()) Unsafe NotConst Win64 (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ())
  , testP "extern \"win64\" fn foo(x: i32) -> i32 { return x + 1 }" (Fn [] InheritedV "foo" (FnDecl [Arg (Just x') i32 ()] (Just i32) False ()) Normal NotConst Win64 (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ())
  , testP "fn foo(x: i32) -> i32 { return x + 1 }" (Fn [] InheritedV "foo" (FnDecl [Arg (Just x') i32 ()] (Just i32) False ()) Normal NotConst Rust (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ())
  , testP "fn foo(x: i32) -> i32 where { return x + 1 }" (Fn [] InheritedV "foo" (FnDecl [Arg (Just x') i32 ()] (Just i32) False ()) Normal NotConst Rust (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ())
  , testP "mod foo;" (Mod [] InheritedV "foo" Nothing ()) 
  , testP "mod foo { }" (Mod [] InheritedV "foo" (Just []) ()) 
  , testP "mod foo { pub fn foo(x: i32) -> i32 { return x + 1 } }" (Mod [] InheritedV "foo" (Just [Fn [] PublicV "foo" (FnDecl [Arg (Just x') i32 ()] (Just i32) False ()) Normal NotConst Rust (Generics [] [] (WhereClause [] ()) ()) (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ()]) ())
  , testP "pub extern { }" (ForeignMod [] PublicV C [] ())
  , testP "extern { }" (ForeignMod [] InheritedV C [] ())
  , testP "extern \"win64\" { pub static x: i32; static mut y: i32; }" (ForeignMod [] InheritedV Win64 [ForeignStatic [] PublicV "x" i32 Immutable (), ForeignStatic [] InheritedV "y" i32 Mutable ()] ())
  , testP "extern { pub fn foo(x: i32, ...); }" (ForeignMod [] InheritedV C [ForeignFn [] PublicV "foo" (FnDecl [Arg (Just x') i32 ()] Nothing True ()) (Generics [] [] (WhereClause [] ()) ()) ()] ())
 
 , testP "enum Option<T> { None, Some(T) }" (Enum [] InheritedV "Option" [ Variant "None" [] (UnitD ()) Nothing ()
                                                                     , Variant "Some" [] (TupleD [ StructField Nothing InheritedV (PathTy Nothing t ()) [] ()] ()) Nothing () ]
                                                                     (Generics [] [TyParam [] "T" [] Nothing ()] (WhereClause [] ()) ()) ())  
  , testP "impl [i32] { }" (Impl [] InheritedV Final Normal Positive (Generics [] [] (WhereClause [] ()) ()) Nothing (Slice i32 ()) [] ())
  , testP "unsafe impl i32 { }" (Impl [] InheritedV Final Unsafe Positive (Generics [] [] (WhereClause [] ()) ()) Nothing i32 [] ())
  , testP "impl (<i32 as a>::b) { }" (Impl [] InheritedV Final Normal Positive (Generics [] [] (WhereClause [] ()) ()) Nothing (ParenTy (PathTy (Just (QSelf i32 1)) (Path False [PathSegment "a" Nothing (), PathSegment "b" Nothing () ] ()) ()) ()) [] ())
  , testP "impl (<i32 as a>::b) { }" (Impl [] InheritedV Final Normal Positive (Generics [] [] (WhereClause [] ()) ()) Nothing (ParenTy (PathTy (Just (QSelf i32 1)) (Path False [PathSegment "a" Nothing (), PathSegment "b" Nothing () ] ()) ()) ()) [] ())
  , testP "impl !Debug for i32 { }" (Impl [] InheritedV Final Normal Negative (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef debug)) i32 [] ())
  , testP "default impl Debug for i32 { }" (Impl [] InheritedV Default Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef debug)) i32 [] ())
  , testP "impl Debug for i32 { }" (Impl [] InheritedV Final Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef debug)) i32 [] ())
  , testP "pub impl Debug for i32 { type T = i32; }" (Impl [] PublicV Final Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef debug)) i32 [TypeI [] InheritedV Final "T" i32 ()] ())
  , testP "impl Debug for i32 { pub default type T = i32; }" (Impl [] InheritedV Final Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef debug)) i32 [TypeI [] PublicV Default "T" i32 ()] ())
  , testP "impl Debug for i32 { const x: i32 = 1; }" (Impl [] InheritedV Final Normal Positive (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef debug)) i32 [ConstI [] InheritedV Final "x" i32 (Lit [] (Int Dec 1 Unsuffixed ()) ()) ()] ())

  , testP "pub impl Debug for i32 { const unsafe fn foo(x: i32) -> i32 { return x + 1 } }" 
  (Impl [] PublicV Final Normal Positive
                    (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef debug)) i32
                    [MethodI [] InheritedV Final "foo"
                             (Generics [] [] (WhereClause [] ()) ())
                             (MethodSig Unsafe Const Rust (FnDecl [Arg (Just x') i32 ()] (Just i32) False ()))
                             (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ()] ())  
  , testP "impl Debug for i32 { pub default extern \"C\" fn foo(x: i32) -> i32 { return x + 1 } }" 
  (Impl [] InheritedV Final Normal Positive
                    (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef debug)) i32
                    [MethodI [] PublicV Default "foo"
                       (Generics [] [] (WhereClause [] ()) ())
                       (MethodSig Normal NotConst C (FnDecl [Arg (Just x') i32 ()] (Just i32) False ()) )
                       (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ()] ())  
  , testP "impl Debug for i32 { fn foo(&self) -> i32 { return x + 1 } }" 
  (Impl [] InheritedV Final Normal Positive
                    (Generics [] [] (WhereClause [] ()) ()) (Just (TraitRef debug)) i32
                    [MethodI [] InheritedV Final "foo"
                             (Generics [] [] (WhereClause [] ()) ())
                             (MethodSig Normal NotConst Rust (FnDecl [SelfRegion Nothing Immutable ()] (Just i32) False ()) )
                             (Block [NoSemi (Ret [] (Just (Binary [] AddOp (PathExpr [] Nothing x ()) (Lit [] (Int Dec 1 Unsuffixed ()) ()) ())) ()) ()] Normal ()) ()] ()) 
  , testP "trait Trace { }" (Trait [] InheritedV "Trace" False Normal (Generics [] [] (WhereClause [] ()) ()) [] [] ()) 
  , testP "unsafe trait Trace { }" (Trait [] InheritedV "Trace" False Unsafe (Generics [] [] (WhereClause [] ()) ()) [] [] ()) 
  , testP "unsafe auto trait Trace { }" (Trait [] InheritedV "Trace" True Unsafe (Generics [] [] (WhereClause [] ()) ()) [] [] ()) 
  , testP "trait Trace: Debug { }" (Trait [] InheritedV "Trace" False Normal (Generics [] [] (WhereClause [] ()) ()) [TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) None ()] [] ()) 
  , testP "unsafe trait Trace: Debug { }" (Trait [] InheritedV "Trace" False Unsafe (Generics [] [] (WhereClause [] ()) ()) [TraitTyParamBound (PolyTraitRef [] (TraitRef debug) ()) None ()] [] ())
  ] 

-- Just a common expression to make the tests above more straightforward
_1 :: Expr ()
_1 = Lit [] (Int Dec 1 Unsuffixed ()) ()

-- Just a common pattern to make the tests above more straightforward
x' :: Pat ()
x' = IdentP (ByValue Immutable) "x" Nothing ()

-- Just a common type to make the tests above more straightforward
i32 :: Ty ()
i32 = PathTy Nothing (Path False [PathSegment "i32" Nothing ()] ()) ()
