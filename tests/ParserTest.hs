{-# LANGUAGE OverloadedStrings, UnicodeSyntax, FlexibleContexts  #-}
module ParserTest (parserSuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Parser.ParseMonad
import Language.Rust.Parser.Parser2 (expressionP, typeP, patternP)
import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
import Language.Rust.Data.InputStream

import Control.Monad
import Control.Monad.Trans.Except

parserSuite :: Test
parserSuite = testGroup "parser suite" [ patternSuite, typeSuite, expressionSuite ]

patternSuite :: Test
patternSuite = testGroup "parsing patterns"
  [ testPat "_"                       (WildP ())
  , testPat "x"                       (IdentP (ByValue Immutable) "x" Nothing ())
  , testPat "& x"                     (RefP (IdentP (ByValue Immutable) "x" Nothing ()) Immutable ())
  , testPat "&mut x"                  (RefP (IdentP (ByValue Immutable) "x" Nothing ()) Mutable ())
  , testPat "(x, ref mut y, box z)"   (TupleP [IdentP (ByValue Immutable) "x" Nothing ()
                                              , IdentP (ByRef Mutable) "y" Nothing ()
                                              , BoxP (IdentP (ByValue Immutable) "z" Nothing ()) ()
                                              ] 
                                              Nothing ())
  , testPat "true"                    (LitP (Lit [] (Bool True Unsuffixed ()) ()) ())
  , testPat "-123"                    (LitP (Unary [] Neg (Lit [] (Int 123 Unsuffixed ()) ()) ()) ())
  , testPat "Point { x, y: y1 }"      (StructP (Path False [("Point", AngleBracketed [] [] [] ())] ())
                                               [ FieldPat "x" (IdentP (ByValue Immutable) "x" Nothing ()) True ()
                                               , FieldPat "y" (IdentP (ByValue Immutable) "y" (Just (IdentP (ByValue Immutable) "y1" Nothing ())) ()) False () ]
                                               False ()) 
  ] 

i32 :: Ty ()
i32 = PathTy Nothing (Path False [("i32", AngleBracketed [] [] [] ())] ()) ()

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
  ]

expressionSuite :: Test
expressionSuite = testGroup "parsing expressions" []

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

