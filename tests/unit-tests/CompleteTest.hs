{-# LANGUAGE ScopedTypeVariables #-}
module CompleteTest (completeSuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Syntax (SourceFile)
import Language.Rust.Parser (parse, Span, inputStreamFromString)
import Language.Rust.Pretty (pretty)

import Text.PrettyPrint.Annotated.WL (renderPretty, display)

-- The following tests render with width 50 and ribbon length 50 too.
--  |                                                |

completeSuite :: Test 
completeSuite = testGroup "complete suite"
  [ testComplete "short mod"
    "mod foo { }"
  , testComplete "short function in mod"
    "mod foo {\n\
    \  pub fn bar(x: i32) -> i32 {\n\
    \    return x + 1\n\
    \  }\n\
    \}"
  , functionArgs
  , methodCalls
  , lets
  , generics
  , whereClauses
  ] 

functionArgs :: Test
functionArgs = testGroup "function args"
  [ testComplete "function with args"
    "fn foo(\n\
    \  u: i32,\n\
    \  v: i32,\n\
    \  x: i32,\n\
    \  y: i32,\n\
    \  z: i32,\n\
    \  w: i32,\n\
    \) -> i32 { }"
  , testComplete "short function with args"
    "fn foo(x: i32, y: i32) -> i32 { }"
  ]

methodCalls :: Test
methodCalls = testGroup "method calls"
  [ testComplete "short chained method call"
    "fn foo() {\n\
    \  obj.bar().baz();\n\
    \}"
  , testComplete "long chained method call"
    "fn foo() {\n\
    \  (foo as ObjectBuilderFactory)\n\
    \    .baaaaaaaaaaar(foo, bar, baz)\n\
    \    .baaaaaaaaaaz(\n\
    \      fooooooooo,\n\
    \      baaaaaaaar,\n\
    \      baaaaaaaaz,\n\
    \    )\n\
    \    .clone();\n\
    \}"
  ]

lets :: Test
lets = testGroup "let statements"
  [ testComplete "short let"
    "fn foo() {\n\
    \  let shortVar: i32 = otherVariable;\n\
    \}"
  , testComplete "long let"
    "fn foo() {\n\
    \  let looooooooooooooooooongVar: i32 =\n\
    \    otherVariable;\n\
    \}"
  , testComplete "longer let"
    "fn foo() {\n\
    \  let looooooooooooooooooongVar:\n\
    \    loooooooooooooooooooongType =\n\
    \    otherVariable;\n\
    \}"
  ]

generics :: Test
generics = testGroup "generics"
  [ testComplete "one line"
    "fn func1<T, U>(x: Vec<T>, y: Vec<U>) {\n\
    \  return;\n\
    \}"
  , testComplete "break arguments before generics"
    "fn func1<T: Display, U: Debug>(\n\
    \  looooooooooongx: Vec<T>,\n\
    \  y: Vec<U>,\n\
    \) {\n\
    \  return;\n\
    \}"
  , testComplete "break generics"
    "fn ridiculously_long_name_1<\n\
    \  ExcessivelyLongGenericName,\n\
    \  AnotherExcessivelyLongGenericName,\n\
    \>(\n\
    \  x: Vec<ExcessivelyLongGenericName>,\n\
    \  y: Vec<AnotherExcessivelyLongGenericName>,\n\
    \) -> ReturnType {\n\
    \  return;\n\
    \}"
  ]

whereClauses :: Test
whereClauses = testGroup "where clauses"
  [ testComplete "where"
    "fn function<T, U>(args)\n\
    \where\n\
    \  T: Bound,\n\
    \  U: AnotherBound,\n\
    \{\n\
    \  body\n\
    \}"
  ]

testComplete :: String -> String -> Test
testComplete name inp = testCase name $ do
  -- Parse the file
  x :: SourceFile Span
    <- case parse (inputStreamFromString inp) of
         Left (pos,msg) -> fail $ show pos ++ " " ++ msg
         Right x -> pure x

  -- Pretty-print it
  let inp' = display $ renderPretty 1.0 50 $ pretty x

  -- Assert that the input and output are the same
  inp @=? inp'
