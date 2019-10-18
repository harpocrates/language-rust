{-# LANGUAGE QuasiQuotes #-}
module QuoteTest(quoteSuite) where

import Language.Rust.Data.Ident(mkIdent)
import Language.Rust.Quote
import Language.Rust.Pretty(Pretty,Resolve,pretty')
import Prelude hiding (lines)
import Test.Framework(Test,testGroup)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

-- | The goal of these is to make sure quoting an unquoting work the way they're
-- supposed to. It will be delicate in the face of changing renderers,
-- unfortunately.
quoteSuite :: Test
quoteSuite = testGroup "quote suite"
  [ quoteTest "Standard Quote (lit)"
              "23"
              [lit| 23 |]
  --
  , quoteTest "Standard Quote (1+1)"
              "1 + 1"
              [expr| 1 + 1 |]
  --
  , quoteTest "Unquote literal (1+1)"
              "1 + 1"
              [expr| $$(one) + 1 |]
  --
  , quoteTest "Unquote if test"
              "if 2 > 1 { return true; }"
              [stmt| if $$(tg1) { return true; } |]
  --
  , quoteTest "Unquote if consequent"
              "if 2 > 1 { return true; }"
              [stmt| if 2 > 1 { $${ret1} } |]
  --
  , quoteTest "Unquote data type"
              "struct Foo {\n  inner: u64,\n}"
              [item| struct $$name { inner: u64 } |]
  --
  , quoteTest "Unquote field type"
              "struct Foo {\n  inner: u64,\n}"
              [item| struct $$name { $$field: u64 } |]
  --
  , quoteTest "Unquote field array type"
              "struct Foo {\n  inner: [u64; 1],\n}"
              [item| struct $$name { $$field: [u64; $$(one)] } |]
  --
  , quoteTest "Impl trait name"
              "impl Add<Output = Foo> for Foo {\n  fn foo(&self, other: Foo) -> Self {\n    unimplemented!()\n  }\n}"
              [item| impl Add<Output=$$name> for $$name {
                        fn foo(&self, other: $$name) -> Self {
                            unimplemented!()
                        }
                     }|]
  --
  , quoteTest "In macro argument literal"
              "write!(f,\"{:X}\"                              , 1)?;"
              (let hashx = [tokenTree| "{:X}" |]
               in [stmt| write!(f, $$(hashx), 1)?; |])
  , quoteTest "Statement splice"
              "fn foo(x: u64) -> u64 {\n  let mut v = x;\n  v = v / 2;\n  v = v + 1;\n  v\n}"
              [item|
               fn foo(x: u64) -> u64 {
                    let mut v = x;
                    $@{lines};
                    v
               }
              |]
  , quoteTest "Documentation example"
              "fn foo(x: u64) -> u64 {\n  let mut v = 1;\n  v = v + 1;\n  v = v + 1;\n  v = v + 1;\n  v = v + 1;\n  v = v + 1;\n  v = v + 1;\n  v = v + 1;\n  v = v + 1;\n  v = v + 1;\n  v = v + 1;\n  v = v % 10;\n  return v;\n}"
              (let initial_value = [expr| 1 |]
                   return_value  = [stmt| return v; |]
                   plusOnes      = replicate 10 [stmt| v = v + 1; |]
               in [item|
                     fn foo(x: u64) -> u64 {
                       let mut v = $$(initial_value);
                       $@{plusOnes}
                       v = v % 10;
                       $${return_value}
                      }
                  |])
  , quoteTest "Splice into module"
              "mod numbers {\n  const ONE: u64 = 1u64;\n  \n  const TWO: u64 = 2u64;\n}"
              (let constants = [ [item| const ONE: u64 = 1u64; |],
                                 [item| const TWO: u64 = 2u64; |] ]
               in [sourceFile|
                     mod numbers {
                       $@{constants}
                     }
                  |])
  , quoteTest "Splice into top level"
              "const ZERO: u64 = 0u64;\n\nconst ONE: u64 = 1u64;\n\nconst TWO: u64 = 2u64;"
              (let constants = [ [item| const ONE: u64 = 1u64; |],
                                 [item| const TWO: u64 = 2u64; |] ]
               in [sourceFile|
                     const ZERO: u64 = 0u64;
                     $@{constants}
                  |])
  ]
 where
  one = [expr| 1 |]
  tg1 = [expr| 2 > 1 |]
  ret1 = [stmt| return true; |]
  name = mkIdent "Foo"
  field = mkIdent "inner"
  lines = [ [stmt| v = v / 2; |],
            [stmt| v = v + 1; |] ]

quoteTest :: (Pretty a, Resolve a) => String -> String -> a -> Test
quoteTest name res v = testCase name (res @=? show (pretty' v))
