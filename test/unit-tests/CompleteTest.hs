{-# LANGUAGE ScopedTypeVariables #-}
module CompleteTest (completeSuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Syntax (SourceFile)
import Language.Rust.Parser (parse, Span, inputStreamFromString, ParseFail(..))
import Language.Rust.Pretty (pretty')

import Data.Text.Prettyprint.Doc (layoutPretty, LayoutOptions(..), PageWidth(..))
import Data.Text.Prettyprint.Doc.Render.String (renderShowS)

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
  , functionCalls
  , methodCalls
  , lets
  , generics
  , whereClauses
  , functions
  , typeAliases
  , traits
  , structs
  , enums
  , matchExpressions
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

functionCalls :: Test
functionCalls = testGroup "function calls"
  [ testComplete "short call"
    "fn main() {\n\
    \  foo(1, 2, 3, 4);\n\
    \}"
  , testComplete "multi line call"
    "fn main() {\n\
    \  foo(\n\
    \    foooooooooooooooo,\n\
    \    baaaaaaaaaaaaaaar,\n\
    \    baaaaaaaaaaaaaaaz,\n\
    \  );\n\
    \}"
  , testComplete "nested mutli line call"
    "fn main() {\n\
    \  foo(\n\
    \    0,\n\
    \    bar(\n\
    \      foooooooooooooooo,\n\
    \      baaaaaaaaaaaaaaar,\n\
    \      baaaaaaaaaaaaaaaz,\n\
    \    ),\n\
    \  );\n\
    \}"
  , testComplete "nested one-arg multi line call"
    "fn main() {\n\
    \  foo(bar(\n\
    \    foooooooooooooooo,\n\
    \    baaaaaaaaaaaaaaar,\n\
    \    baaaaaaaaaaaaaaaz,\n\
    \  ));\n\
    \}"
  , testComplete "nested one-arg multi line call"
    "fn main() {\n\
    \  foo(bar(baz(boo(far(faz(\n\
    \    foooooooooooooooo,\n\
    \    baaaaaaaaaaaaaaar,\n\
    \    baaaaaaaaaaaaaaaz,\n\
    \  ))))));\n\
    \}"
  ]

methodCalls :: Test
methodCalls = testGroup "method calls"
  [ testComplete "short chained method call"
    "fn foo() {\n\
    \  obj.bar()?.baz();\n\
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
  , testComplete "long chained method call / fields / try / index"
    "fn foo() {\n\
    \  (foo as ObjectBuilderFactory)\n\
    \    .baaaaaaaaaaar(foo, bar, baz)\n\
    \    .foo\n\
    \    .bar[0]\n\
    \    .baz?\n\
    \    .baf[0][0].4\n\
    \    .bar(baaaaaaaaaaz(\n\
    \      fooooooooo,\n\
    \      baaaaaaaar,\n\
    \      baaaaaaaaz,\n\
    \    ))\n\
    \    .clone();\n\
    \}"
  , testComplete "long caller"
    "fn foo() {\n\
    \  Point {\n\
    \    withExceeeeeeeeeedingly: 1,\n\
    \    looooooooooooongFields: 2,\n\
    \  }\n\
    \    .baaaaaaaaaaar(foo, bar, baz)\n\
    \    .foo\n\
    \    .bar[0]\n\
    \    .baz?\n\
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

-- See <https://github.com/rust-lang-nursery/fmt-rfcs/issues/29>
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
  , testComplete "break one generic"
    "fn ridiculously_long_name_1<\n\
    \  AnotherExcessivelyLongGenericName,\n\
    \>(\n\
    \  x: Vec<ExcessivelyLongGenericName>,\n\
    \  y: Vec<AnotherExcessivelyLongGenericName>,\n\
    \) -> ReturnType {\n\
    \  return;\n\
    \}"
  , testComplete "break two generics"
    "fn ridiculously_long_name_1<\n\
    \  ExcessivelyLongGenericName,\n\
    \  AnotherExcessivelyLongGenericName,\n\
    \>(\n\
    \  x: Vec<ExcessivelyLongGenericName>,\n\
    \  y: Vec<AnotherExcessivelyLongGenericName>,\n\
    \) -> ReturnType {\n\
    \  return;\n\
    \}"
  , testComplete "break generics and bounds"
    "fn ridiculously_long_name_1<\n\
    \  ExcessivelyLongGenericName:\n\
    \    Sized +\n\
    \    PartialEq +\n\
    \    PartialOrd +,\n\
    \  AnotherExcessivelyLongGenericName: Sized,\n\
    \>(\n\
    \  x: Vec<ExcessivelyLongGenericName>,\n\
    \  y: Vec<AnotherExcessivelyLongGenericName>,\n\
    \) -> ReturnType {\n\
    \  return;\n\
    \}"
  ]

-- See <https://github.com/rust-lang-nursery/fmt-rfcs/issues/38>
whereClauses :: Test
whereClauses = testGroup "where clauses"
  [ testComplete "where"
    "fn function<T, U>(args: i32)\n\
    \where\n\
    \  T: Bound,\n\
    \  U: AnotherBound,\n\
    \{\n\
    \  body\n\
    \}"
  , testComplete "method without body where"
    "trait T {\n\
    \  fn foo<T, U>() -> ReturnType\n\
    \  where\n\
    \    T: Bound,\n\
    \    U: AnotherBound;\n\
    \}"
  , testComplete "long where"
    "fn itemize_list<'a, T, I, F1, F2, F3>(\n\
    \  codemap: &'a CodeMap,\n\
    \  inner: I,\n\
    \  terminator: &'a str,\n\
    \  get_lo: F1,\n\
    \  get_hi: F2,\n\
    \  get_item_string: F3,\n\
    \  prev_span_end: BytePos,\n\
    \  next_span_start: BytePos,\n\
    \) -> ListItems<'a, I, F1, F2, F3>\n\
    \where\n\
    \  I: Iterator<Item = T>,\n\
    \  F1: Fn(&T) -> BytePos,\n\
    \  F2: Fn(&T) -> BytePos,\n\
    \  F3: Fn(&T) -> Option<String>,\n\
    \{\n\
    \  ListItems {\n\
    \    codemap: codemap,\n\
    \    inner: inner.peekable(),\n\
    \    get_lo: get_lo,\n\
    \    get_hi: get_hi,\n\
    \    get_item_string: get_item_string,\n\
    \    prev_span_end: prev_span_end,\n\
    \    next_span_start: next_span_start,\n\
    \    terminator: terminator,\n\
    \  }\n\
    \}"
  , testComplete "impl no where"
    "impl<K: Hash + Eq, V> HashMap<K, V> {\n\
    \  fn add(key: K, value: V) { }\n\
    \}"
  , testComplete "impl"
    "impl<K, V> HashMap<K, V>\n\
    \where\n\
    \  K: Hash + Eq,\n\
    \{\n\
    \  fn add(key: K, value: V) { }\n\
    \}"
  ]

functions :: Test
functions = testGroup "functions"
  -- The '-> ret_ty' should never be on a line of its own - split the args instead.
  -- See <https://github.com/rust-lang-nursery/fmt-rfcs/issues/77>
  [ testComplete "long return type"
    "fn bit(\n\
    \  x: i32,\n\
    \) -> LoooooooooooooooooooooooongType { }"
  ]

-- See <https://github.com/rust-lang-nursery/fmt-rfcs/issues/32>
typeAliases :: Test
typeAliases = testGroup "type aliases"
  [ testComplete "type alias short"
    "type FormattedAlias<T: Copy> = Vec<T>;"
  , testComplete "type alias long"
    "type LoooooooooooooooonnnnnnnnnnnnnngAlias =\n\
    \  Vec<Loooooooooooong>;"
  , testComplete "type alias where"
    "type FormattedAlias<T>\n\
    \where\n\
    \  T: Copy,\n\
    \= Vec<T>;"
  ]

traits :: Test
traits = testGroup "traits"
  [ testComplete "simple trait"
    "trait Animal {\n\
    \  fn new(name: &'static str) -> Self;\n\
    \}"
  , testComplete "generic trait"
    "trait DoubleDrop<T> {\n\
    \  fn double_drop(self, _: T);\n\
    \}"
  , testComplete "trait with bounds"
    "pub trait Ord: Eq + PartialOrd<Self> {\n\
    \  fn cmp(&self, other: &Self) -> Ordering;\n\
    \}"
  ]

structs :: Test
structs = testGroup "structs"
  [ testComplete "unit struct"
    "struct Bleh;"
  , testComplete "tuple struct"
    "struct Bleh(i32, i32);"
  , testComplete "regular struct"
    "struct Baz {\n\
    \  field: i32,\n\
    \}"
  , testComplete "generic unit struct"
    "struct Bleh<T: Copy, U: Sized>;"
  , testComplete "generic tuple struct"
    "struct Bleh<T: Copy, U: Sized>(T, U);"
  , testComplete "generic regular struct"
    "struct Baz<T: Copy> {\n\
    \  field: T,\n\
    \}"
  , testComplete "where unit struct"
    "struct Bleh<T, U>\n\
    \where\n\
    \  T: Copy,\n\
    \  U: Sized;"
  , testComplete "where tuple struct"
    "struct Bleh<T, U>(T, U)\n\
    \where\n\
    \  T: Copy,\n\
    \  U: Sized;"
  , testComplete "where regular struct"
    "struct Baz<T>\n\
    \where\n\
    \  T: Copy,\n\
    \{\n\
    \  field: T,\n\
    \}"
  ]

enums :: Test
enums = testGroup "enums"
  [ testComplete "empty enum"
    "enum Foo { }"
  , testComplete "basic enum"
    "enum Foo {\n\
    \  UnitCon,\n\
    \  UnitCon = 3,\n\
    \  Baz {\n\
    \    foo: i32,\n\
    \    bar: (),\n\
    \  },\n\
    \  Bar(i32, i32),\n\
    \}"
  , testComplete "generic enum"
    "enum Foo<T: Sized> {\n\
    \  UnitCon,\n\
    \  UnitCon = 3,\n\
    \  Baz {\n\
    \    foo: T,\n\
    \    bar: (),\n\
    \  },\n\
    \  Bar(T, i32),\n\
    \}"
  , testComplete "where enum"
    "enum Foo<T>\n\
    \where\n\
    \  T: Sized,\n\
    \{\n\
    \  UnitCon,\n\
    \  UnitCon = 3,\n\
    \  Baz {\n\
    \    foo: T,\n\
    \    bar: (),\n\
    \  },\n\
    \  Bar(T, i32),\n\
    \}"
  ]

-- See <https://github.com/rust-lang-nursery/fmt-rfcs/issues/34>
matchExpressions :: Test
matchExpressions = testGroup "match expressions"
  [ testComplete "empty match"
    "fn foo() {\n\
    \  match expr { }\n\
    \}"
  , testComplete "simple match"
    "fn foo() {\n\
    \  match expr {\n\
    \    0 => 1,\n\
    \    1 => { 2 },\n\
    \    2 => 3,\n\
    \  }\n\
    \}"
  , testComplete "multiple patterns one line match"
    "fn foo() {\n\
    \  match expr {\n\
    \    0 => 1,\n\
    \    1 | 2 | 3 | 4 => { 2 },\n\
    \    5 => 3,\n\
    \  }\n\
    \}"
  , testComplete "multiple patterns multiple lines match"
    "fn foo() {\n\
    \  match expr {\n\
    \    0 => 1,\n\
    \    1432482379423 |\n\
    \    2423894732 |\n\
    \    3423423 |\n\
    \    4234273 => { 2 },\n\
    \    5 => 3,\n\
    \  }\n\
    \}"
  ]

testComplete :: String -> String -> Test
testComplete name inp = testCase name $ do
  -- Parse the file
  x :: SourceFile Span
    <- case parse (inputStreamFromString inp) of
         Left (ParseFail pos msg) -> fail $ show pos ++ " " ++ msg
         Right x -> pure x

  -- Pretty print it
  let opts = LayoutOptions (AvailablePerLine 50 1.0)
      inp' = renderShowS (layoutPretty opts (pretty' x)) ""
  
  -- Assert that the input and output are the same
  inp @=? inp'
