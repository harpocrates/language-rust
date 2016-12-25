{-# LANGUAGE OverloadedStrings #-}
module PrettyTest (prettySuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Syntax.AST
import Language.Rust.Syntax.Token
import Language.Rust.Pretty

import Control.Monad
import Control.Monad.Trans.Except
import Text.PrettyPrint (render, Doc)

prettySuite :: Test
prettySuite = testGroup "pretty suite" [ commonCode, literals ]

-- | This contains some random real-life code fragments. The purpose here is 
-- primarily black-box testing.
commonCode :: Test
commonCode = testGroup "printing common code fragments" []

literals :: Test
literals = testGroup "printing literals"
  [ testRender "\"hello world\"" (printLit (Str "hello world" Cooked Unsuffixed ()))
  , testRender "\"hello \\n w\\x90rld\"" (printLit (Str "hello \n w\x90rld" Cooked Unsuffixed ()))
  , testRender "123.45f32" (printLit (Float 123.45 F32 ()))
  , testRender "123isize" (printLit (Int 123 Is ()))
  , testRender "true" (printLit (Bool True Unsuffixed ()))
  , testRender "false" (printLit (Bool False Unsuffixed ()))
  ]

testRender :: String -> Doc -> Test
testRender str doc = testCase str $ str @=? render doc
{-
data Suffix = Unsuffixed | Is | I8 | I16 | I32 | I64 | Us | U8 | U16 |  U32 | U64

Str String StrStyle Suffix a            -- ^ A string ("foo")
  | ByteStr ByteString StrStyle Suffix a    -- ^ A byte string (b"foo")
  | Char Char Suffix a                      -- ^ A character ('a')
  | Byte Word8 Suffix a                     -- ^ A byte (b'f')
  | Int Integer Suffix a                    -- ^ An integer (1)
  | Float Double Suffix a                   -- ^ A float literal (1.12e4)
  | Bool Bool Suffix a             
  -}
