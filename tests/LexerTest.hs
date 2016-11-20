{-# LANGUAGE OverloadedStrings, UnicodeSyntax  #-}
module LexerTest (lexerSuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Parser.Lexer
import Language.Rust.Parser.ParseMonad
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident
import Language.Rust.Data.Position
import Language.Rust.Data.InputStream

import Control.Monad
import Control.Monad.Trans.Except

lexerSuite :: Test
lexerSuite = testGroup "lexer suite" [ commonCode, literals ]

-- | This contains some random real-life code fragments. The purpose here is 
-- primarily black-box testing.
commonCode :: Test
commonCode = testGroup "lexing common code fragments"
  [ testCode "let span = $p.span;" 
             [ IdentTok (mkIdent "let")
             , Space Whitespace (Name " ")
             , IdentTok (mkIdent "span")
             , Space Whitespace (Name " ")
             , Equal
             , Space Whitespace (Name " ")
             , Dollar
             , IdentTok (mkIdent "p")
             , Dot
             , IdentTok (mkIdent "span")
             , Semicolon
             ]
  , testCode "pub s: pp::Printer<'a>,"
             [ IdentTok (mkIdent "pub")
             , Space Whitespace (Name " ")
             , IdentTok (mkIdent "s")
             , Colon
             , Space Whitespace (Name " ")
             , IdentTok (mkIdent "pp")
             , ModSep
             , IdentTok (mkIdent "Printer")
             , Less
             , LifetimeTok (mkIdent "a")
             , Greater
             , Comma
             ]
  , testCode "impl<'a,T> Tr for &'a T {}"
             [ IdentTok (mkIdent "impl")
             , Less
             , LifetimeTok (mkIdent "a")
             , Comma
             , IdentTok (mkIdent "T")
             , Greater
             , Space Whitespace (Name " ")
             , IdentTok (mkIdent "Tr")
             , Space Whitespace (Name " ")
             , IdentTok (mkIdent "for")
             , Space Whitespace (Name " ")
             , Ampersand
             , LifetimeTok (mkIdent "a")
             , Space Whitespace (Name " ")
             , IdentTok (mkIdent "T")
             , Space Whitespace (Name " ")
             , OpenDelim Brace
             , CloseDelim Brace
             ]
  , testCode "x /* some comment */ y"
             [ IdentTok (mkIdent "x")
             , Space Whitespace (Name " ")
             , Space Comment (Name "comment not captured (TODO)")
             , Space Whitespace (Name " ")
             , IdentTok (mkIdent "y")
             ]
  , testCode "x /* some /* nested */ comment */ y"
             [ IdentTok (mkIdent "x")
             , Space Whitespace (Name " ")
             , Space Comment (Name "comment not captured (TODO)")
             , Space Whitespace (Name " ")
             , IdentTok (mkIdent "y")
             ]
   , testCode "fn ܐ_ܐ() { println!(\"Hello, čušpajž日本語\"); }"
              [ IdentTok (mkIdent "fn")
              , Space Whitespace (Name " ")
              , IdentTok (mkIdent "ܐ_ܐ")
              , OpenDelim Paren 
              , CloseDelim Paren
              , Space Whitespace (Name " ")
              , OpenDelim Brace
              , Space Whitespace (Name " ")
              , IdentTok (mkIdent "println")
              , Exclamation
              , OpenDelim Paren
              , LiteralTok (StrTok (Name "Hello, čušpajž日本語")) Nothing
              , CloseDelim Paren
              , Semicolon
              , Space Whitespace (Name " ")
              , CloseDelim Brace
              ]
  ]


-- | test group for literals. Note that literals can have any suffix (even if
-- almost all of those suffixes end up being invalid).
literals :: Test
literals = testGroup "literals (numbers, characters, strings, etc.)"
  -- byte's
  [ testCode "b'a'" [ LiteralTok (ByteTok (Name "a")) Nothing ]
  , testCode "b'\\n'" [ LiteralTok (ByteTok (Name "\\n")) Nothing ]
  , testCode "b'a'suffix" [ LiteralTok (ByteTok (Name "a")) (Just (Name "suffix")) ]
  -- char's
  , testCode "'a'" [ LiteralTok (CharTok (Name "a")) Nothing ]
  , testCode "'\\n'" [ LiteralTok (CharTok (Name "\\n")) Nothing ]
  , testCode "'a'suffix" [ LiteralTok (CharTok (Name "a")) (Just (Name "suffix")) ]
  -- integers
  , testCode "123" [ LiteralTok (IntegerTok (Name "123")) Nothing ]
  , testCode "123i32" [ LiteralTok (IntegerTok (Name "123")) (Just (Name "i32")) ]
  , testCode "0b1100_1101" [ LiteralTok (IntegerTok (Name "0b1100_1101")) Nothing ]
  , testCode "0b1100_1101isize" [ LiteralTok (IntegerTok (Name "0b1100_1101")) (Just (Name "isize")) ]
  , testCode "0o3170" [ LiteralTok (IntegerTok (Name "0o3170")) Nothing ]
  , testCode "0o3170i64" [ LiteralTok (IntegerTok (Name "0o3170")) (Just (Name "i64")) ]
  , testCode "0xAFAC" [ LiteralTok (IntegerTok (Name "0xAFAC")) Nothing ]
  , testCode "0xAFACu32" [ LiteralTok (IntegerTok (Name "0xAFAC")) (Just (Name "u32")) ]
  -- float's
  , testCode "123.1" [ LiteralTok (FloatTok (Name "123.1")) Nothing ]
  , testCode "123.f32" [ LiteralTok (IntegerTok (Name "123")) Nothing, Dot, IdentTok (mkIdent "f32") ]
  , testCode "123.1f32" [ LiteralTok (FloatTok (Name "123.1")) (Just (Name "f32")) ]
  , testCode "123e-9f32" [ LiteralTok (FloatTok (Name "123e-9")) (Just (Name "f32")) ]
  -- string's
  , testCode "\"hello \\n world!\"" [ LiteralTok (StrTok (Name "hello \\n world!")) Nothing ]
  , testCode "\"hello \\n world!\"suffix" [ LiteralTok (StrTok (Name "hello \\n world!")) (Just (Name "suffix")) ]
  -- raw string's
  , testCode "r\"hello \n world!\"" [ LiteralTok (StrRawTok (Name "hello \n world!") 0) Nothing ]
  , testCode "r\"hello \n world!\"suffix" [ LiteralTok (StrRawTok (Name "hello \n world!") 0) (Just (Name "suffix")) ]
  , testCode "r##\"hello \"#\n world!\"###suffix" [ LiteralTok (StrRawTok (Name "hello \"#\n world!") 2) (Just (Name "suffix")) ]
  -- bytestring's
  , testCode "b\"hello \\n world!\"" [ LiteralTok (ByteStrTok (Name "hello \\n world!")) Nothing ]
  , testCode "b\"hello \\n world!\"suffix" [ LiteralTok (ByteStrTok (Name "hello \\n world!")) (Just (Name "suffix")) ]
  -- raw bytestring's
  , testCode "rb\"hello \n world!\"" [ LiteralTok (ByteStrRawTok (Name "hello \n world!") 0) Nothing ]
  , testCode "rb\"hello \n world!\"suffix" [ LiteralTok (ByteStrRawTok (Name "hello \n world!") 0) (Just (Name "suffix")) ]
  , testCode "rb##\"hello \"#\n world!\"###suffix" [ LiteralTok (ByteStrRawTok (Name "hello \"#\n world!") 2) (Just (Name "suffix")) ]
  ]

-- | Create a test for a code fragment that should tokenize.
testCode :: String -> [Token] -> Test
testCode inp toks = testCase inp $ Right toks @=? lexTokensNoSpans (inputStreamFromString inp)

-- | Turn an InputStream into either an error or a list of tokens.
lexTokensNoSpans :: InputStream -> Either (Position,String) [Token]
lexTokensNoSpans inp = runExcept (map unspan <$> tokens)
  where
    tokens :: Except (Position,String) [Spanned Token]
    tokens = execParser lexTokens inp initPos

