{-# LANGUAGE OverloadedStrings, UnicodeSyntax  #-}
module LexerTest (lexerSuite) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Language.Rust.Parser.Lexer
import Language.Rust.Parser.ParseMonad
import Language.Rust.Syntax
import Language.Rust.Data.Ident
import Language.Rust.Data.Position
import Language.Rust.Data.InputStream

lexerSuite :: Test
lexerSuite = testGroup "lexer suite" [ commonCode, literals ]

-- | This contains some random real-life code fragments. The purpose here is
-- primarily black-box testing.
commonCode :: Test
commonCode = testGroup "lexing common code fragments"
  [ testCode "let span = $p.span;"
             [ IdentTok (mkIdent "let")
             , Space Whitespace " "
             , IdentTok (mkIdent "span")
             , Space Whitespace " "
             , Equal
             , Space Whitespace " "
             , Dollar
             , IdentTok (mkIdent "p")
             , Dot
             , IdentTok (mkIdent "span")
             , Semicolon
             ]
  , testCode "$(p.span),+"
             [ Dollar
             , OpenDelim Paren
             , IdentTok (mkIdent "p")
             , Dot
             , IdentTok (mkIdent "span")
             , CloseDelim Paren
             , Comma
             , Plus
             ]
  , testCode "pub s: pp::Printer<'a>,"
             [ IdentTok (mkIdent "pub")
             , Space Whitespace " "
             , IdentTok (mkIdent "s")
             , Colon
             , Space Whitespace " "
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
             , Space Whitespace " "
             , IdentTok (mkIdent "Tr")
             , Space Whitespace " "
             , IdentTok (mkIdent "for")
             , Space Whitespace " "
             , Ampersand
             , LifetimeTok (mkIdent "a")
             , Space Whitespace " "
             , IdentTok (mkIdent "T")
             , Space Whitespace " "
             , OpenDelim Brace
             , CloseDelim Brace
             ]
  , testCode "x /* some comment */ y"
             [ IdentTok (mkIdent "x")
             , Space Whitespace " "
             , Space Comment " some comment "
             , Space Whitespace " "
             , IdentTok (mkIdent "y")
             ]
  , testCode "x /* some /* nested */ comment */ y"
             [ IdentTok (mkIdent "x")
             , Space Whitespace " "
             , Space Comment " some /* nested */ comment "
             , Space Whitespace " "
             , IdentTok (mkIdent "y")
             ]
   , testCode "fn ܐ_ܐ() { println!(\"Hello, čušpajž日本語\"); }"
              [ IdentTok (mkIdent "fn")
              , Space Whitespace " "
              , IdentTok (mkIdent "ܐ_ܐ")
              , OpenDelim Paren
              , CloseDelim Paren
              , Space Whitespace " "
              , OpenDelim Brace
              , Space Whitespace " "
              , IdentTok (mkIdent "println")
              , Exclamation
              , OpenDelim Paren
              , LiteralTok (StrTok "Hello, čušpajž日本語") Nothing
              , CloseDelim Paren
              , Semicolon
              , Space Whitespace " "
              , CloseDelim Brace
              ]
  , testCode "123.f32"
              [ LiteralTok (IntegerTok "123") Nothing
              , Dot
              , IdentTok (mkIdent "f32")
              ]
  , testCode "0e+10"
              [ LiteralTok (FloatTok "0e+10") Nothing
              ]
  , testCode "123.+1"
              [ LiteralTok (FloatTok "123.") Nothing
              , Plus
              , LiteralTok (IntegerTok "1") Nothing
              ]

  ]


-- | test group for literals. Note that literals can have any suffix (even if
-- almost all of those suffixes end up being invalid).
literals :: Test
literals = testGroup "literals (numbers, characters, strings, etc.)"
  -- byte's
  [ testCode "b'a'" [ LiteralTok (ByteTok "a") Nothing ]
  , testCode "b'\\n'" [ LiteralTok (ByteTok "\\n") Nothing ]
  , testCode "b'a'suffix" [ LiteralTok (ByteTok "a") (Just "suffix") ]
  -- char's
  , testCode "'a'" [ LiteralTok (CharTok "a") Nothing ]
  , testCode "'\\n'" [ LiteralTok (CharTok  "\\n") Nothing ]
  , testCode "'a'suffix" [ LiteralTok (CharTok "a") (Just "suffix") ]
  -- integers
  , testCode "123" [ LiteralTok (IntegerTok "123") Nothing ]
  , testCode "123i32" [ LiteralTok (IntegerTok "123") (Just "i32") ]
  , testCode "0b1100_1101" [ LiteralTok (IntegerTok "0b1100_1101") Nothing ]
  , testCode "0b1100_1101isize" [ LiteralTok (IntegerTok "0b1100_1101") (Just "isize") ]
  , testCode "0o3170" [ LiteralTok (IntegerTok "0o3170") Nothing ]
  , testCode "0o3170i64" [ LiteralTok (IntegerTok "0o3170") (Just "i64") ]
  , testCode "0xAFAC" [ LiteralTok (IntegerTok "0xAFAC") Nothing ]
  , testCode "0xAFACu32" [ LiteralTok (IntegerTok "0xAFAC") (Just "u32") ]
  -- float's
  , testCode "123." [ LiteralTok (FloatTok "123.") Nothing ]
  , testCode "123.1" [ LiteralTok (FloatTok "123.1") Nothing ]
  , testCode "123.1f32" [ LiteralTok (FloatTok "123.1") (Just "f32") ]
  , testCode "123e-9f32" [ LiteralTok (FloatTok "123e-9") (Just "f32") ]
  , testCode "9e+10" [ LiteralTok (FloatTok "9e+10") Nothing ]
  -- string's
  , testCode "\"hello \\n world!\"" [ LiteralTok (StrTok "hello \\n world!") Nothing ]
  , testCode "\"hello \\n world!\"suffix" [ LiteralTok (StrTok "hello \\n world!") (Just "suffix") ]
  -- raw string's
  , testCode "r\"hello \n world!\"" [ LiteralTok (StrRawTok "hello \n world!" 0) Nothing ]
  , testCode "r\"hello \n world!\"suffix" [ LiteralTok (StrRawTok "hello \n world!" 0) (Just "suffix") ]
  , testCode "r##\"hello \"#\n world!\"##suffix" [ LiteralTok (StrRawTok "hello \"#\n world!" 2) (Just "suffix") ]
  -- bytestring's
  , testCode "b\"hello \\n world!\"" [ LiteralTok (ByteStrTok "hello \\n world!") Nothing ]
  , testCode "b\"hello \\n world!\"suffix" [ LiteralTok (ByteStrTok "hello \\n world!") (Just "suffix") ]
  -- raw bytestring's
  , testCode "br\"hello \n world!\"" [ LiteralTok (ByteStrRawTok "hello \n world!" 0) Nothing ]
  , testCode "br\"hello \n world!\"suffix" [ LiteralTok (ByteStrRawTok "hello \n world!" 0) (Just "suffix") ]
  , testCode "br##\"hello \"#\n world!\"##suffix" [ LiteralTok (ByteStrRawTok "hello \"#\n world!" 2) (Just "suffix") ]
  -- multiline strings
  , testCode "\"hello \\\n     world!\"" [ LiteralTok (StrTok "hello \\\n     world!") Nothing ]
  , testCode "b\"hello \\\n     world!\"" [ LiteralTok (ByteStrTok "hello \\\n     world!") Nothing ]
  ]

-- | Create a test for a code fragment that should tokenize.
testCode :: String -> [Token] -> Test
testCode inp toks = testCase inp $ Right toks @=? lexTokensNoSpans (inputStreamFromString inp)

-- | Turn an InputStream into either an error or a list of tokens.
lexTokensNoSpans :: InputStream -> Either ParseFail [Token]
lexTokensNoSpans inp = map unspan <$> execParser (lexTokens lexToken) inp initPos

