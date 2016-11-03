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
             , Whitespace
             , IdentTok (mkIdent "span")
             , Whitespace
             , Eq
             , Whitespace
             , Dollar
             , IdentTok (mkIdent "p")
             , Dot
             , IdentTok (mkIdent "span")
             , Semi
             ]
  , testCode "pub s: pp::Printer<'a>,"
             [ IdentTok (mkIdent "pub")
             , Whitespace
             , IdentTok (mkIdent "s")
             , Colon
             , Whitespace
             , IdentTok (mkIdent "pp")
             , ModSep
             , IdentTok (mkIdent "Printer")
             , Lt
             , Lifetime (mkIdent "a")
             , Gt
             , Comma
             ]
  , testCode "impl<'a,T> Tr for &'a T {}"
             [ IdentTok (mkIdent "impl")
             , Lt
             , Lifetime (mkIdent "a")
             , Comma
             , IdentTok (mkIdent "T")
             , Gt
             , Whitespace
             , IdentTok (mkIdent "Tr")
             , Whitespace
             , IdentTok (mkIdent "for")
             , Whitespace
             , BinOp And
             , Lifetime (mkIdent "a")
             , Whitespace
             , IdentTok (mkIdent "T")
             , Whitespace
             , OpenDelim Brace
             , CloseDelim Brace
             ]
  , testCode "x /* some comment */ y"
             [ IdentTok (mkIdent "x")
             , Whitespace
             , Comment
             , Whitespace
             , IdentTok (mkIdent "y")
             ]
  , testCode "x /* some /* nested */ comment */ y"
             [ IdentTok (mkIdent "x")
             , Whitespace
             , Comment
             , Whitespace
             , IdentTok (mkIdent "y")
             ]
  -- , testCode "fn ಠ_ಠ() { println!(\"Hello, 世界\"); }"
  --            [ IdentTok (mkIdent "fn")
  --            , Whitespace
  --            , IdentTok (mkIdent "ಠ_ಠ")
  --            , OpenDelim Paren 
  --            , CloseDelim Paren
  --            , Whitespace
  --            , OpenDelim Brace
  --            , Whitespace
  --            , IdentTok (mkIdent "prinln")
  --            , Not
  --            , OpenDelim Paren
  --            , Literal (Str_ (Name "Hello, 世界")) Nothing
  --            , CloseDelim Paren
  --            , Semi
  --            , Whitespace
  --            , CloseDelim Brace
  --            ]
  ]


-- | test group for literals. Note that literals can have any suffix (even if
-- almost all of those suffixes end up being invalid).
literals :: Test
literals = testGroup "literals (numbers, characters, strings, etc.)"
  -- byte's
  [ testCode "b'a'" [ Literal (Byte (Name "a")) Nothing ]
  , testCode "b'\\n'" [ Literal (Byte (Name "\\n")) Nothing ]
  , testCode "b'a'suffix" [ Literal (Byte (Name "a")) (Just (Name "suffix")) ]
  -- char's
  , testCode "'a'" [ Literal (Char (Name "a")) Nothing ]
  , testCode "'\\n'" [ Literal (Char (Name "\\n")) Nothing ]
  , testCode "'a'suffix" [ Literal (Char (Name "a")) (Just (Name "suffix")) ]
  -- integers
  , testCode "123" [ Literal (Integer (Name "123")) Nothing ]
  , testCode "123i32" [ Literal (Integer (Name "123")) (Just (Name "i32")) ]
  , testCode "0b1100_1101" [ Literal (Integer (Name "0b1100_1101")) Nothing ]
  , testCode "0b1100_1101isize" [ Literal (Integer (Name "0b1100_1101")) (Just (Name "isize")) ]
  , testCode "0o3170" [ Literal (Integer (Name "0o3170")) Nothing ]
  , testCode "0o3170i64" [ Literal (Integer (Name "0o3170")) (Just (Name "i64")) ]
  , testCode "0xAFAC" [ Literal (Integer (Name "0xAFAC")) Nothing ]
  , testCode "0xAFACu32" [ Literal (Integer (Name "0xAFAC")) (Just (Name "u32")) ]
  -- float's
  , testCode "123.1" [ Literal (Float (Name "123.1")) Nothing ]
  , testCode "123.f32" [ Literal (Integer (Name "123")) Nothing, Dot, IdentTok (mkIdent "f32") ]
  , testCode "123.1f32" [ Literal (Float (Name "123.1")) (Just (Name "f32")) ]
  , testCode "123e-9f32" [ Literal (Float (Name "123e-9")) (Just (Name "f32")) ]
  -- string's
  , testCode "\"hello \\n world!\"" [ Literal (Str_ (Name "hello \\n world!")) Nothing ]
  , testCode "\"hello \\n world!\"suffix" [ Literal (Str_ (Name "hello \\n world!")) (Just (Name "suffix")) ]
  -- raw string's
  , testCode "r\"hello \n world!\"" [ Literal (StrRaw (Name "hello \n world!") 0) Nothing ]
  , testCode "r\"hello \n world!\"suffix" [ Literal (StrRaw (Name "hello \n world!") 0) (Just (Name "suffix")) ]
  , testCode "r##\"hello \"#\n world!\"###suffix" [ Literal (StrRaw (Name "hello \"#\n world!") 2) (Just (Name "suffix")) ]
  -- bytestring's
  , testCode "b\"hello \\n world!\"" [ Literal (ByteStr (Name "hello \\n world!")) Nothing ]
  , testCode "b\"hello \\n world!\"suffix" [ Literal (ByteStr (Name "hello \\n world!")) (Just (Name "suffix")) ]
  -- raw bytestring's
  , testCode "rb\"hello \n world!\"" [ Literal (ByteStrRaw (Name "hello \n world!") 0) Nothing ]
  , testCode "rb\"hello \n world!\"suffix" [ Literal (ByteStrRaw (Name "hello \n world!") 0) (Just (Name "suffix")) ]
  , testCode "rb##\"hello \"#\n world!\"###suffix" [ Literal (ByteStrRaw (Name "hello \"#\n world!") 2) (Just (Name "suffix")) ]
  ]

-- | Create a test for a code fragment that should tokenize.
testCode :: InputStream -> [Token] -> Test
testCode inp toks = testCase (show inp) $ Right toks @=? lexTokensNoSpans inp

-- | Turn an InputStream into either an error or a list of tokens.
lexTokensNoSpans :: InputStream -> Either (Position,String) [Token]
lexTokensNoSpans inp = runExcept (map unspan <$> tokens)
  where
    tokens :: Except (Position,String) [Spanned Token]
    tokens = execParser lexTokens inp initPos
