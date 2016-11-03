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
  ]

literals :: Test
literals = testGroup "literals (numbers, characters, strings, etc.)"
  [ testCode "b'a'" [ Literal (Byte (Name "a")) Nothing ]
  , testCode "b'\\n'" [ Literal (Byte (Name "\\n")) Nothing ]
  , testCode "b'a'suffix" [ Literal (Byte (Name "a")) (Just (Name "suffix")) ]
  , testCode "123" [ Literal (Integer (Name "123")) Nothing ]
  , testCode "123i32" [ Literal (Integer (Name "123")) (Just (Name "i32")) ]
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
