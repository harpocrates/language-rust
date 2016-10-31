{
module Language.Rust.Parser.Lexer2 where

import Language.Rust.Data.InputStream
import Language.Rust.Data.Position
import Language.Rust.Parser.ParseMonad
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident

import Control.Monad (when)
import Data.Word
import Data.Char
}

tokens :-

" "+            ;
"=="            { \_ _ -> pure Eq }

{
-- -----------------------------------------------------------------------------
-- The input type

type Token_ = Spanned Token

type AlexInput = (Position,    -- current position,
                  InputStream) -- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

-- for alex-3.0
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (p,is) | inputStreamEmpty is = Nothing
                   | otherwise  = let (b,s) = takeByte is in
                                  -- this is safe for latin-1, but ugly
                                  let p' = alexMove p (chr (fromIntegral b)) in p' `seq`
                                  Just (b, (p', s))

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,is) | inputStreamEmpty is = Nothing
                   | otherwise  = let (c,s) = takeChar is in
                                  let p' = alexMove p c in p' `seq`
                                  Just (c, (p', s))

alexMove :: Position -> Char -> Position
alexMove pos ' '  = incPos pos 1
alexMove pos '\n' = retPos pos
-- alexMove pos '\r' = incOffset pos 1  -- TODO understand
alexMove pos _    = incPos pos 1

lexToken :: P Token_
lexToken = lexToken' True

lexToken' :: Bool -> P Token_
lexToken' modifyCache = do
  pos <- getPosition
  inp <- getInput
  case alexScan (pos, inp) 0 of
    AlexEOF -> do
        handleEofToken
        return (Spanned Eof (Span pos pos))
    AlexError inp' -> lexicalError
    AlexSkip  (pos', inp') len -> do
        setPosition pos'
        setInput inp'
        lexToken' modifyCache
    AlexToken (pos', inp') len action -> do
        setPosition pos'
        setInput inp'
        tok <- action pos len inp
        when modifyCache $ setLastToken tok
        return (Spanned tok (Span pos pos'))

lexRust :: (Token_ -> P a) -> P a
lexRust = (lexToken >>=)
}

