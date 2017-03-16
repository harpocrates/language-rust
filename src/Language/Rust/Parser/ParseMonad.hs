{-|
Module      : Language.Rust.Parser.ParseMonad
Description : Parsing monad for lexer/parser
Copyright   : (c) Alec Theriault, 2017
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : portable

Both the lexer and the parser run inside of the 'P' monad. As detailed in the section on 
[threaded-lexers](https://www.haskell.org/happy/doc/html/sec-monads.html#sec-lexers) in Happy's
instruction manual, the benefits of this are that:

  * Lexical errors can be treated in the same way as parse errors
  * Information such as the current position in the file shared between the lexer and parser
  * General information can be passed back from the parser to the lexer too

In our case, this shared information is held in 'PState'.
-}

{-# LANGUAGE InstanceSigs #-}

module Language.Rust.Parser.ParseMonad (
  -- * Parsing monad
  P, execParser, initPos, PState(..),
  -- * Monadic operations
  getPState, setPState, getPosition, setPosition, getInput, setInput, popToken, pushToken,
  -- * Error reporting
  parseError,
) where

import Language.Rust.Data.InputStream (InputStream)
import Language.Rust.Data.Position (Spanned, Position, initPos)
import Language.Rust.Syntax.Token (Token)

import Data.Maybe (listToMaybe)
import Control.Monad (liftM, ap)

-- | Parsing and lexing monad. A value of type @'P' a@ represents a parser that can be run (using
-- 'execParser') to possibly produce a value of type @a@.
newtype P a = P { runParser :: PState -> ParseResult a }

-- | The result of running a parser
data ParseResult a
  = Ok a !PState             -- ^ successful output
  | Failed String Position   -- ^ the error message and position

-- | State that the lexer and parser share
data PState = PState
  { curPos       :: !Position        -- ^ position at current input location
  , curInput     :: !InputStream     -- ^ the current input
  , prevPos      ::  Position        -- ^ position at previous input location
  , pushedTokens :: [Spanned Token]  -- ^ tokens manually pushed by the user
  }

instance Functor P where
  fmap  = liftM

instance Applicative P where
  pure = return
  (<*>) = ap

instance Monad P where
  return :: a -> P a
  return a = P (Ok a)

  (>>=) :: P a -> (a -> P b) -> P b
  P m >>= k = P $ \s -> case m s of
                          Ok a s'        -> runParser (k a) s'
                          Failed err pos -> Failed err pos

  fail :: String -> P a
  fail msg = do { pos <- getPosition; P (\_ -> Failed msg pos) }

-- | Execute the given parser on the supplied input stream at the given start position, returning
-- either the position of an error and the error message, or the value parsed.
execParser :: P a -> InputStream -> Position -> Either (Position,String) a
execParser (P parser) input pos =
  case parser initialState of
    Failed message errPos -> Left (errPos, message)
    Ok result _ -> Right result
  where initialState = PState
          { curPos = pos
          , curInput = input
          , prevPos = error "ParseMonad.execParser: Touched undefined position!"
          , pushedTokens = []
          }

-- | Extract the state stored in the parser.
getPState :: P PState
getPState = P $ \s -> Ok s s

-- | Update the state stored in the parser.
setPState :: PState -> P ()
setPState s = P $ \_ -> Ok () s 

-- | Retrieve the current position of the parser.
getPosition :: P Position
getPosition = P $ \s@PState{ curPos = pos } -> Ok pos s

-- | Update the current position of the parser.
setPosition :: Position -> P ()
setPosition pos = P $ \s -> Ok () s{ curPos = pos }

-- | Retrieve the current 'InputStream' of the parser
getInput :: P InputStream
getInput = P $ \s@PState{ curInput = i } -> Ok i s

-- | Update the current 'InputStream' of the parser
setInput :: InputStream -> P ()
setInput i = P $ \s -> Ok () s{ curInput = i }

-- | Manually push a @'Spanned' 'Token'@. This turns out to be useful when parsing tokens that need
-- to be broken up. For example, when seeing a 'GreaterEqual' token but only expecting a 'Greater'
-- token, one can consume the 'GreaterEqual' token and push back a 'Equal' token.
pushToken :: Spanned Token -> P ()
pushToken tok = P $ \s@PState{ pushedTokens = toks } -> Ok () s{ pushedTokens = tok : toks }

-- | Manually pop a @'Spanned' 'Token'@ (if there are no tokens to pop, returns 'Nothing'). See
-- 'pushToken' for more details.
popToken :: P (Maybe (Spanned Token))
popToken = P $ \s@PState{ pushedTokens = toks } -> Ok (listToMaybe toks) s{ pushedTokens = drop 1 toks }

-- | Signal a syntax error.
parseError :: Show b => b -> P a
parseError b = fail ("Syntax error: the symbol `" ++ show b ++ "' does not fit here")


