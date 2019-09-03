{-|
Module      : Language.Rust.Parser.ParseMonad
Description : Parsing monad for lexer/parser
Copyright   : (c) Alec Theriault, 2017-2018
License     : BSD-style
Maintainer  : alec.theriault@gmail.com
Stability   : experimental
Portability : GHC

Both the lexer and the parser run inside of the 'P' monad. As detailed in the section on 
on [threaded-lexers](https://www.haskell.org/happy/doc/html/sec-monads.html#sec-lexers) in Happy's
instruction manual, the benefits of this are that:

  * Lexical errors can be treated in the same way as parse errors
  * Information such as the current position in the file shared between the lexer and parser
  * General information can be passed back from the parser to the lexer too

In our case, this shared information is held in 'PState'.
-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.Rust.Parser.ParseMonad (
  -- * Parsing monad
  P,
  execParser,
  execParser',
  initPos,
  PState(..),

  -- * Monadic operations
  getPState,
  setPState,
  getPosition,
  setPosition,
  getInput,
  setInput,
  popToken,
  pushToken,
  swapToken,

  -- * Error reporting
  ParseFail(..),
  parseError,
) where

import Language.Rust.Data.InputStream  ( InputStream )
import Language.Rust.Data.Position     ( Spanned, Position, initPos, prettyPosition )
import Language.Rust.Syntax.Token      ( Token )

import Control.Monad.Fail as Fail
import Control.Exception               ( Exception )
import Data.Maybe                      ( listToMaybe )
import Data.Typeable                   ( Typeable )

-- | Parsing and lexing monad. A value of type @'P' a@ represents a parser that can be run (using
-- 'execParser') to possibly produce a value of type @a@.
newtype P a = P { unParser :: forall r. PState                       -- State being passed along
                                     -> (a -> PState -> r)           -- Successful parse continuation
                                     -> (String -> Position -> r)    -- Failed parse continuation
                                     -> r                            -- Final output
                }

-- | State that the lexer and parser share
data PState = PState
  { curPos       :: !Position        -- ^ position at current input location
  , curInput     :: !InputStream     -- ^ the current input
  , prevPos      ::  Position        -- ^ position at previous input location
  , pushedTokens :: [Spanned Token]  -- ^ tokens manually pushed by the user
  , swapFunction :: Token -> Token   -- ^ function to swap token
  }

instance Functor P where
  fmap f m = P $ \ !s pOk pFailed -> unParser m s (pOk . f) pFailed

instance Applicative P where
  pure x = P $ \ !s pOk _ -> pOk x s

  m <*> k = P $ \ !s pOk pFailed ->
    let pOk' x s' = unParser k s' (pOk . x) pFailed
    in unParser m s pOk' pFailed

instance Monad P where
  return = pure

  m >>= k = P $ \ !s pOk pFailed ->
    let pOk' x s' = unParser (k x) s' pOk pFailed
    in unParser m s pOk' pFailed

instance Fail.MonadFail P where
  fail msg = P $ \ !s _ pFailed -> pFailed msg (curPos s)

-- | Exceptions that occur during parsing
data ParseFail = ParseFail Position String deriving (Eq, Typeable)

instance Show ParseFail where
  showsPrec p (ParseFail pos msg) = showParen (p >= 11) (showString err)
    where err = unwords [ "parse failure at", prettyPosition pos, "(" ++ msg ++ ")" ]

instance Exception ParseFail

-- | Execute the given parser on the supplied input stream at the given start position, returning
-- either the position of an error and the error message, or the value parsed.
execParser :: P a -> InputStream -> Position -> Either ParseFail a
execParser p input pos = execParser' p input pos id

-- | Generalized version of 'execParser' that expects an extra argument that lets you hot-swap a
-- token that was just lexed before it gets passed to the parser.
execParser' :: P a -> InputStream -> Position -> (Token -> Token) -> Either ParseFail a
execParser' parser input pos swap = unParser parser
                                     initialState
                                     (\result _ -> Right result)
                                     (\message errPos -> Left (ParseFail errPos message))
  where initialState = PState
          { curPos = pos
          , curInput = input
          , prevPos = error "ParseMonad.execParser: Touched undefined position!"
          , pushedTokens = []
          , swapFunction = swap
          }

-- | Swap a token using the swap function.
swapToken :: Token -> P Token
swapToken t = P $ \ !s@PState{ swapFunction = f } pOk _ -> pOk (f $! t) s

-- | Extract the state stored in the parser.
getPState :: P PState
getPState = P $ \ !s pOk _ -> pOk s s 

-- | Update the state stored in the parser.
setPState :: PState -> P ()
setPState s = P $ \ _ pOk _ -> pOk () s 

-- | Modify the state stored in the parser.
modifyPState :: (PState -> PState) -> P ()
modifyPState f = P $ \ !s pOk _ -> pOk () (f $! s) 

-- | Retrieve the current position of the parser.
getPosition :: P Position
getPosition = curPos <$> getPState

-- | Update the current position of the parser.
setPosition :: Position -> P ()
setPosition pos = modifyPState $ \ s -> s{ curPos = pos }

-- | Retrieve the current 'InputStream' of the parser.
getInput :: P InputStream
getInput = curInput <$> getPState 

-- | Update the current 'InputStream' of the parser.
setInput :: InputStream -> P ()
setInput i = modifyPState $ \s -> s{ curInput = i }

-- | Manually push a @'Spanned' 'Token'@. This turns out to be useful when parsing tokens that need
-- to be broken up. For example, when seeing a 'Language.Rust.Syntax.GreaterEqual' token but only
-- expecting a 'Language.Rust.Syntax.Greater' token, one can consume the
-- 'Language.Rust.Syntax.GreaterEqual' token and push back an 'Language.Rust.Syntax.Equal' token.
pushToken :: Spanned Token -> P ()
pushToken tok = modifyPState $ \s@PState{ pushedTokens = toks } -> s{ pushedTokens = tok : toks }

-- | Manually pop a @'Spanned' 'Token'@ (if there are no tokens to pop, returns 'Nothing'). See
-- 'pushToken' for more details.
popToken :: P (Maybe (Spanned Token))
popToken = P $ \ !s@PState{ pushedTokens = toks } pOk _ -> pOk (listToMaybe toks) s{ pushedTokens = drop 1 toks }

-- | Signal a syntax error.
parseError :: Show b => b -> P a
parseError b = Fail.fail ("Syntax error: the symbol `" ++ show b ++ "' does not fit here")

