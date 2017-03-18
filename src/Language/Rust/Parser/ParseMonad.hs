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

{-# LANGUAGE InstanceSigs, RankNTypes #-}

module Language.Rust.Parser.ParseMonad (
  -- * Parsing monad
  P, ParserT, execParser, execParserT, initPos, PState(..),
  -- * Monadic operations
  getPState, setPState, getPosition, setPosition, getInput, setInput, popToken, pushToken, swapToken,
  -- * Error reporting
  parseError,
) where

import Language.Rust.Data.InputStream (InputStream)
import Language.Rust.Data.Position (Spanned, Position, initPos)
import Language.Rust.Syntax.Token (Token)

import Data.Maybe (listToMaybe)
import Data.Functor.Identity (Identity(..))

-- | Parsing and lexing monad. A value of type @'P' a@ represents a parser that can be run (using
-- 'execParser') to possibly produce a value of type @a@.
type P a = ParserT Identity a

-- | Church encoding of a parser
newtype ParserT m a = P
  { unParser :: forall b. PState m                    -- ^ State
                       -> (a -> PState m -> m b)      -- ^ Parsing worked
                       -> (String -> Position -> m b) -- ^ Parsing failed (error message, position)
                       -> m b
  }

-- | State that the lexer and parser share
data PState m = PState
  { curPos       :: !Position          -- ^ position at current input location
  , curInput     :: !InputStream       -- ^ the current input
  , prevPos      ::  Position          -- ^ position at previous input location
  , pushedTokens :: [Spanned Token]    -- ^ tokens manually pushed by the user
  , swappingFunc :: Token -> m Token   -- ^ monadically swap out a function
  }

instance Monad m => Functor (ParserT m) where
  fmap :: (a -> b) -> ParserT m a -> ParserT m b
  fmap f p = P $ \s pOk pFailed -> unParser p s (pOk . f) pFailed

instance Monad m => Applicative (ParserT m) where
  pure :: a -> ParserT m a
  pure x = P $ \s pOk _ -> pOk x s

  (<*>) :: ParserT m (a -> b) -> ParserT m a -> ParserT m b
  m <*> k = P $ \s pOk pFailed ->
    let pOk' x s' = unParser k s' (pOk . x) pFailed
    in unParser m s pOk' pFailed

instance Monad m => Monad (ParserT m) where
  return :: a -> ParserT m a
  return = pure
  
  (>>=) :: ParserT m a -> (a -> ParserT m b) -> ParserT m b
  m >>= k = P $ \s pOk pFailed ->
    let pOk' x s' = unParser (k x) s' pOk pFailed
    in unParser m s pOk' pFailed


  fail :: String -> ParserT m a
  fail msg = P $ \s _ pFailed -> pFailed msg (curPos s)

-- | Execute the given parser on the supplied input stream at the given start position, returning
-- either the position of an error and the error message, or the value parsed.
execParser :: P a -> InputStream -> Position -> Either (Position,String) a
execParser parser input pos = runIdentity (execParserT parser input pos Identity)

execParserT :: Monad m => ParserT m a
                       -> InputStream
                       -> Position 
                       -> (Token -> m Token) 
                       -> m (Either (Position,String) a)
execParserT parser input pos func = unParser parser initialState pOk pFailed 
  where pOk x _ = pure (Right x) 
        pFailed m p = pure (Left (p,m))
        initialState = PState
          { curPos = pos
          , curInput = input
          , prevPos = error "ParseMonad.execParser: Touched undefined position!"
          , pushedTokens = []
          , swappingFunc = func
          }

-- | Extract the state stored in the parser.
getPState :: Monad m => ParserT m (PState m)
getPState = P $ \s pOk _ -> pOk s s

-- | Update the state stored in the parser.
setPState :: Monad m => PState m -> ParserT m ()
setPState s = P $ \_ pOk _ -> pOk () s 

-- | Modify the state stored in the parser.
modifyPState :: Monad m => (PState m -> PState m) -> ParserT m ()
modifyPState f = P $ \s pOk _ -> pOk () (f $! s) 

-- | Swap the last token
swapToken :: Monad m => Token -> ParserT m Token
swapToken t = P $ \s@PState{ swappingFunc = f } pOk _ -> f t >>= \t' -> pOk t' s

-- | Retrieve the current position of the parser.
getPosition :: Monad m => ParserT m Position
getPosition = curPos <$> getPState

-- | Update the current position of the parser.
setPosition :: Monad m => Position -> ParserT m ()
setPosition pos = modifyPState $ \s -> s{ curPos = pos }

-- | Retrieve the current 'InputStream' of the parser
getInput :: Monad m => ParserT m InputStream
getInput = curInput <$> getPState 

-- | Update the current 'InputStream' of the parser
setInput :: Monad m => InputStream -> ParserT m ()
setInput i = modifyPState $ \s -> s{ curInput = i }

-- | Manually push a @'Spanned' 'Token'@. This turns out to be useful when parsing tokens that need
-- to be broken up. For example, when seeing a 'GreaterEqual' token but only expecting a 'Greater'
-- token, one can consume the 'GreaterEqual' token and push back a 'Equal' token.
pushToken :: Monad m => Spanned Token -> ParserT m ()
pushToken tok = modifyPState $ \s@PState{ pushedTokens = toks } -> s{ pushedTokens = tok : toks }

-- | Manually pop a @'Spanned' 'Token'@ (if there are no tokens to pop, returns 'Nothing'). See
-- 'pushToken' for more details.
popToken :: Monad m => ParserT m (Maybe (Spanned Token))
popToken = P $ \s@PState{ pushedTokens = toks } pOk _ -> pOk (listToMaybe toks) s{ pushedTokens = drop 1 toks }

-- | Signal a syntax error.
parseError :: (Monad m, Show b) => b -> ParserT m a
parseError b = fail ("Syntax error: the symbol `" ++ show b ++ "' does not fit here")


