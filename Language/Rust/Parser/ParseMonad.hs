{-# LANGUAGE InstanceSigs #-}

module Language.Rust.Parser.ParseMonad where

import Language.Rust.Data.InputStream
import Language.Rust.Syntax.Token

import Control.Monad
import Control.Monad.Trans.Except


data Position = Position {
    absoluteOffset :: {-# UNPACK #-} !Int, -- ^ absolute offset the source file.
    row :: {-# UNPACK #-} !Int,            -- ^ row (line) in the source file.
    col :: {-# UNPACK #-} !Int             -- ^ column in the source file.
  } deriving (Show)

newtype ParseError = ParseError (String,Position) deriving (Show)

data ParseResult a
  = Ok a !PState
  | Failed String Position   -- The error message and position

data PState = PState {
    curPos     :: !Position,       -- position at current input location
    curInput   :: !InputStream,    -- the current input
    prevToken  ::  Token,          -- the previous token
    savedToken ::  Token           -- and the token before that
 }

newtype P a = P { runP :: PState -> ParseResult a }

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
                          Ok a s'        -> runP (k a) s'
                          Failed err pos -> Failed err pos

  fail :: String -> P a
  fail msg = do { pos <- getPos; P (\_ -> Failed msg pos) }

-- | execute the given parser on the supplied input stream.
--   returns 'ParseError' if the parser failed, and a pair of
--   result and remaining name supply otherwise
--
-- Synopsis: @execParser parser inputStream initialPos predefinedTypedefs uniqNameSupply@
execParser :: P a -> InputStream -> Position -> Except ParseError a
execParser (P parser) input pos =
  case parser initialState of
    Failed message errpos -> throwE (ParseError (message,errpos))
    Ok result st -> return result
  where initialState = PState {
          curPos = pos,
          curInput = input,
          prevToken = error "CLexer.execParser: Touched undefined token!",
          savedToken = error "CLexer.execParser: Touched undefined token (saved token)!"
        }

setPos :: Position -> P ()
setPos pos = P $ \s -> Ok () s{curPos=pos}

getPos :: P Position
getPos = P $ \s@PState{curPos=pos} -> Ok pos s

getInput :: P InputStream
getInput = P $ \s@PState{curInput=i} -> Ok i s

setInput :: InputStream -> P ()
setInput i = P $ \s -> Ok () s{curInput=i}

getLastToken :: P Token
getLastToken = P $ \s@PState{prevToken=tok} -> Ok tok s

getSavedToken :: P Token
getSavedToken = P $ \s@PState{savedToken=tok} -> Ok tok s

-- | @setLastToken modifyCache tok@
setLastToken :: Token -> P ()
setLastToken Eof = P $ \s -> Ok () s{savedToken=(prevToken s)}
setLastToken tok = P $ \s -> Ok () s{prevToken=tok,savedToken=(prevToken s)}

-- | handle an End-Of-File token (changes savedToken)
handleEofToken :: P ()
handleEofToken = P $ \s -> Ok () s{savedToken=(prevToken s)}

getCurrentPosition :: P Position
getCurrentPosition = P $ \s@PState{curPos=pos} -> Ok pos s