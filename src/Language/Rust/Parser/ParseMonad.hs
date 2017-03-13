{-# LANGUAGE InstanceSigs, PatternSynonyms #-}

module Language.Rust.Parser.ParseMonad where

import Language.Rust.Data.InputStream
import Language.Rust.Data.Position
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident

import Data.Maybe (listToMaybe)
import Control.Monad
import Control.Monad.Trans.Except

-- | Pattern for Identifiers
pattern Identifier :: String -> Spanned Token
pattern Identifier s <- (Spanned (IdentTok (Ident s _)) _)

-- | the result of running a parser
data ParseResult a
  = Ok a !PState             -- ^ successful output
  | Failed String Position   -- ^ the error message and position

-- | state that the parser passes along
data PState = PState {
    curPos       :: !Position,             -- position at current input location
    curInput     :: !InputStream,          -- the current input
    prevPos      ::  Position,             -- position at previous input location
    pushedTokens :: [Spanned Token]        -- possible user-pushed tokens
 }

-- | parsing monad
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
  fail msg = do { pos <- getPosition; P (\_ -> Failed msg pos) }

-- | execute the given parser on the supplied input stream.
--   returns 'ParseError' if the parser failed, and a pair of
--   result and remaining name supply otherwise
--
-- Synopsis: @execParser parser inputStream initialPos predefinedTypedefs uniqNameSupply@
execParser :: P a -> InputStream -> Position -> Except (Position,String) a
execParser (P parser) input pos =
  case parser initialState of
    Failed message errpos -> throwE (errpos,message)
    Ok result _ -> return result
  where initialState = PState
          { curPos = pos
          , curInput = input
          , prevPos = error "ParseMonad.execParser: Touched undefined position!"
          , pushedTokens = []
          }

-- | manually push on a token
pushToken :: Spanned Token -> P ()
pushToken tok = P $ \s@PState{ pushedTokens = toks } -> Ok () s{ pushedTokens = tok : toks }

-- | pop a token (if there is one to pop, otherwise returns Nothing) 
popToken :: P (Maybe (Spanned Token))
popToken = P $ \s@PState{ pushedTokens = toks } -> Ok (listToMaybe toks) s{ pushedTokens = drop 1 toks }

-- | retrieve the position of the parser
getPosition :: P Position
getPosition = P (\s@PState{ curPos = pos } -> Ok pos s)

-- | retrieve the position of the parser
setPosition :: Position -> P ()
setPosition pos = P (\s -> Ok () s{ curPos = pos })

-- | retrieve the input stream of the parser
getInput :: P InputStream
getInput = P (\s@PState{ curInput = i } -> Ok i s)

-- | set the input stream of the parser
setInput :: InputStream -> P ()
setInput i = P (\s -> Ok () s{ curInput = i })

-- | go back to previous position
popPosition :: P ()
popPosition = P $ \s@PState{ prevPos = p } ->
  Ok () s{ curPos = p
         , prevPos = error "ParseMonad.popPosition: Touched undefined position!"
         }
