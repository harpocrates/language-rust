{-# LANGUAGE InstanceSigs, PatternSynonyms #-}

module Language.Rust.Parser.ParseMonad where

import Language.Rust.Data.InputStream
import Language.Rust.Data.Position
import Language.Rust.Syntax.Token
import Language.Rust.Syntax.Ident

import Control.Monad
import Control.Monad.Trans.Except

-- | Pattern for Identifiers
pattern Identifier :: String -> Spanned Token
pattern Identifier s <- (Spanned (IdentTok (Ident (Name s) _)) _)

-- | Pattern for tokens preceded by space
pattern SpTok :: Spanned Token -> TokenSpace Spanned
pattern SpTok s <- (TokenSpace s (_:_))

-- | Pattern for tokens not preceded by space
pattern NoSpTok :: Spanned Token -> TokenSpace Spanned
pattern NoSpTok s <- (TokenSpace s [])

-- | Pattern for tokens (preceded or not by space)
pattern Tok :: Spanned Token -> TokenSpace Spanned
pattern Tok s <- (TokenSpace s _)

-- | the result of running a parser
data ParseResult a
  = Ok a !PState             -- ^ successful output
  | Failed String Position   -- ^ the error message and position

-- | state that the parser passes along
data PState = PState {
    curPos     :: !Position,       -- position at current input location
    curInput   :: !InputStream,    -- the current input
    prevToken  ::  Token,          -- the previous token
    savedToken ::  Token           -- and the token before that
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
          , prevToken = error "ParseMonad.execParser: Touched undefined token!"
          , savedToken = error "ParseMonad.execParser: Touched undefined token (saved token)!"
          }

-- | update the position of the parser
updatePosition :: (Position -> Position) -> P ()
updatePosition update = P (\s@PState{ curPos = pos } -> Ok () s{ curPos = update pos })

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

-- | get the previous token
getLastToken :: P Token
getLastToken = P (\s@PState{ prevToken = tok } -> Ok tok s)

-- | get the previous, previous token
getSavedToken :: P Token
getSavedToken = P (\s@PState{ savedToken = tok} -> Ok tok s)

-- | update the last token
setLastToken :: Token -> P ()
setLastToken Eof = P (\s -> Ok () s{ savedToken = prevToken s })
setLastToken tok = P (\s -> Ok () s{ prevToken = tok, savedToken = prevToken s })

-- | handle an End-Of-File token (changes savedToken)
handleEofToken :: P ()
handleEofToken = P (\s -> Ok () s{ savedToken = prevToken s })
