{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Rust.QuasiQuote (
  expr 
) where

import Language.Rust.Parser.ParseMonad
import Language.Rust.Parser.Internal
import Language.Rust.Data.InputStream (inputStreamFromString)
import Language.Rust.Data.Position (Position(..))

import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(..), dataToExpQ, dataToPatQ)

import Data.Data (Data)

expr :: QuasiQuoter
expr = QuasiQuoter { quoteExp = expr' 
                   , quotePat = notExprFail
                   , quoteType = noExprFail
                   , quoteDec = notExprFail
                   }
  where
  notExprFail :: String -> Q a
  notExprFail _ = fail "This quasiquoter only works for expressions"

  exprParser 

  expr' :: String -> Q Exp
  expr' s =
    case parsePartial (inputStreamFromString s) of
      Left (pos,msg) -> fail $ "Could not parse the type in the quasiquote 
    let inp = inputStreamFromString s
        ty_either = parsePartial inp
    in 



