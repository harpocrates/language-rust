module Main where

import System.IO

import LexerTest (lexerSuite)
import ParserTest (parserSuite)
import PrettyTest (prettySuite)
import CompleteTest (completeSuite)

import Test.Framework (defaultMain)

main :: IO ()
main = hSetEncoding stdout utf8 *> defaultMain [ lexerSuite, parserSuite, prettySuite, completeSuite ]
