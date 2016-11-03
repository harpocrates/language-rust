module Main where

import LexerTest (lexerSuite)

import Test.Framework (Test, defaultMain)

main :: IO ()
main = defaultMain [ lexerSuite ]