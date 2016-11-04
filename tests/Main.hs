module Main where

import System.IO

import LexerTest (lexerSuite)

import Test.Framework (Test, defaultMain)

main :: IO ()
main = hSetEncoding stdout utf8 *> defaultMain [ lexerSuite ]