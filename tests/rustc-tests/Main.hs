module Main where

import Diff

import Language.Rust.Parser
import Language.Rust.Syntax.AST (SourceFile)

import System.Directory
import System.Process
import Data.ByteString.Lazy (hGetContents)
import Data.Aeson
import Control.Monad (filterM)
import Data.Foldable (for_)

import Control.Monad.Trans.Writer

main :: IO ()
main = do
  let folder = "/Users/atheriault/Scratch/language-rust/sample-sources/"
  entries <- map (folder ++) <$> listDirectory folder
  files <- filterM doesFileExist entries
  putStrLn $ "\nTesting files: " ++ unwords entries

  for_ files $ \file -> do
    val <- getSourceFile file
    val' <- getJsonAST file
    case execWriter (diffSourceFile val val') of
      [] -> putStrLn $ "No differences found in file `" ++ file ++ "'."
      diffs -> putStrLn $ "Differences found in file `" ++ file ++ "':\n" ++ unlines diffs

-- | Given a path pointing to a rust source file, read that file and parse it into a 'SourceFile'
getSourceFile :: FilePath -> IO (SourceFile Span)
getSourceFile fileName = do
  inp <- readInputStream fileName
  let Right sourceFile = parse inp
  pure sourceFile

-- | Given a path pointing to a rust source file, read that file and parse it into JSON
getJsonAST :: FilePath -> IO Value
getJsonAST fileName = do
  (_, Just hOut, _, _) <- createProcess (proc "rustc" ["-Z", "ast-json", fileName]){ std_out = CreatePipe }
  jsonContents <- hGetContents hOut
  let Just value = decode' jsonContents
  pure value

