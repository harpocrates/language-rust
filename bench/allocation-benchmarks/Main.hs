{-# LANGUAGE OverloadedStrings #-}

import Weigh

import Control.DeepSeq (rnf)
import Control.Monad (filterM)
import Control.Exception (catch, throwIO, evaluate)
import Data.Traversable (for)
import GHC.Exts (fromString)

import Language.Rust.Data.InputStream (InputStream)
import Language.Rust.Syntax (SourceFile)
import Language.Rust.Parser (readInputStream, Span, parse')

import System.Directory (getCurrentDirectory, listDirectory, createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>), (<.>), takeFileName)
import System.Process (proc, readCreateProcess)
import System.IO.Error (isDoesNotExistError)

import Data.Aeson

-- TODO: only allocation and GCs seem to be really reproducible. Live and max sometimes are 0.

main :: IO ()
main = do
  -- Open the output log file
  status <- readCreateProcess (proc "git" ["status", "--porcelain"]) ""
  logFileName <- case status of
                   "" -> init <$> readCreateProcess (proc "git" ["rev-parse", "HEAD"]) ""
                   _ -> pure "WIP"

  -- Get the test cases
  workingDirectory <- getCurrentDirectory
  let sampleSources = workingDirectory </> "sample-sources"
      benchIgnore = sampleSources </> ".benchignore"
  benchIgnoreExists <- doesFileExist benchIgnore
  ignore <- if benchIgnoreExists
              then (\f -> map (sampleSources </>) (lines f)) <$> readFile benchIgnore
              else pure []
  entries <- map (sampleSources </>) <$> listDirectory sampleSources
  files <- filterM doesFileExist (filter (`notElem` ignore) entries)

  -- Clear out previous WIP (if there is one)
  catch (removeFile (workingDirectory </> "bench" </> "allocations" </> "WIP" <.> "json"))
        (\e -> if isDoesNotExistError e then pure () else throwIO e)

  -- Run 'weigh' tests
  weighResultsJson <- fmap object . for files $ \file -> do
    is <- readInputStream file
    evaluate (rnf is)
    let testName = fromString (takeFileName file)
    (allocatedBytes, _gcs, _liveBytes, _maxBytes) <-
      weighFunc (parse' :: InputStream -> SourceFile Span) is
    pure $ testName .= object [ "allocated" .= allocatedBytes
--                               , "max"       .= maxBytes
--                               , "live"      .= liveBytes
--                               , "GCs"       .= gcs
                              ]

  -- Save the output to JSON
  createDirectoryIfMissing False (workingDirectory </> "bench" </> "allocations")
  let logFile = workingDirectory </> "bench" </> "allocations" </> logFileName <.> "json"
  putStrLn $ "writing results to: " ++ logFile
  encodeFile logFile weighResultsJson

