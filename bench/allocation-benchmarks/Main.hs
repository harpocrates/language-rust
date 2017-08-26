{-# LANGUAGE OverloadedStrings #-}

import Weigh

import Control.Monad (filterM)
import Control.Exception (catch, throwIO)
import Data.Foldable (for_)
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
import qualified Data.ByteString.Lazy as BL

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
  fileStreams <- for files $ \file -> do { is <- readInputStream file; pure (takeFileName file, is) }
  let weigh = do setColumns [ Case, Max, Allocated, GCs, Live ]
                 for_ fileStreams $ \(file,is) -> func file (parse' :: InputStream -> SourceFile Span) is
  mainWith weigh
  (wr, _) <- weighResults weigh
  let results = object [ case maybeErr of
                           Nothing -> key .= object [ "allocated" .= weightAllocatedBytes weight
--                                                    , "max"       .= weightMaxBytes w
--                                                    , "live"      .= weightLiveBytes w
--                                                    , "GCs"       .= weightGCs w
                                                    ]
                           Just err -> key .= String (fromString err)
                       | (weight, maybeErr) <- wr
                       , let key = fromString (weightLabel weight)
                       ]

  -- Save the output to JSON
  createDirectoryIfMissing False (workingDirectory </> "bench" </> "allocations")
  let logFile = workingDirectory </> "bench" </> "allocations" </> logFileName <.> "json"
  putStrLn $ "writing results to: " ++ logFile
  logFile `BL.writeFile` encode results

