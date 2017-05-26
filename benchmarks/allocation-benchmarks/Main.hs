{-# LANGUAGE OverloadedStrings #-}

import Weigh

import Control.Monad (filterM)
import Data.Foldable (traverse_)
import GHC.Exts (fromString)

import Language.Rust.Parser (parseSourceFile')

import System.Directory (getCurrentDirectory, listDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>), takeFileName)
import System.Process (proc, readCreateProcess)

import Data.Aeson
import qualified Data.ByteString.Lazy as BL

-- TODO:
-- Only allocation and GCs seem to be really reproducible. Live and max sometimes are 0.

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
  entries <- map (sampleSources </>) <$> listDirectory sampleSources
  files <- filterM doesFileExist entries

  -- Run 'weigh' tests
  let weigh = setColumns [ Case, Max, Allocated, GCs, Live ] >> traverse_ (\f -> io (takeFileName f) parseSourceFile' f) files
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
  createDirectoryIfMissing False (workingDirectory </> "allocations")
  let logFile = workingDirectory </> "allocations" </> logFileName <.> "json"
  logFile `BL.writeFile` encode results

