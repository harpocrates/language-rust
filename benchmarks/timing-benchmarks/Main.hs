{-# LANGUAGE OverloadedStrings #-}

import Criterion
import Criterion.Types (anMean, reportAnalysis)
import Statistics.Resampling.Bootstrap (Estimate(..))

import Control.Monad (filterM)
import Data.Traversable (for)
import GHC.Exts (fromString)

import Language.Rust.Parser (parseSourceFile')

import System.Directory (getCurrentDirectory, listDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>), takeFileName)
import System.Process (proc, readCreateProcess)

import Data.Aeson
import qualified Data.ByteString.Lazy as BL

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

  -- Run 'criterion' tests
  reports <- for files $ \f -> do
    let name = takeFileName f
    putStrLn name
    bnch <- benchmark' (nfIO (parseSourceFile' f))
    pure (name, bnch)
  let results = object [ fromString name .= object [ "mean" .= m
                                                   , "lower bound" .= l
                                                   , "upper bound" .= u
                                                   ]
                       | (name,report) <- reports
                       , let Estimate m l u _ = anMean (reportAnalysis report)
                       ]

  -- Save the output to JSON
  createDirectoryIfMissing False (workingDirectory </> "timings")
  let logFile = workingDirectory </> "timings" </> logFileName <.> "json"
  logFile `BL.writeFile` encode results

