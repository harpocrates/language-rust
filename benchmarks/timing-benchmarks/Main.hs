{-# LANGUAGE OverloadedStrings, TypeApplications #-}

import Criterion
import Criterion.Main (defaultConfig)
import Criterion.Types (anMean, reportAnalysis, timeLimit, anOutlierVar, ovEffect, OutlierEffect(Severe))
import Statistics.Resampling.Bootstrap (Estimate(..))

import Control.Monad (filterM)
import Control.Exception (catch, throwIO)
import Data.Traversable (for)
import GHC.Exts (fromString)

import Language.Rust.Syntax (SourceFile)
import Language.Rust.Parser (readInputStream, Span, parse')

import System.Directory (getCurrentDirectory, listDirectory, createDirectoryIfMissing, doesFileExist, removeFile)
import System.FilePath ((</>), (<.>), takeFileName)
import System.Process (proc, readCreateProcess)
import System.IO.Error (isDoesNotExistError)

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

  -- Clear out previous WIP (if there is one)
  catch (removeFile (workingDirectory </> "timings" </> "WIP" <.> "json"))
        (\e -> if isDoesNotExistError e then pure () else throwIO e)

  -- Run 'criterion' tests
  reports <- for files $ \f -> do
    let name = takeFileName f
    putStrLn name
    is <- readInputStream f
    bnch <- benchmarkWith' defaultConfig{ timeLimit = 15 } (nf (parse' @(SourceFile Span)) is)
    pure (name, bnch)
  let results = object [ fromString name .= object [ "mean" .= m
                                                   , "lower bound" .= l
                                                   , "upper bound" .= u
                                                   ]
                       | (name,report) <- reports
                       , let Estimate m l u _ = anMean (reportAnalysis report)
                       , ovEffect (anOutlierVar (reportAnalysis report)) /= Severe
                       ]

  -- Save the output to JSON
  createDirectoryIfMissing False (workingDirectory </> "timings")
  let logFile = workingDirectory </> "timings" </> logFileName <.> "json"
  putStrLn $ "writing results to: " ++ logFile
  logFile `BL.writeFile` encode results

