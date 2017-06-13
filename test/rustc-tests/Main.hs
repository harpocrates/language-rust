{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Diff ()
import DiffUtils

import Control.Monad (filterM, when)
import Control.Exception (catch, SomeException)

import Data.ByteString.Lazy (hGetContents)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson (decode', Value)

import Language.Rust.Parser (readSourceFile)

import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..), callProcess, readProcess)
import System.FilePath ((</>), takeFileName)

import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (fromGregorian, showGregorian, diffDays)

import Test.Framework (defaultMain)
import Test.Framework.Providers.API

main :: IO ()
main = do
  -- Check last time `rustc` version was bumped
  let lastDay = fromGregorian 2017 6 13
  today <- utctDay <$> getCurrentTime
  when (diffDays today lastDay > 32) $
    putStrLn $ "\x1b[33m" ++ "\nThe version of `rustc' the tests will try to use is older than 1 month" ++ "\x1b[0m"

  -- Setting `rustc` version to the right nightly, just in this directory
  callProcess "rustup" ["override", "set", "nightly-" ++ showGregorian lastDay]
  version <- readProcess "rustc" ["--version"] ""
  putStrLn $ "\x1b[32m" ++ "Running tests with " ++ version ++ "\x1b[0m"

  -- Run the tests
  workingDirectory <- getCurrentDirectory
  let folder = workingDirectory </> "sample-sources"
  entries <- map (folder </>) <$> listDirectory folder
  files <- filterM doesFileExist (filter (/= folder </> ".benchignore") entries)
  defaultMain (map (\f -> Test (takeFileName f) (DiffTest f)) files)

-- | Given a path pointing to a rust source file, read that file and parse it into JSON
getJsonAST :: FilePath -> IO Value
getJsonAST fileName = do
  let cp = (proc "rustc" [ "-Z", "ast-json-noexpand"
                         , "-Z", "no-analysis"
                         , fileName ]){ std_out = CreatePipe
                                      , std_err = NoStream
                                      , std_in  = NoStream
                                      }
  (_, Just hOut, _, _) <- createProcess cp
  jsonContents <- hGetContents hOut
  case decode' jsonContents of
    Just value -> pure value
    Nothing -> error ("Failed to get `rustc' JSON\n" ++ unpack jsonContents)


-- * Difference tests

-- | A 'DiffTest' only needs to know the name of the file it is diffing
data DiffTest = DiffTest String

-- | These are the possible pending statuses of a 'DiffTest'
data DiffRunning = ParsingReference
                 | ParsingImplementation
                 | ParsingDiffing

instance Show DiffRunning where
  show ParsingReference = "Parsing using `rustc'"
  show ParsingImplementation = "Parsing using our parser"
  show ParsingDiffing = "Comparing the two parsed outputs"

-- | These are the possible final states of a 'DiffTest'
data DiffResult = Error String
                | Done

instance Show DiffResult where
  show (Error message) = "ERROR: " ++ message
  show Done = "OK"

-- | A test is successful if it finishes and has no diffs
instance TestResultlike DiffRunning DiffResult where
  testSucceeded Done = True
  testSucceeded (Error _) = False

-- | With timeouts and catching errors
instance Testlike DiffRunning DiffResult DiffTest where
  testTypeName _ = "Difference tests"
  runTest TestOptions{ topt_timeout = K timeout } (DiffTest file) = runImprovingIO $ do
    yieldImprovement ParsingReference
    val'_me <- maybeTimeoutImprovingIO timeout $ liftIO (try' (getJsonAST file))
    case val'_me of
      Nothing -> pure (Error "Timed out parsing using reference implementation")
      Just (Left e) -> pure (Error e)
      Just (Right val') -> do
        yieldImprovement ParsingImplementation
        val_me <- maybeTimeoutImprovingIO timeout $ liftIO (try' (readSourceFile file))
        case val_me of
          Nothing -> pure (Error "Timed out parsing using our implementation")
          Just (Left e) -> pure (Error e)
          Just (Right val) -> do
            yieldImprovement ParsingDiffing
            diff_m <- maybeTimeoutImprovingIO timeout $ liftIO (try' (val === val'))
            case diff_m of
              Nothing -> pure (Error "Timed out while finding differences")
              Just (Left e) -> pure (Error e)
              Just (Right _) -> pure Done
                        

                 

-- | Variant of 'try' which separates the error case by just returning 'Left msg' when there is an
-- exception.
try' :: IO a -> IO (Either String a)
try' io = catch (Right <$> io)
                (\e -> pure (Left (show (e :: SomeException))))

