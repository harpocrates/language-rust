{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Diff

import Control.Monad (filterM)
import Control.Exception (catch, evaluate, SomeException)
import Control.Monad.Trans.Writer (execWriter, Writer)

import Data.ByteString.Lazy (hGetContents)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson (decode', Value)

import Language.Rust.Parser (parse', readInputStream, Span)
import Language.Rust.Syntax.AST (SourceFile)

import System.Directory (getCurrentDirectory, listDirectory, doesFileExist)
import System.Process (createProcess, proc, CreateProcess(..), StdStream(..))
import System.FilePath ((</>), takeFileName)

import Test.Framework (defaultMain)
import Test.Framework.Providers.API

main :: IO ()
main = do
  workingDirectory <- getCurrentDirectory
  let folder = workingDirectory </> "sample-sources"
  entries <- map (folder </>) <$> listDirectory folder
  files <- filterM doesFileExist entries
  defaultMain (map (\f -> Test (takeFileName f) (DiffTest f)) files)


-- | Given a path pointing to a rust source file, read that file and parse it into a 'SourceFile'
getSourceFile :: FilePath -> IO (SourceFile Span)
getSourceFile fileName = parse' <$> readInputStream fileName

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
    Nothing -> error (unpack jsonContents)


-- * Difference tests

-- | A 'DiffTest' only needs to know the name of the file it is diffing
data DiffTest = DiffTest String

-- | These are the possible pending statuses of a 'DiffTest'
data DiffRunning = RunningReference
                 | RunningImplementation
                 | Diffing

instance Show DiffRunning where
  show RunningReference = "Running reference implementation"
  show RunningImplementation = "Running our implementation"
  show Diffing = "Comparing the two"

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
-- TODO: make this nicer
instance Testlike DiffRunning DiffResult DiffTest where
  testTypeName _ = "Difference tests"
  runTest TestOptions{ topt_timeout = K timeout } (DiffTest file) = runImprovingIO $ do
    yieldImprovement RunningReference
    val'_me <- maybeTimeoutImprovingIO timeout $ liftIO (try' (getJsonAST file))
    case val'_me of
      Nothing -> pure (Error "Timed out running reference implementation")
      Just (Left e) -> pure (Error e)
      Just (Right val') -> do
        yieldImprovement RunningImplementation
        val_me <- maybeTimeoutImprovingIO timeout $ liftIO (try' (getSourceFile file))
        case val_me of
          Nothing -> pure (Error "Timed out running our implementation")
          Just (Left e) -> pure (Error e)
          Just (Right val) -> do
            yieldImprovement Diffing
            diff_m <- maybeTimeoutImprovingIO timeout $ liftIO (try' (diffSourceFile val val'))
            case diff_m of
              Nothing -> pure (Error "Timed out while finding differences")
              Just (Left e) -> pure (Error e)
              Just (Right _) -> pure Done

-- | Variant of 'try' which separates the error case by just returning 'Left msg' when there is an
-- exception.
try' :: IO a -> IO (Either String a)
try' io = catch (Right <$> io)
                (\e -> pure (Left (show (e :: SomeException))))

