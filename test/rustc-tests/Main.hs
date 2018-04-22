{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import Diff ()
import DiffUtils

import Control.Monad (filterM, when)
import Control.Exception (catch, SomeException, evaluate)
import Data.Typeable (Typeable)

import Data.ByteString.Lazy (hGetContents)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Aeson (decode', Value)

import Language.Rust.Parser (readSourceFile)
import Language.Rust.Pretty (prettyUnresolved, Resolve(..), Issue(..), Severity(Clean))
import Language.Rust.Syntax (SourceFile)

import System.Directory (getCurrentDirectory, getTemporaryDirectory, listDirectory, doesFileExist, findExecutable)
import System.Process (withCreateProcess, proc, CreateProcess(..), StdStream(..), callProcess, readProcess)
import System.FilePath ((</>), takeFileName)
import System.IO (withFile, IOMode(WriteMode,ReadMode))
import System.Exit (exitSuccess)

import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (fromGregorian, showGregorian, diffDays)

import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Render.Text (renderIO)

import Test.Framework (defaultMain)
import Test.Framework.Providers.API

main :: IO ()
main = do
  -- Check last time `rustc` version was bumped
  let lastDay = fromGregorian 2018 4 19
  today <- utctDay <$> getCurrentTime
  when (diffDays today lastDay > 32) $
    putStrLn $ "\x1b[33m" ++ "\nThe version of `rustc' the tests will try to use is older than 1 month" ++ "\x1b[0m"

  -- Don't bother running the tests if you don't have `rustup` or `rustc` installed.
  missingProgs <- any null <$> traverse findExecutable ["rustup","rustc"]
  when missingProgs $ do 
    putStrLn $ "Could not find `rustup`/`rustc`, so skipping these tests"
    exitSuccess

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
  withCreateProcess cp $ \_ (Just hOut) _ _ -> do
    jsonContents <- hGetContents hOut
    case decode' jsonContents of
      Just value -> pure value
      Nothing -> error ("Failed to get `rustc' JSON\n" ++ unpack jsonContents)

-- | Given an AST and a file name, print it into a temporary file (without resolving) and return
-- that path
prettySourceFile :: FilePath -> SourceFile a -> IO FilePath
prettySourceFile path ast = do
  tmp <- getTemporaryDirectory
  let path' = tmp </> takeFileName path
      opts = PP.LayoutOptions (PP.AvailablePerLine 100 1.0)
  withFile path' WriteMode (\hdl -> renderIO hdl (PP.layoutPretty opts (prettyUnresolved ast)))
  pure path'

resolveDiff :: (Monoid a, Typeable a) => SourceFile a -> IO ()
resolveDiff ast = when (sev /= Clean) $
                    error ("Resolve thinks there is (are) some " ++ show sev ++ "\n" ++ msgs)
  where (_, sev, iss) = resolveVerbose ast
        msgs = unlines [ "  " ++ show sev' ++ " " ++ desc | Issue desc sev' _ <- iss ]


-- * Difference tests

-- | A 'DiffTest' only needs to know the name of the file it is diffing
data DiffTest = DiffTest String

-- | These are the possible pending statuses of a 'DiffTest'
data DiffRunning = ParsingReference
                 | ParsingImplementation
                 | ParsingDiffing
                 | PrintingParsed
                 | ReparsingReference
                 | ReparsingDiffing
                 | ResolveInvariant


instance Show DiffRunning where
  show ParsingReference = "Parsing using `rustc'"
  show ParsingImplementation = "Parsing using our parser"
  show ParsingDiffing = "Comparing the two parsed outputs"
  show PrintingParsed = "Pretty printing the parsed syntax tree"
  show ReparsingReference = "Reparsing using `rustc'"
  show ReparsingDiffing = "Comparing to the reparsed output"
  show ResolveInvariant = "Checking that the parsed output is unchanged by `resolve'"

-- | These are the possible final states of a 'DiffTest'
data DiffResult = Error DiffRunning String
                | Done

instance Show DiffResult where
  show (Error improvement message) = "ERROR (" ++ show improvement ++ "): " ++ message
  show Done = "OK"

-- | A test is successful if it finishes and has no diffs
instance TestResultlike DiffRunning DiffResult where
  testSucceeded Done = True
  testSucceeded (Error _ _) = False

-- | With timeouts and catching errors
instance Testlike DiffRunning DiffResult DiffTest where
  testTypeName _ = "Difference tests"

  runTest TestOptions{ topt_timeout = K timeout } (DiffTest file) = runImprovingIO $
    step timeout ParsingReference (getJsonAST file) $ \parsedRustc ->
      step timeout ParsingImplementation (evaluate =<< withFile file ReadMode readSourceFile) $ \parsedOurs ->
        step timeout ParsingDiffing (parsedOurs === parsedRustc) $ \_ ->
          step timeout PrintingParsed (prettySourceFile file parsedOurs) $ \tmpFile ->
            step timeout ReparsingReference (getJsonAST tmpFile) $ \reparsedRustc ->
              step timeout ReparsingDiffing (parsedOurs === reparsedRustc) $ \_ ->
                step timeout ResolveInvariant (resolveDiff parsedOurs) $ \_ ->
                  pure Done


step :: Maybe Int                                              -- ^ timeout for the step
     -> DiffRunning                                            -- ^ improvement for the step
     -> IO a                                                   -- ^ content of the step
     -> (a -> ImprovingIO DiffRunning DiffResult DiffResult)   -- ^ continuation to run afterwards
     -> ImprovingIO DiffRunning DiffResult DiffResult
step timeout improvement action continuation = do
  yieldImprovement improvement
  val_me <- maybeTimeoutImprovingIO timeout $ liftIO (try' action)
  case val_me of
     Nothing -> pure (Error improvement "Timed out")
     Just (Left e) -> pure (Error improvement e)
     Just (Right val) -> continuation val
  

                 

-- | Variant of 'try' which separates the error case by just returning 'Left msg' when there is an
-- exception.
try' :: IO a -> IO (Either String a)
try' io = catch (Right <$> io)
                (\e -> pure (Left (show (e :: SomeException))))

