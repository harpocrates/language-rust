module Options ( getOptions, RustcTestsConfig(..) ) where

import Prelude hiding ( fail )

import System.Console.GetOpt
import System.Environment     ( getArgs, getProgName )
import Control.Monad.Fail     ( fail )
import Data.Either            ( partitionEithers )

import Test.Framework.Runners.Console
import Test.Framework.Runners.Options

-- | Configuration settings for `rustc-tests`
data RustcTestsConfig = RTS
  { sourceFolder :: FilePath
  , pruneTests :: Bool
  }

-- | Base configuration
defaultConfig :: RustcTestsConfig
defaultConfig = RTS
  { sourceFolder = "sample-sources"
  , pruneTests = False
  }

type RustcTestsOptions = RustcTestsConfig -> RustcTestsConfig

-- | Command-line options for modifying the config
options :: [OptDescr RustcTestsOptions]
options = [ Option [] ["rust-sources"]
                   (ReqArg (\f r -> r { sourceFolder = f }) "FOLDER")
                   "Folder containing rust file test cases"
          , Option [] ["prune-failing-rustc"]
                   (NoArg (\r -> r { pruneTests = True }))
                   "Remove test cases which `rustc` can't initially parse"
          ]

fullOptions :: [OptDescr (Either RustcTestsOptions SuppliedRunnerOptions)]
fullOptions = map (fmap Left) options ++ map (fmap Right) optionsDescription

-- | Parse out options from the command line arguments
getOptions :: IO (RustcTestsConfig, RunnerOptions)
getOptions = do
  args <- getArgs
  prog <- getProgName
  let header = "Usage: " ++ prog ++ " [OPTION...]"
  case getOpt Permute fullOptions args of
    (o, [], []) -> let (ourOpts, optTestFrameworkOpts) = partitionEithers o
                       testFrameworkOpts = sequence optTestFrameworkOpts
                   in case testFrameworkOpts of
                        Nothing -> fail (usageInfo header fullOptions)
                        Just theirOpts -> pure ( foldl (flip id) defaultConfig ourOpts
                                               , mconcat theirOpts )
    (_, _, errs) -> fail (concat errs ++ usageInfo header fullOptions)
