module Bin.Git where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Effect (Effect)

type ExecResult =
  { success :: Boolean
  , value :: String
  }

foreign import execSyncImpl :: String -> Effect ExecResult

-- | Execute a command synchronously and return the result
execSync :: String -> Effect (Either String String)
execSync cmd = do
  result <- execSyncImpl cmd
  pure
    if result.success then
      Right result.value
    else
      Left result.value

-- | Get a list of modified files (working tree changes)
getModifiedFiles :: Effect (Either String (Array String))
getModifiedFiles = do
  result <- execSync "git diff --name-only --diff-filter=ACMR"
  pure $ result <#> parseGitOutput

-- | Get a list of staged files (changes in index)
getStagedFiles :: Effect (Either String (Array String))
getStagedFiles = do
  result <- execSync "git diff --cached --name-only --diff-filter=ACMR"
  pure $ result <#> parseGitOutput

-- | Get a list of all dirty files (modified, staged, and untracked)
getDirtyFiles :: Effect (Either String (Array String))
getDirtyFiles = do
  result <- execSync "git status --short --porcelain"
  pure $ result <#> parseDirtyGitOutput

-- | Parse git status output into an array of file paths
parseDirtyGitOutput :: String -> Array String
parseDirtyGitOutput output = do
  let
    -- Extract file path from git status line
    -- (format: "XY filename" or "XY filename -> newname")
    extractFilePath :: String -> String
    extractFilePath line = do
      -- Skip the status code (first 3 characters: "XY ")
      let pathPart = String.drop 3 line

      -- Handle renames (take the part after " -> " if present)
      case String.indexOf (Pattern " -> ") pathPart of
        Just idx -> String.drop (idx + 4) pathPart
        Nothing -> pathPart

  output
    # String.trim
    # String.split (Pattern "\n")
    # Array.filter (not <<< String.null)
    # map extractFilePath
    # Array.filter (not <<< String.null)

-- | Parse git output into an array of file paths
parseGitOutput :: String -> Array String
parseGitOutput output =
  output
    # String.trim
    # String.split (Pattern "\n")
    # Array.filter (not <<< String.null)
