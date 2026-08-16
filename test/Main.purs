module Test.Main where

import Prelude

import Ansi.Output (foreground, withGraphics)
import ArgParse.Basic as Arg
import Bin.Arguments as Arguments
import Bin.FormatOptions as Bin
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (any, findMap, fold, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Dodo.Ansi (Color(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import Node.Process as Process
import Pursfmt (SingleLineLetInOption(..))
import Test.Snapshot (SnapshotResult(..), SnapshotResultGroup(..), snapshotFormat)

main :: Effect Unit
main = do
  args <- Process.argv
  let accept = any (eq "--accept" || eq "-a") args
  let printOutput = any (eq "--print-output" || eq "-p") args
  let filter = Pattern <$> findMap (String.stripPrefix (Pattern "--filter=")) args
  let printResult = printResultGroup printOutput
  launchAff_ do
    liftEffect runFormatOptionTests
    results@(SnapshotResultGroup { hasBad }) <- snapshotFormat accept filter
    printResult 0 results
    when hasBad do
      liftEffect $ Process.setExitCode 1
  where
  indent :: Int -> String
  indent = fold <<< flip Array.replicate "  "

  printResultGroup :: Boolean -> Int -> SnapshotResultGroup -> Aff Unit
  printResultGroup printOutput lvl (SnapshotResultGroup { results, nested }) = do
    let
      logIndented :: Int -> String -> Aff Unit
      logIndented lvl' = Console.log <<< String.joinWith "\n" <<< map (append (indent lvl')) <<< String.split (Pattern "\n")

    for_ results \{ name, results: outputResults } -> do
      logIndented lvl $ "Checking " <> name
      for_ outputResults \{ output, result, directive } -> case result of
        Passed -> do
          logIndented (lvl + 1) $ withGraphics (foreground Green) "✓" <> " " <> directive <> " passed."
          when printOutput $ logIndented (lvl + 2) output
        Saved -> do
          logIndented (lvl + 1) $ withGraphics (foreground Yellow) "✓" <> " " <> directive <> " saved."
          when printOutput $ logIndented (lvl + 2) output
        Accepted -> do
          logIndented (lvl + 1) $ withGraphics (foreground Yellow) "✓" <> " " <> directive <> " accepted."
          when printOutput $ logIndented (lvl + 2) output
        Failed diff -> do
          logIndented (lvl + 1) $ withGraphics (foreground Red) "✗" <> " " <> directive <> " failed."
          logIndented (lvl + 2) diff
          when printOutput $ logIndented (lvl + 2) output
        ErrorRunningTest err -> do
          logIndented (lvl + 1) $ withGraphics (foreground Red) "✗" <> " " <> directive <> " failed due to an error."
          logIndented (lvl + 2) $ Error.message err

    for_ (Map.toUnfoldable nested :: Array (Tuple String SnapshotResultGroup)) \(Tuple path group) -> do
      logIndented lvl "================="
      logIndented lvl (correctCase path)
      printResultGroup printOutput (lvl + 1) group

  correctCase :: String -> String
  correctCase =
    String.split (Pattern "-")
      >>> map upperFirstChar
      >>> String.joinWith " "

  upperFirstChar :: String -> String
  upperFirstChar s = case Array.uncons (SCU.toCharArray s) of
    Nothing -> ""
    Just { head, tail } -> String.toUpper (SCU.fromCharArray (Array.singleton head)) <> SCU.fromCharArray tail

runFormatOptionTests :: Effect Unit
runFormatOptionTests = do
  Console.log "Checking FormatOptions"
  assertEqual
    "legacy no-value --single-line-let-in before positional arg"
    [ "--single-line-let-in", "always", "src" ]
    (Arguments.normalizeLegacyArgs [ "--single-line-let-in", "src" ])
  assertEqual
    "legacy no-value -slli before positional arg"
    [ "-slli", "always", "src" ]
    (Arguments.normalizeLegacyArgs [ "-slli", "src" ])
  assertEqual
    "legacy no-value --single-line-let-in before another flag"
    [ "--single-line-let-in", "always", "--width", "80" ]
    (Arguments.normalizeLegacyArgs [ "--single-line-let-in", "--width", "80" ])
  assertEqual
    "explicit --single-line-let-in smart"
    [ "--single-line-let-in", "smart" ]
    (Arguments.normalizeLegacyArgs [ "--single-line-let-in", "smart" ])
  assertParsedMode "legacy no-value long flag" [ "--single-line-let-in" ] SingleLineLetInAlways
  assertParsedMode "legacy no-value short flag" [ "-slli" ] SingleLineLetInAlways
  assertParsedMode "explicit smart argument" [ "--single-line-let-in", "smart" ] SingleLineLetInSmart
  assertParsedMode "explicit smart short argument" [ "-slli", "smart" ] SingleLineLetInSmart
  assertParsedMode "explicit equals argument" [ "--single-line-let-in=never" ] SingleLineLetInNever
  assertJsonMode "legacy boolean true config" (Json.fromBoolean true) SingleLineLetInAlways
  assertJsonMode "legacy boolean false config" (Json.fromBoolean false) SingleLineLetInNever
  assertJsonMode "string smart config" (Json.fromString "smart") SingleLineLetInSmart
  assertJsonModeFails "invalid numeric config" (Json.fromNumber 1.0)

assertEqual :: forall a. Eq a => String -> a -> a -> Effect Unit
assertEqual name expected actual =
  when (actual /= expected) do
    Error.throw $ name <> " failed"

assertParsedMode :: String -> Array String -> SingleLineLetInOption -> Effect Unit
assertParsedMode name args expected =
  case Arg.parseArgs "format-options-test" "Parse format options." Bin.formatOptions (Arguments.normalizeLegacyArgs args) of
    Right options
      | options.singleLineLetIn == expected ->
          pure unit
      | otherwise ->
          Error.throw $ name <> " parsed the wrong singleLineLetIn mode"
    Left err ->
      Error.throw $ name <> " failed to parse: " <> Arg.printArgError err

assertJsonMode :: String -> Json.Json -> SingleLineLetInOption -> Effect Unit
assertJsonMode name value expected =
  case Bin.fromJson (Json.jsonSingletonObject "singleLineLetIn" value) of
    Right options
      | options.singleLineLetIn == expected ->
          pure unit
      | otherwise ->
          Error.throw $ name <> " decoded the wrong singleLineLetIn mode"
    Left err ->
      Error.throw $ name <> " failed to decode: " <> printJsonDecodeError err

assertJsonModeFails :: String -> Json.Json -> Effect Unit
assertJsonModeFails name value =
  case Bin.fromJson (Json.jsonSingletonObject "singleLineLetIn" value) of
    Left _ ->
      pure unit
    Right _ ->
      Error.throw $ name <> " unexpectedly decoded"
