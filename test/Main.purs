module Test.Main where

import Prelude

import Ansi.Output (foreground, withGraphics)
import ArgParse.Basic as Arg
import Bin.Arguments as Arguments
import Bin.FormatOptions as Bin
import Data.Argonaut.Core as Json
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.Foldable (any, findMap, fold, foldMap, for_)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple(..), fst)
import Dodo.Ansi (Color(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Error
import Foreign.Object as Object
import Main as PursfmtMain
import Node.Process as Process
import Prim.Row as Row
import Prim.RowList as RL
import Pursfmt (AlignCaseBranchesOption(..), AlignClausesOption(..), ImportSortOption(..), ImportWrapOption(..), SingleLineLetInOption(..), ThenPlacementOption(..), TypeArrowOption(..), UnicodeOption(..))
import Record as Record
import Test.Snapshot (SnapshotResult(..), SnapshotResultGroup(..), snapshotFormat)
import Type.Proxy (Proxy(..))

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
  for_ [ Bin.defaults, allChanged, otherValues ] assertConfigRoundTrip
  assertNoDuplicateNames
  where
  -- Between them, `Bin.defaults` and these two use every value of every
  -- option, including `null` for the optional ones.
  allChanged :: Bin.FormatOptions
  allChanged =
    { importSort: ImportSortIde
    , importWrap: ImportWrapAuto
    , indent: 4
    , operatorsFile: Just ".pursfmt-operators"
    , ribbon: 0.5
    , thenPlacement: ThenNewLineForLongLine
    , typeArrowPlacement: TypeArrowLast
    , unicode: UnicodeAlways
    , width: Just 80
    , whereClauseSameLine: true
    , compactRecords: true
    , letClauseSameLine: true
    , singleLineLetIn: SingleLineLetInSmart
    , alignClauses: AlignClausesFull
    , alignCaseBranches: AlignCaseBranchesFull
    }

  otherValues :: Bin.FormatOptions
  otherValues = allChanged
    { thenPlacement = ThenNewLineAlways
    , unicode = UnicodeNever
    , singleLineLetIn = SingleLineLetInAlways
    , alignClauses = AlignClausesEquals
    , alignCaseBranches = AlignCaseBranchesArrows
    }

-- | Write the options the way `generate-config` does and read them back the
-- | way `.pursfmt.yaml` is read. The option names come from the
-- | `FormatOptions` type, so a new option is checked without changing this.
assertConfigRoundTrip :: Bin.FormatOptions -> Effect Unit
assertConfigRoundTrip options = do
  let yaml = Bin.printConfig options
  let fields = recordFields options
  json <- either Error.throw pure (Bin.parseConfigJson "generated config" yaml)
  let written = maybe [] Object.keys (Json.toObject json)
  let missing = Array.filter (\label -> not (Array.elem label written)) (map _.label fields)
  unless (Array.null missing) do
    Error.throw $ "generate-config does not write " <> String.joinWith ", " missing <> ", it wrote:\n" <> yaml
  decoded <- either Error.throw pure (Bin.parseConfig "generated config" yaml)
  let changed = map _.label (Array.filter (\field -> not (field.same options decoded)) fields)
  unless (Array.null changed) do
    Error.throw $ "config round trip changed " <> String.joinWith ", " changed <> ", generate-config wrote:\n" <> yaml

type Field r = { label :: String, same :: Record r -> Record r -> Boolean }

recordFields :: forall r rl. RL.RowToList r rl => Fields rl r => Record r -> Array (Field r)
recordFields _ = fields (Proxy :: Proxy rl)

-- | The label of every field in a record type, with an equality check for
-- | that field.
class Fields :: RL.RowList Type -> Row Type -> Constraint
class Fields rl r where
  fields :: Proxy rl -> Array (Field r)

instance fieldsNil :: Fields RL.Nil r where
  fields _ = []

instance fieldsCons :: (IsSymbol label, Row.Cons label a rest r, Eq a, Fields tail r) => Fields (RL.Cons label a tail) r where
  fields _ = Array.cons field (fields (Proxy :: Proxy tail))
    where
    labelProxy = Proxy :: Proxy label
    field =
      { label: reflectSymbol labelProxy
      , same: \x y -> Record.get labelProxy x == Record.get labelProxy y
      }

-- | Two flags or commands with the same name make one of them unreachable.
assertNoDuplicateNames :: Effect Unit
assertNoDuplicateNames = do
  let found = commands (Arg.HelpCommand [ "pursfmt" ] "" (Arg.parserHelp PursfmtMain.parser))
  unless (Array.any (eq "format" <<< fst) found) do
    Error.throw "the format command is missing from the parser help"
  for_ found \(Tuple command help) -> do
    let used = names help
    let duplicates = Array.nub (Array.difference used (Array.nub used))
    unless (Array.null duplicates) do
      Error.throw $ command <> " has more than one flag or command named " <> String.joinWith ", " duplicates
  where
  -- Every command in the help, with its own help.
  commands :: Arg.ArgHelp -> Array (Tuple String Arg.ArgHelp)
  commands = case _ of
    Arg.HelpCommand commandNames _ help -> Array.cons (Tuple (String.joinWith ", " commandNames) help) (commands help)
    Arg.HelpFormat _ help -> commands help
    Arg.HelpArgs helps -> foldMap commands helps
    Arg.HelpChoose _ helps -> foldMap commands helps
    _ -> []

  -- The names of one command's flags and of its direct subcommands.
  names :: Arg.ArgHelp -> Array String
  names = case _ of
    Arg.HelpFlag flagNames _ -> flagNames
    Arg.HelpCommand commandNames _ _ -> commandNames
    Arg.HelpFormat _ help -> names help
    Arg.HelpArgs helps -> foldMap names helps
    Arg.HelpChoose _ helps -> foldMap names helps
    _ -> []

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
