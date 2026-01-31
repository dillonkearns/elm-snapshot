module Snapshot exposing
    ( Test
    , run
    , test
    , json
    , custom
    , taskTest
    , taskJson
    , taskCustom
    , withScrubbers
    , describe
    , only
    , skip
    , todo
    )

{-| Snapshot testing framework for Elm.

An idiomatic Elm API for approval/snapshot testing.

    -- String output (most common)
    Snapshot.test "greeting" <|
        \() -> greet "World"

    -- JSON output with pretty printing
    Snapshot.json "user data" <|
        \() -> User.encode user

    -- With scrubbers for non-deterministic output
    Snapshot.test "log entry" (\() -> formatLog entry)
        |> Snapshot.withScrubbers [ Scrubber.timestamp ]

    -- Grouped tests
    Snapshot.describe "Date formatting"
        [ Snapshot.test "ISO format" <| \() -> Date.toIso date
        , Snapshot.test "human readable" <| \() -> Date.toHuman date
        ]

    -- Custom printer
    Snapshot.custom myPrinter "custom format" <|
        \() -> myValue

    -- Focus on specific tests during development
    Snapshot.only <|
        Snapshot.test "work in progress" <|
            \() -> newFeature

    -- Skip tests temporarily
    Snapshot.skip <|
        Snapshot.test "broken test" <|
            \() -> brokenCode

    -- Placeholder for tests to implement later
    Snapshot.todo "implement edge case handling"

@docs Test, run, test, json, custom, taskTest, taskJson, taskCustom, withScrubbers, describe, only, skip, todo

-}

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Glob as Glob
import BackendTask.Stream as Stream
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Diff exposing (Change(..))
import FatalError exposing (FatalError)
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Set exposing (Set)
import Snapshot.Printer as Printer exposing (Printer)
import Snapshot.Scrubber exposing (Scrubber)


{-| An opaque type representing a snapshot test or group of tests.

Create tests using `test`, `json`, `custom`, or their `task*` variants.
Group related tests using `describe`.

-}
type Test
    = PureTest String String (List Scrubber) (() -> String)
    | TaskTest String String (List Scrubber) (BackendTask FatalError String)
    | Describe String (List Test)
    | Only Test
    | Skip Test
    | Todo String


{-| Create a snapshot test that compares string output.

    Snapshot.test "greeting" <|
        \() ->
            greet "World"

This is the most common form - use when your function already returns a String.

-}
test : String -> (() -> String) -> Test
test name fn =
    PureTest name "txt" [] fn


{-| Create a snapshot test for JSON values with pretty printing.

    Snapshot.json "user data" <|
        \() ->
            User.encode user

Uses 2-space indentation. Keys are sorted alphabetically for deterministic output.

-}
json : String -> (() -> Encode.Value) -> Test
json name fn =
    PureTest name Printer.json.extension [] (\() -> Printer.json.print (fn ()))


{-| Create a snapshot test with a custom printer.

    Snapshot.custom myXmlPrinter "config file" <|
        \() ->
            buildConfig options

Use this when you need a custom `a -> String` conversion.

-}
custom : Printer a -> String -> (() -> a) -> Test
custom printer name fn =
    PureTest name printer.extension [] (\() -> printer.print (fn ()))


{-| Add scrubbers to a test for non-deterministic output.

    Snapshot.test "log entry" (\() -> formatLog entry)
        |> Snapshot.withScrubbers [ Scrubber.timestamp ]

    Snapshot.json "api response" (\() -> encodeResponse resp)
        |> Snapshot.withScrubbers [ Scrubber.guid, Scrubber.timestamp ]

Scrubbers run after printing, replacing patterns like timestamps
and GUIDs with stable placeholders.

-}
withScrubbers : List Scrubber -> Test -> Test
withScrubbers scrubbers testCase =
    case testCase of
        PureTest name ext existingScrubbers fn ->
            PureTest name ext (existingScrubbers ++ scrubbers) fn

        TaskTest name ext existingScrubbers task ->
            TaskTest name ext (existingScrubbers ++ scrubbers) task

        Describe name tests ->
            Describe name (List.map (withScrubbers scrubbers) tests)

        Only innerTest ->
            Only (withScrubbers scrubbers innerTest)

        Skip innerTest ->
            Skip (withScrubbers scrubbers innerTest)

        Todo description ->
            Todo description


{-| Group related tests together.

    Snapshot.describe "Date formatting"
        [ Snapshot.test "ISO format" <|
            \() -> Date.toIsoString date
        , Snapshot.test "human readable" <|
            \() -> Date.toHumanString date
        ]

Test names are prefixed with the group name, separated by " / ".
Snapshot files are stored in subdirectories matching the group structure.

-}
describe : String -> List Test -> Test
describe name tests =
    Describe name tests


{-| Run only this test (and any other tests marked with `only`).

    Snapshot.only <|
        Snapshot.test "focus on this" <|
            \() -> workInProgress

When any test is marked with `only`, all tests without `only` will be skipped.
Use this during development to focus on specific tests.

Note: `skip` takes precedence over `only` - a skipped test won't run even
if it's inside an `only` block.

-}
only : Test -> Test
only =
    Only


{-| Skip this test.

    Snapshot.skip <|
        Snapshot.test "broken test" <|
            \() -> brokenCode

The test will be reported as skipped but won't be executed.
Use this to temporarily disable tests without deleting them.

Note: `skip` takes precedence over `only` - a skipped test won't run even
if it's inside an `only` block.

-}
skip : Test -> Test
skip =
    Skip


{-| A placeholder for a test you intend to write later.

    Snapshot.todo "handle edge case where input is empty"

The test will be reported as a todo item. This is useful for jotting down
test ideas without implementing them immediately.

-}
todo : String -> Test
todo =
    Todo


{-| Create a BackendTask-powered snapshot test for string output.

    Snapshot.taskTest "elm.json deps" <|
        File.rawFile "elm.json"
            |> BackendTask.allowFatal

Use this for tests that need IO (file reading, HTTP, etc).

-}
taskTest : String -> BackendTask FatalError String -> Test
taskTest name task =
    TaskTest name "txt" [] task


{-| Create a BackendTask-powered snapshot test for JSON values.

    Snapshot.taskJson "api response" <|
        Http.get url decoder
            |> BackendTask.allowFatal

Uses 2-space indentation. Keys are sorted alphabetically for deterministic output.

-}
taskJson : String -> BackendTask FatalError Encode.Value -> Test
taskJson name task =
    TaskTest name Printer.json.extension [] (BackendTask.map Printer.json.print task)


{-| Create a BackendTask-powered snapshot test with a custom printer.

    Snapshot.taskCustom myPrinter "fetched data" <|
        fetchData
            |> BackendTask.allowFatal

-}
taskCustom : Printer a -> String -> BackendTask FatalError a -> Test
taskCustom printer name task =
    TaskTest name printer.extension [] (BackendTask.map printer.print task)


type alias TestResult =
    { name : String
    , extension : String
    , outcome : Outcome
    }


type Outcome
    = Pass
    | FailNew String
    | FailMismatch { expected : String, received : String }
    | FailFatalError FatalError
    | Approved
    | Skipped
    | TodoOutcome String


type alias CliOptions =
    { approve : ApproveMode
    , ci : Bool
    , list : Bool
    , prune : Bool
    , reporter : Maybe String
    }


type ApproveMode
    = NoApprove
    | ApproveAll
    | ApproveNamed String


{-| Run a list of snapshot tests as an elm-pages Script.

    -- In your Snapshots.elm script:
    run : Script
    run =
        Snapshot.run "Snapshots"
            [ Snapshot.test "greeting" <| \() -> greet "World"
            , Snapshot.json "config" <| \() -> encodeConfig config
            ]

The first argument is the script name (typically matching the module name).
This organizes snapshots into `snapshots/<ScriptName>/` directories,
allowing multiple snapshot scripts in the same project.

Supports CLI options:

  - `--approve` - Approve all new/changed snapshots
  - `--approve-only "test name"` - Approve a specific test
  - `--ci` - Compact output for CI environments
  - `--list` - List all test names without running
  - `--prune` - Remove obsolete snapshot files
  - `--reporter code|opendiff|meld` - Open diff tool for failures

-}
run : String -> List Test -> Script
run scriptName tests =
    Script.withCliOptions program
        (\options ->
            let
                flattenedTests =
                    List.concatMap (flattenTest "") tests

                testNames =
                    List.map .name flattenedTests

                hasOnly =
                    List.any (\t -> t.runStatus == OnlyTest) flattenedTests
            in
            -- First check for duplicate test names
            case findDuplicates testNames of
                firstDuplicate :: _ ->
                    BackendTask.fail
                        (FatalError.fromString
                            ("Duplicate test name: \""
                                ++ firstDuplicate
                                ++ "\"\n\nEach snapshot test must have a unique name."
                            )
                        )

                [] ->
                    if options.list then
                        listTests scriptName flattenedTests

                    else
                        -- Run tests and check for obsolete/untracked snapshots
                        BackendTask.map3 (\a b c -> ( a, b, c ))
                            (flattenedTests
                                |> List.map (runFlatTest scriptName options hasOnly)
                                |> BackendTask.combine
                            )
                            (findObsoleteSnapshots scriptName testNames)
                            (findUntrackedSnapshots scriptName testNames)
                            |> BackendTask.andThen
                                (\( results, obsolete, untracked ) ->
                                    reportResultsWithObsolete scriptName options results obsolete untracked
                                )
        )


{-| Find duplicate strings in a list.
-}
findDuplicates : List String -> List String
findDuplicates names =
    let
        step name ( seen, duplicates ) =
            if Set.member name seen then
                ( seen, name :: duplicates )

            else
                ( Set.insert name seen, duplicates )
    in
    List.foldl step ( Set.empty, [] ) names
        |> Tuple.second
        |> List.reverse


{-| Find .approved files that don't correspond to any test.
-}
findObsoleteSnapshots : String -> List String -> BackendTask FatalError (List String)
findObsoleteSnapshots scriptName testNames =
    let
        -- Build set of expected base paths (without extension)
        expectedBasePaths =
            testNames
                |> List.map (snapshotBasePath scriptName)
                |> Set.fromList

        snapshotDir =
            "snapshots/" ++ sanitizeName scriptName ++ "/"

        -- Extract base path from a full file path (remove .approved.ext or .received.ext)
        getBasePath : String -> String
        getBasePath path =
            path
                |> String.replace ".approved" ""
                |> (\p ->
                        -- Remove the file extension (e.g., .json, .txt)
                        case String.split "." p |> List.reverse of
                            _ :: rest ->
                                rest |> List.reverse |> String.join "."

                            [] ->
                                p
                   )
    in
    Glob.succeed identity
        |> Glob.captureFilePath
        |> Glob.match (Glob.literal snapshotDir)
        |> Glob.match Glob.recursiveWildcard
        |> Glob.match (Glob.literal ".approved.")
        |> Glob.match Glob.wildcard
        |> Glob.toBackendTask
        |> BackendTask.map
            (\approvedFiles ->
                approvedFiles
                    |> List.filter (\path -> not (Set.member (getBasePath path) expectedBasePaths))
            )


{-| Get the base snapshot path without status or content extension.
-}
snapshotBasePath : String -> String -> String
snapshotBasePath scriptName testName =
    let
        segments =
            String.split " / " testName

        sanitizedSegments =
            List.map sanitizeName segments

        relativePath =
            String.join "/" sanitizedSegments
    in
    "snapshots/" ++ sanitizeName scriptName ++ "/" ++ relativePath


{-| Find .approved files that exist but aren't tracked by git.
-}
findUntrackedSnapshots : String -> List String -> BackendTask FatalError (List String)
findUntrackedSnapshots scriptName _ =
    let
        snapshotDir =
            "snapshots/" ++ sanitizeName scriptName ++ "/"
    in
    -- Use git ls-files to find untracked .approved.* files
    Stream.commandWithOptions
        (Stream.defaultCommandOptions |> Stream.allowNon0Status)
        "git"
        [ "ls-files", "--others", "--exclude-standard", snapshotDir ]
        |> Stream.read
        |> BackendTask.map
            (\result ->
                result.body
                    |> String.trim
                    |> String.lines
                    |> List.filter (\line -> String.contains ".approved." line && not (String.isEmpty line))
            )
        |> BackendTask.onError (\_ -> BackendTask.succeed [])


{-| Flatten nested describe blocks into a flat list of tests with prefixed names.
-}
flattenTest : String -> Test -> List FlatTest
flattenTest prefix testCase =
    flattenTestWithStatus prefix Normal testCase


flattenTestWithStatus : String -> RunStatus -> Test -> List FlatTest
flattenTestWithStatus prefix status testCase =
    case testCase of
        PureTest name ext scrubbers fn ->
            [ { name = prefixName prefix name
              , extension = ext
              , scrubbers = scrubbers
              , task = BackendTask.succeed (fn ())
              , runStatus = status
              }
            ]

        TaskTest name ext scrubbers task ->
            [ { name = prefixName prefix name
              , extension = ext
              , scrubbers = scrubbers
              , task = task
              , runStatus = status
              }
            ]

        Describe groupName nestedTests ->
            let
                newPrefix =
                    prefixName prefix groupName
            in
            List.concatMap (flattenTestWithStatus newPrefix status) nestedTests

        Only innerTest ->
            -- Only upgrades Normal to OnlyTest, but Skip takes precedence
            let
                newStatus =
                    case status of
                        SkipTest ->
                            SkipTest

                        TodoTest desc ->
                            TodoTest desc

                        _ ->
                            OnlyTest
            in
            flattenTestWithStatus prefix newStatus innerTest

        Skip innerTest ->
            -- Skip always wins
            flattenTestWithStatus prefix SkipTest innerTest

        Todo description ->
            [ { name = prefixName prefix description
              , extension = "txt"
              , scrubbers = []
              , task = BackendTask.succeed ""
              , runStatus = TodoTest description
              }
            ]


prefixName : String -> String -> String
prefixName prefix name =
    if String.isEmpty prefix then
        name

    else
        prefix ++ " / " ++ name


type alias FlatTest =
    { name : String
    , extension : String
    , scrubbers : List Scrubber
    , task : BackendTask FatalError String
    , runStatus : RunStatus
    }


type RunStatus
    = Normal
    | OnlyTest
    | SkipTest
    | TodoTest String


listTests : String -> List FlatTest -> BackendTask FatalError ()
listTests scriptName tests =
    let
        output =
            tests
                |> List.map (\t -> "  " ++ t.name)
                |> String.join "\n"

        header =
            scriptName ++ ": " ++ String.fromInt (List.length tests) ++ " snapshot tests\n\n"
    in
    Script.log (header ++ output)


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build
                (\approveAll approveOnly ci list prune reporter ->
                    { approve =
                        if approveAll then
                            ApproveAll

                        else
                            case approveOnly of
                                Just name ->
                                    ApproveNamed name

                                Nothing ->
                                    NoApprove
                    , ci = ci
                    , list = list
                    , prune = prune
                    , reporter = reporter
                    }
                )
                |> OptionsParser.with (Option.flag "approve")
                |> OptionsParser.with (Option.optionalKeywordArg "approve-only")
                |> OptionsParser.with (Option.flag "ci")
                |> OptionsParser.with (Option.flag "list")
                |> OptionsParser.with (Option.flag "prune")
                |> OptionsParser.with (Option.optionalKeywordArg "reporter")
                |> OptionsParser.withDoc "Run snapshot tests"
            )


runFlatTest : String -> CliOptions -> Bool -> FlatTest -> BackendTask FatalError TestResult
runFlatTest scriptName options hasOnly flatTest =
    let
        -- Determine effective run status considering the "only" filter
        effectiveStatus =
            case flatTest.runStatus of
                SkipTest ->
                    SkipTest

                TodoTest desc ->
                    TodoTest desc

                OnlyTest ->
                    OnlyTest

                Normal ->
                    if hasOnly then
                        -- Normal tests become skipped when there are "only" tests
                        SkipTest

                    else
                        Normal
    in
    case effectiveStatus of
        SkipTest ->
            BackendTask.succeed
                { name = flatTest.name
                , extension = flatTest.extension
                , outcome = Skipped
                }

        TodoTest description ->
            BackendTask.succeed
                { name = flatTest.name
                , extension = flatTest.extension
                , outcome = TodoOutcome description
                }

        _ ->
            runTestWithReceived scriptName options flatTest.name flatTest.extension flatTest.scrubbers flatTest.task


runTestWithReceived : String -> CliOptions -> String -> String -> List Scrubber -> BackendTask FatalError String -> BackendTask FatalError TestResult
runTestWithReceived scriptName options name extension scrubbers receivedTask =
    let
        approvedPath =
            snapshotPath scriptName name ".approved" extension

        receivedPath =
            snapshotPath scriptName name ".received" extension

        shouldApprove =
            case options.approve of
                ApproveAll ->
                    True

                ApproveNamed targetName ->
                    name == targetName

                NoApprove ->
                    False

        applyScrubbers : String -> String
        applyScrubbers input =
            List.foldl (\scrubber acc -> scrubber acc) input scrubbers
    in
    receivedTask
        |> BackendTask.andThen
            (\rawReceived ->
                let
                    received =
                        applyScrubbers rawReceived
                in
                File.rawFile approvedPath
                    |> BackendTask.map Just
                    |> BackendTask.onError (\_ -> BackendTask.succeed Nothing)
                    |> BackendTask.andThen
                        (\maybeApproved ->
                            case maybeApproved of
                                Nothing ->
                                    if shouldApprove then
                                        writeApprovedFile approvedPath received
                                            |> BackendTask.map
                                                (\_ ->
                                                    { name = name
                                                    , extension = extension
                                                    , outcome = Approved
                                                    }
                                                )

                                    else
                                        writeReceivedFile receivedPath received
                                            |> BackendTask.map
                                                (\_ ->
                                                    { name = name
                                                    , extension = extension
                                                    , outcome = FailNew received
                                                    }
                                                )

                                Just approved ->
                                    if received == approved then
                                        BackendTask.succeed
                                            { name = name
                                            , extension = extension
                                            , outcome = Pass
                                            }

                                    else if shouldApprove then
                                        writeApprovedFile approvedPath received
                                            |> BackendTask.andThen
                                                (\_ ->
                                                    deleteReceivedFile receivedPath
                                                )
                                            |> BackendTask.map
                                                (\_ ->
                                                    { name = name
                                                    , extension = extension
                                                    , outcome = Approved
                                                    }
                                                )

                                    else
                                        writeReceivedFile receivedPath received
                                            |> BackendTask.map
                                                (\_ ->
                                                    { name = name
                                                    , extension = extension
                                                    , outcome =
                                                        FailMismatch
                                                            { expected = approved
                                                            , received = received
                                                            }
                                                    }
                                                )
                        )
            )
        |> BackendTask.onError
            (\fatalError ->
                BackendTask.succeed
                    { name = name
                    , extension = extension
                    , outcome = FailFatalError fatalError
                    }
            )


writeReceivedFile : String -> String -> BackendTask FatalError ()
writeReceivedFile path content =
    Script.writeFile
        { path = path
        , body = content
        }
        |> BackendTask.allowFatal


writeApprovedFile : String -> String -> BackendTask FatalError ()
writeApprovedFile path content =
    Script.writeFile
        { path = path
        , body = content
        }
        |> BackendTask.allowFatal


deleteReceivedFile : String -> BackendTask FatalError ()
deleteReceivedFile path =
    BackendTask.succeed ()


snapshotPath : String -> String -> String -> String -> String
snapshotPath scriptName testName statusExtension contentExtension =
    let
        -- Split on " / " to get path segments from describe blocks
        segments =
            String.split " / " testName

        -- Sanitize each segment
        sanitizedSegments =
            List.map sanitizeName segments

        -- Join with "/" to create directory structure
        relativePath =
            String.join "/" sanitizedSegments
    in
    "snapshots/" ++ sanitizeName scriptName ++ "/" ++ relativePath ++ statusExtension ++ "." ++ contentExtension


sanitizeName : String -> String
sanitizeName name =
    name
        |> String.replace " " "_"
        |> String.replace "/" "_"
        |> String.replace "\\" "_"
        |> String.replace ":" "_"
        |> String.replace "\"" "_"
        |> String.replace "<" "_"
        |> String.replace ">" "_"
        |> String.replace "|" "_"
        |> String.replace "?" "_"
        |> String.replace "*" "_"


reportResultsWithObsolete : String -> CliOptions -> List TestResult -> List String -> List String -> BackendTask FatalError ()
reportResultsWithObsolete scriptName options results obsoleteSnapshots untrackedSnapshots =
    let
        passing =
            List.filter (\r -> r.outcome == Pass) results

        failing =
            List.filter isFailing results

        approved =
            List.filter (\r -> r.outcome == Approved) results

        skipped =
            List.filter (\r -> r.outcome == Skipped) results

        todos =
            List.filter
                (\r ->
                    case r.outcome of
                        TodoOutcome _ ->
                            True

                        _ ->
                            False
                )
                results

        resultLines =
            List.map (formatResult scriptName options) results

        obsoleteCount =
            List.length obsoleteSnapshots

        untrackedCount =
            List.length untrackedSnapshots

        skippedCount =
            List.length skipped

        todoCount =
            List.length todos

        summaryParts =
            [ String.fromInt (List.length passing) ++ " passing"
            , String.fromInt (List.length failing) ++ " failing"
            ]
                ++ (if List.length approved > 0 then
                        [ String.fromInt (List.length approved) ++ " approved" ]

                    else
                        []
                   )
                ++ (if skippedCount > 0 then
                        [ String.fromInt skippedCount ++ " skipped" ]

                    else
                        []
                   )
                ++ (if todoCount > 0 then
                        [ String.fromInt todoCount ++ " todo" ]

                    else
                        []
                   )
                ++ (if obsoleteCount > 0 && not options.prune then
                        [ String.fromInt obsoleteCount ++ " obsolete" ]

                    else
                        []
                   )
                ++ (if untrackedCount > 0 then
                        [ String.fromInt untrackedCount ++ " untracked" ]

                    else
                        []
                   )

        summary =
            String.join ", " summaryParts

        obsoleteWarning =
            if obsoleteCount > 0 && not options.ci then
                if options.prune then
                    "\n\n"
                        ++ yellow "Pruned"
                        ++ " "
                        ++ String.fromInt obsoleteCount
                        ++ " obsolete snapshot(s):\n"
                        ++ (obsoleteSnapshots
                                |> List.map (\p -> "  " ++ p)
                                |> String.join "\n"
                           )

                else
                    "\n\n"
                        ++ yellow "⚠"
                        ++ " "
                        ++ String.fromInt obsoleteCount
                        ++ " obsolete snapshot(s):\n"
                        ++ (obsoleteSnapshots
                                |> List.map (\p -> "  " ++ p)
                                |> String.join "\n"
                           )
                        ++ "\n\n  Run with --prune to remove obsolete snapshots."

            else
                ""

        untrackedWarning =
            if untrackedCount > 0 && not options.ci then
                "\n\n"
                    ++ yellow "⚠"
                    ++ " "
                    ++ String.fromInt untrackedCount
                    ++ " untracked snapshot(s):\n"
                    ++ (untrackedSnapshots
                            |> List.map (\p -> "  " ++ p)
                            |> String.join "\n"
                       )
                    ++ "\n\n  These exist locally but aren't in git. Run:\n    git add "
                    ++ (if untrackedCount == 1 then
                            Maybe.withDefault "" (List.head untrackedSnapshots)

                        else
                            "snapshots/"
                       )

            else
                ""

        -- Check if all tests are new (no approved files found at all)
        allTestsAreNew =
            List.all isFailNew results && not (List.isEmpty results)

        noApprovedNote =
            if allTestsAreNew && not options.ci then
                "\n\n"
                    ++ yellow "Note:"
                    ++ " No existing snapshots found in snapshots/"
                    ++ sanitizeName scriptName
                    ++ "/\n"
                    ++ "  - First run? Use --approve to create snapshots\n"
                    ++ "  - Expected existing snapshots? Check your working directory"

            else
                ""

        output =
            String.join "\n" resultLines ++ "\n\n" ++ summary ++ obsoleteWarning ++ untrackedWarning ++ noApprovedNote

        -- Prune obsolete snapshots if requested
        pruneTask =
            if options.prune && obsoleteCount > 0 then
                obsoleteSnapshots
                    |> List.map deleteFile
                    |> BackendTask.combine
                    |> BackendTask.map (\_ -> ())

            else
                BackendTask.succeed ()

        -- Open diff tool for failures (both mismatches and new snapshots)
        -- Use sequence (not combine) to run diff tools one at a time, since GUI diff tools
        -- like meld often have single-instance behavior that breaks with parallel launching
        reporterTask =
            case options.reporter of
                Just reporterName ->
                    failing
                        |> List.filterMap
                            (\result ->
                                case result.outcome of
                                    FailMismatch _ ->
                                        Just
                                            (openDiffTool reporterName
                                                ( snapshotPath scriptName result.name ".approved" result.extension
                                                , snapshotPath scriptName result.name ".received" result.extension
                                                )
                                            )

                                    FailNew _ ->
                                        let
                                            approvedPath =
                                                snapshotPath scriptName result.name ".approved" result.extension

                                            receivedPath =
                                                snapshotPath scriptName result.name ".received" result.extension
                                        in
                                        -- Create empty .approved file first so diff tool has something to compare against
                                        Just
                                            (writeApprovedFile approvedPath ""
                                                |> BackendTask.andThen (\_ -> openDiffTool reporterName ( approvedPath, receivedPath ))
                                            )

                                    _ ->
                                        Nothing
                            )
                        |> BackendTask.sequence
                        |> BackendTask.map (\_ -> ())

                Nothing ->
                    BackendTask.succeed ()
        -- Extract FatalErrors from failing tests
        fatalErrors =
            failing
                |> List.filterMap
                    (\result ->
                        case result.outcome of
                            FailFatalError err ->
                                Just err

                            _ ->
                                Nothing
                    )
    in
    pruneTask
        |> BackendTask.andThen (\_ -> reporterTask)
        |> BackendTask.andThen (\_ -> Script.log output)
        |> BackendTask.andThen
            (\_ ->
                if List.isEmpty failing then
                    BackendTask.succeed ()

                else
                    -- If there's exactly one FatalError, re-throw it so elm-pages prints the full message
                    case fatalErrors of
                        [ singleError ] ->
                            BackendTask.fail singleError

                        _ ->
                            BackendTask.fail
                                (FatalError.build
                                    { title = "Snapshot Tests Failed"
                                    , body = String.fromInt (List.length failing) ++ " test(s) failed"
                                    }
                                )
            )


{-| Open a diff tool to compare two files.
-}
openDiffTool : String -> ( String, String ) -> BackendTask FatalError ()
openDiffTool reporter ( approvedPath, receivedPath ) =
    case reporter of
        "code" ->
            Script.exec "code" [ "--diff", "--wait", approvedPath, receivedPath ]

        "opendiff" ->
            Script.exec "opendiff" [ "-W", approvedPath, receivedPath ]

        "meld" ->
            Script.exec "meld" [ approvedPath, receivedPath ]

        "ksdiff" ->
            Script.exec "ksdiff" [ "--wait", approvedPath, receivedPath ]

        "kdiff3" ->
            Script.exec "kdiff3" [ approvedPath, receivedPath ]

        "diff" ->
            Script.exec "diff" [ "-u", approvedPath, receivedPath ]

        other ->
            -- Try to use it as a command directly
            Script.exec other [ approvedPath, receivedPath ]


deleteFile : String -> BackendTask FatalError ()
deleteFile path =
    Script.exec "rm" [ path ]


isFailing : TestResult -> Bool
isFailing result =
    case result.outcome of
        Pass ->
            False

        Approved ->
            False

        Skipped ->
            False

        TodoOutcome _ ->
            False

        FailNew _ ->
            True

        FailMismatch _ ->
            True

        FailFatalError _ ->
            True


isFailNew : TestResult -> Bool
isFailNew result =
    case result.outcome of
        FailNew _ ->
            True

        _ ->
            False


formatResult : String -> CliOptions -> TestResult -> String
formatResult scriptName options result =
    case result.outcome of
        Pass ->
            green "✓" ++ " " ++ result.name

        Approved ->
            green "✓" ++ " " ++ result.name ++ dim " (approved)"

        FailNew received ->
            if options.ci then
                red "✗" ++ " " ++ result.name ++ " - new snapshot (no .approved file)"

            else
                red "✗"
                    ++ " "
                    ++ result.name
                    ++ "\n\n  New snapshot (no .approved file found)\n\n  Received:\n"
                    ++ indentBlock (green received)
                    ++ "\n\n  To approve this snapshot, run with:\n    --approve"
                    ++ "\n\n  Or manually:\n    mv "
                    ++ snapshotPath scriptName result.name ".received" result.extension
                    ++ " "
                    ++ snapshotPath scriptName result.name ".approved" result.extension

        FailMismatch { expected, received } ->
            if options.ci then
                red "✗" ++ " " ++ result.name ++ " - snapshot mismatch"

            else
                red "✗"
                    ++ " "
                    ++ result.name
                    ++ "\n\n  Snapshot mismatch:\n\n"
                    ++ formatDiff expected received
                    ++ "\n\n  To approve this change, run with:\n    --approve"
                    ++ "\n\n  Or manually:\n    mv "
                    ++ snapshotPath scriptName result.name ".received" result.extension
                    ++ " "
                    ++ snapshotPath scriptName result.name ".approved" result.extension

        FailFatalError _ ->
            red "✗" ++ " " ++ result.name ++ " - fatal error"

        Skipped ->
            yellow "○" ++ " " ++ result.name ++ dim " (skipped)"

        TodoOutcome description ->
            yellow "○" ++ " " ++ description ++ dim " (todo)"


formatDiff : String -> String -> String
formatDiff expected received =
    let
        expectedLines =
            String.lines expected

        receivedLines =
            String.lines received

        changes =
            Diff.diff expectedLines receivedLines
    in
    changes
        |> List.concatMap formatChange
        |> String.join "\n"


formatChange : Change Never String -> List String
formatChange change =
    case change of
        NoChange line ->
            [ "    " ++ dim line ]

        Removed line ->
            [ "  " ++ red ("- " ++ line) ]

        Added line ->
            [ "  " ++ green ("+ " ++ line) ]

        Similar _ _ n ->
            never n


indentBlock : String -> String
indentBlock content =
    content
        |> String.lines
        |> List.map (\line -> "    " ++ line)
        |> String.join "\n"



-- ANSI color helpers


red : String -> String
red text =
    "\u{001B}[31m" ++ text ++ "\u{001B}[0m"


green : String -> String
green text =
    "\u{001B}[32m" ++ text ++ "\u{001B}[0m"


yellow : String -> String
yellow text =
    "\u{001B}[33m" ++ text ++ "\u{001B}[0m"


dim : String -> String
dim text =
    "\u{001B}[2m" ++ text ++ "\u{001B}[0m"
