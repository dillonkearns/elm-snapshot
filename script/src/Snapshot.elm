module Snapshot exposing
    ( Test
    , run
    , test
    , json
    , expect
    , taskTest
    , taskJson
    , taskExpect
    , withScrubbers
    , describe
    )

{-| Snapshot testing framework for Elm.

An idiomatic Elm API for approval/snapshot testing.

    -- String output (most common)
    Snapshot.test "greeting" <|
        \() -> greet "World"

    -- JSON output with pretty printing
    Snapshot.json 2 "user data" <|
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
    Snapshot.expect myPrinter "custom format" <|
        \() -> myValue

@docs Test, run, test, json, expect, taskTest, taskJson, taskExpect, withScrubbers, describe

-}

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Diff exposing (Change(..))
import FatalError exposing (FatalError)
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Snapshot.Printer as Printer exposing (Printer)
import Snapshot.Scrubber exposing (Scrubber)


type Test
    = PureTest String (List Scrubber) (() -> String)
    | TaskTest String (List Scrubber) (BackendTask FatalError String)
    | Describe String (List Test)


{-| Create a snapshot test that compares string output.

    Snapshot.test "greeting" <|
        \() ->
            greet "World"

This is the most common form - use when your function already returns a String.

-}
test : String -> (() -> String) -> Test
test name fn =
    PureTest name [] fn


{-| Create a snapshot test for JSON values with pretty printing.

    Snapshot.json 2 "user data" <|
        \() ->
            User.encode user

The first argument is the indentation level (spaces per level).
Keys are sorted alphabetically for deterministic output.

-}
json : Int -> String -> (() -> Encode.Value) -> Test
json indent name fn =
    PureTest name [] (\() -> Printer.json indent (fn ()))


{-| Create a snapshot test with a custom printer.

    Snapshot.expect myXmlPrinter "config file" <|
        \() ->
            buildConfig options

Use this when you need a custom `a -> String` conversion.

-}
expect : Printer a -> String -> (() -> a) -> Test
expect printer name fn =
    PureTest name [] (\() -> printer (fn ()))


{-| Add scrubbers to a test for non-deterministic output.

    Snapshot.test "log entry" (\() -> formatLog entry)
        |> Snapshot.withScrubbers [ Scrubber.timestamp ]

    Snapshot.json 2 "api response" (\() -> encodeResponse resp)
        |> Snapshot.withScrubbers [ Scrubber.guid, Scrubber.timestamp ]

Scrubbers run after printing, replacing patterns like timestamps
and GUIDs with stable placeholders.

-}
withScrubbers : List Scrubber -> Test -> Test
withScrubbers scrubbers testCase =
    case testCase of
        PureTest name existingScrubbers fn ->
            PureTest name (existingScrubbers ++ scrubbers) fn

        TaskTest name existingScrubbers task ->
            TaskTest name (existingScrubbers ++ scrubbers) task

        Describe name tests ->
            Describe name (List.map (withScrubbers scrubbers) tests)


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


{-| Create a BackendTask-powered snapshot test for string output.

    Snapshot.taskTest "elm.json deps" <|
        File.rawFile "elm.json"
            |> BackendTask.allowFatal

Use this for tests that need IO (file reading, HTTP, etc).

-}
taskTest : String -> BackendTask FatalError String -> Test
taskTest name task =
    TaskTest name [] task


{-| Create a BackendTask-powered snapshot test for JSON values.

    Snapshot.taskJson 2 "api response" <|
        Http.get url decoder
            |> BackendTask.allowFatal

-}
taskJson : Int -> String -> BackendTask FatalError Encode.Value -> Test
taskJson indent name task =
    TaskTest name [] (BackendTask.map (Printer.json indent) task)


{-| Create a BackendTask-powered snapshot test with a custom printer.

    Snapshot.taskExpect myPrinter "fetched data" <|
        fetchData
            |> BackendTask.allowFatal

-}
taskExpect : Printer a -> String -> BackendTask FatalError a -> Test
taskExpect printer name task =
    TaskTest name [] (BackendTask.map printer task)


type alias TestResult =
    { name : String
    , outcome : Outcome
    }


type Outcome
    = Pass
    | FailNew String
    | FailMismatch { expected : String, received : String }
    | Approved


type alias CliOptions =
    { approve : ApproveMode
    , ci : Bool
    , list : Bool
    }


type ApproveMode
    = NoApprove
    | ApproveAll
    | ApproveNamed String


run : List Test -> Script
run tests =
    Script.withCliOptions program
        (\options ->
            let
                flattenedTests =
                    List.concatMap (flattenTest "") tests
            in
            if options.list then
                listTests flattenedTests

            else
                flattenedTests
                    |> List.map (runFlatTest options)
                    |> BackendTask.combine
                    |> BackendTask.andThen (reportResults options)
        )


{-| Flatten nested describe blocks into a flat list of tests with prefixed names.
-}
flattenTest : String -> Test -> List FlatTest
flattenTest prefix testCase =
    case testCase of
        PureTest name scrubbers fn ->
            [ { name = prefixName prefix name
              , scrubbers = scrubbers
              , task = BackendTask.succeed (fn ())
              }
            ]

        TaskTest name scrubbers task ->
            [ { name = prefixName prefix name
              , scrubbers = scrubbers
              , task = task
              }
            ]

        Describe groupName nestedTests ->
            let
                newPrefix =
                    prefixName prefix groupName
            in
            List.concatMap (flattenTest newPrefix) nestedTests


prefixName : String -> String -> String
prefixName prefix name =
    if String.isEmpty prefix then
        name

    else
        prefix ++ " / " ++ name


type alias FlatTest =
    { name : String
    , scrubbers : List Scrubber
    , task : BackendTask FatalError String
    }


listTests : List FlatTest -> BackendTask FatalError ()
listTests tests =
    let
        output =
            tests
                |> List.map (\t -> "  " ++ t.name)
                |> String.join "\n"

        header =
            String.fromInt (List.length tests) ++ " snapshot tests:\n\n"
    in
    Script.log (header ++ output)


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build
                (\approveAll approveOnly ci list ->
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
                    }
                )
                |> OptionsParser.with (Option.flag "approve")
                |> OptionsParser.with (Option.optionalKeywordArg "approve-only")
                |> OptionsParser.with (Option.flag "ci")
                |> OptionsParser.with (Option.flag "list")
                |> OptionsParser.withDoc "Run snapshot tests"
            )


runFlatTest : CliOptions -> FlatTest -> BackendTask FatalError TestResult
runFlatTest options flatTest =
    runTestWithReceived options flatTest.name flatTest.scrubbers flatTest.task


runTestWithReceived : CliOptions -> String -> List Scrubber -> BackendTask FatalError String -> BackendTask FatalError TestResult
runTestWithReceived options name scrubbers receivedTask =
    let
        approvedPath =
            snapshotPath name ".approved"

        receivedPath =
            snapshotPath name ".received"

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
                                                    , outcome = Approved
                                                    }
                                                )

                                    else
                                        writeReceivedFile receivedPath received
                                            |> BackendTask.map
                                                (\_ ->
                                                    { name = name
                                                    , outcome = FailNew received
                                                    }
                                                )

                                Just approved ->
                                    if received == approved then
                                        BackendTask.succeed
                                            { name = name
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
                                                    , outcome = Approved
                                                    }
                                                )

                                    else
                                        writeReceivedFile receivedPath received
                                            |> BackendTask.map
                                                (\_ ->
                                                    { name = name
                                                    , outcome =
                                                        FailMismatch
                                                            { expected = approved
                                                            , received = received
                                                            }
                                                    }
                                                )
                        )
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


snapshotPath : String -> String -> String
snapshotPath testName extension =
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
    "tests/snapshots/" ++ relativePath ++ extension


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


reportResults : CliOptions -> List TestResult -> BackendTask FatalError ()
reportResults options results =
    let
        passing =
            List.filter (\r -> r.outcome == Pass) results

        failing =
            List.filter (isFailing) results

        approved =
            List.filter (\r -> r.outcome == Approved) results

        resultLines =
            List.map (formatResult options) results

        summary =
            if options.ci then
                String.fromInt (List.length passing)
                    ++ " passing, "
                    ++ String.fromInt (List.length failing)
                    ++ " failing"

            else if List.length approved > 0 then
                String.fromInt (List.length passing)
                    ++ " passing, "
                    ++ String.fromInt (List.length failing)
                    ++ " failing, "
                    ++ String.fromInt (List.length approved)
                    ++ " approved"

            else
                String.fromInt (List.length passing)
                    ++ " passing, "
                    ++ String.fromInt (List.length failing)
                    ++ " failing"

        output =
            String.join "\n" resultLines ++ "\n\n" ++ summary
    in
    Script.log output
        |> BackendTask.andThen
            (\_ ->
                if List.isEmpty failing then
                    BackendTask.succeed ()

                else
                    BackendTask.fail (FatalError.fromString "Tests failed")
            )


isFailing : TestResult -> Bool
isFailing result =
    case result.outcome of
        Pass ->
            False

        Approved ->
            False

        FailNew _ ->
            True

        FailMismatch _ ->
            True


formatResult : CliOptions -> TestResult -> String
formatResult options result =
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
                    ++ snapshotPath result.name ".received"
                    ++ " "
                    ++ snapshotPath result.name ".approved"

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
                    ++ snapshotPath result.name ".received"
                    ++ " "
                    ++ snapshotPath result.name ".approved"


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


dim : String -> String
dim text =
    "\u{001B}[2m" ++ text ++ "\u{001B}[0m"
