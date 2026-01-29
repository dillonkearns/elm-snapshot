module Snapshot exposing (Test, run, taskTest, test)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Cli.Option as Option
import Cli.OptionsParser as OptionsParser
import Cli.Program as Program
import Diff exposing (Change(..))
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)


type Test
    = PureTest String (() -> String)
    | TaskTest String (BackendTask FatalError String)


test : String -> (() -> String) -> Test
test name fn =
    PureTest name fn


taskTest : String -> BackendTask FatalError String -> Test
taskTest name task =
    TaskTest name task


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
    }


type ApproveMode
    = NoApprove
    | ApproveAll
    | ApproveNamed String


run : List Test -> Script
run tests =
    Script.withCliOptions program
        (\options ->
            tests
                |> List.map (runTest options)
                |> BackendTask.combine
                |> BackendTask.andThen (reportResults options)
        )


program : Program.Config CliOptions
program =
    Program.config
        |> Program.add
            (OptionsParser.build
                (\approveAll approveOnly ci ->
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
                    }
                )
                |> OptionsParser.with (Option.flag "approve")
                |> OptionsParser.with (Option.optionalKeywordArg "approve-only")
                |> OptionsParser.with (Option.flag "ci")
                |> OptionsParser.withDoc "Run snapshot tests"
            )


runTest : CliOptions -> Test -> BackendTask FatalError TestResult
runTest options testCase =
    case testCase of
        PureTest name fn ->
            runTestWithReceived options name (BackendTask.succeed (fn ()))

        TaskTest name task ->
            runTestWithReceived options name task


runTestWithReceived : CliOptions -> String -> BackendTask FatalError String -> BackendTask FatalError TestResult
runTestWithReceived options name receivedTask =
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
    in
    receivedTask
        |> BackendTask.andThen
            (\received ->
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
    "tests/snapshots/" ++ sanitizeName testName ++ extension


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
