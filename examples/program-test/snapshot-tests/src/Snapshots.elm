module Snapshots exposing (run)

import CounterApp
import Pages.Script exposing (Script)
import ProgramTest exposing (ProgramTest)
import Snapshot
import Snapshot.ProgramTest
import Test.Html.Selector as Selector


run : Script
run =
    Snapshot.run "Snapshots"
        [ Snapshot.describe "Counter App"
            [ Snapshot.ProgramTest.view [] "initial view" <|
                \() -> startCounterApp
            , Snapshot.ProgramTest.view [] "view after increment" <|
                \() ->
                    startCounterApp
                        |> ProgramTest.clickButton "++"
            , Snapshot.ProgramTest.view [ Selector.tag "span" ] "just the counter" <|
                \() ->
                    startCounterApp
                        |> ProgramTest.clickButton "++"
            , Snapshot.ProgramTest.model Debug.toString "model after two increments" <|
                \() ->
                    startCounterApp
                        |> ProgramTest.clickButton "++"
                        |> ProgramTest.clickButton "++"
                        |> ProgramTest.clickButton "++"
                        |> ProgramTest.clickButton "++"
            ]
        ]


startCounterApp : ProgramTest CounterApp.Model CounterApp.Msg ()
startCounterApp =
    ProgramTest.createSandbox
        { init = CounterApp.init
        , update = CounterApp.update
        , view = CounterApp.view
        }
        |> ProgramTest.start ()
