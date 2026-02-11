module Snapshots exposing (run)

import CounterApp
import Pages.Script exposing (Script)
import ProgramTest
import Snapshot
import Snapshot.Printer as Printer


run : Script
run =
    Snapshot.run "Snapshots"
        [ Snapshot.describe "Counter"
            [ Snapshot.checkedTest "initial view" <|
                \() ->
                    counterApp
                        |> ProgramTest.getViewHtml
            , Snapshot.checkedTest "after clicking + three times" <|
                \() ->
                    counterApp
                        |> ProgramTest.clickButton "++"
                        |> ProgramTest.clickButton "++"
                        |> ProgramTest.clickButton "++"
                        |> ProgramTest.getViewHtml
            , Snapshot.checkedTest "after clicking -- twice" <|
                \() ->
                    counterApp
                        |> ProgramTest.clickButton "--"
                        |> ProgramTest.clickButton "--"
                        |> ProgramTest.getViewHtml
            , Snapshot.checkedCustom (Printer.elm Debug.toString) "model after interactions" <|
                \() ->
                    counterApp
                        |> ProgramTest.clickButton "++"
                        |> ProgramTest.clickButton "++"
                        |> ProgramTest.clickButton "--"
                        |> ProgramTest.getModel
            ]
        ]


counterApp : ProgramTest.ProgramTest CounterApp.Model CounterApp.Msg ()
counterApp =
    ProgramTest.createSandbox
        { init = CounterApp.init
        , update = CounterApp.update
        , view = CounterApp.view
        }
        |> ProgramTest.start ()
