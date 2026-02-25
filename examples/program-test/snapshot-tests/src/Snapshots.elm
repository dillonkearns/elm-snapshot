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
            [ Snapshot.ProgramTest.view [] "initial view" counterApp
            , Snapshot.ProgramTest.view [] "view after increment"
                (counterApp |> ProgramTest.clickButton "++")
            , Snapshot.ProgramTest.view [ Selector.tag "span" ] "just the counter"
                (counterApp |> ProgramTest.clickButton "++")
            , Snapshot.ProgramTest.model Debug.toString "model after two increments"
                (counterApp
                    |> ProgramTest.clickButton "++"
                    |> ProgramTest.clickButton "++"
                    |> ProgramTest.clickButton "++"
                    |> ProgramTest.clickButton "++"
                )
            ]
        ]


counterApp : ProgramTest CounterApp.Model CounterApp.Msg ()
counterApp =
    ProgramTest.createSandbox
        { init = CounterApp.init
        , update = CounterApp.update
        , view = CounterApp.view
        }
        |> ProgramTest.start ()
