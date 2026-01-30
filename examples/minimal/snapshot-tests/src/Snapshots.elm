module Snapshots exposing (run)

{-| Minimal snapshot tests demonstrating the basics.
-}

import Greeting
import Pages.Script exposing (Script)
import Snapshot


run : Script
run =
    Snapshot.run "Snapshots"
        [ Snapshot.test "greeting" <|
            \() -> Greeting.greet "World"
        , Snapshot.test "greeting with name" <|
            \() -> Greeting.greet "Elm"
        ]
