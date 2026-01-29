module Snapshots exposing (run)

{-| Example snapshot tests demonstrating the Snapshot testing framework.
-}

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import FatalError exposing (FatalError)
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Script as Script exposing (Script)
import Snapshot


run : Script
run =
    Snapshot.run
        [ -- Pure tests (no IO)
          Snapshot.test "greeting" <|
            \() ->
                greet "World"
        , Snapshot.test "user JSON encoding" <|
            \() ->
                encodeUser { name = "Alice", age = 30 }
        , Snapshot.test "format shopping list" <|
            \() ->
                formatList [ "Apples", "Bananas", "Cherries" ]

        -- BackendTask test (with IO)
        , Snapshot.taskTest "elm.json direct dependencies" <|
            readDirectDeps
        ]


greet : String -> String
greet name =
    "Hello, " ++ name ++ "!"


type alias User =
    { name : String
    , age : Int
    }


encodeUser : User -> String
encodeUser user =
    Encode.object
        [ ( "name", Encode.string user.name )
        , ( "age", Encode.int user.age )
        ]
        |> Encode.encode 2


formatList : List String -> String
formatList items =
    items
        |> List.indexedMap (\i item -> String.fromInt (i + 1) ++ ". " ++ item)
        |> String.join "\n"


{-| Read elm.json and extract direct dependencies.
This demonstrates a BackendTask-powered test that does file IO.
-}
readDirectDeps : BackendTask FatalError String
readDirectDeps =
    File.jsonFile directDepsDecoder "elm.json"
        |> BackendTask.allowFatal
        |> BackendTask.map
            (\deps ->
                deps
                    |> List.sort
                    |> List.map (\dep -> "- " ++ dep)
                    |> String.join "\n"
            )


directDepsDecoder : Decode.Decoder (List String)
directDepsDecoder =
    Decode.at [ "dependencies", "direct" ] (Decode.keyValuePairs Decode.string)
        |> Decode.map (List.map Tuple.first)
