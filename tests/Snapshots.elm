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
import Snapshot.Scrubber as Scrubber


run : Script
run =
    Snapshot.run
        [ -- Pure tests (no IO) - string output
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

        -- JSON output with pretty printing
        , Snapshot.json 2 "config with printer" <|
            \() ->
                Encode.object
                    [ ( "name", Encode.string "MyApp" )
                    , ( "version", Encode.string "1.0.0" )
                    , ( "debug", Encode.bool False )
                    ]

        -- String test with scrubber (timestamps)
        , Snapshot.test "log entry with timestamp"
            (\() ->
                "2024-01-15T10:30:00Z - User logged in\n2024-01-15T10:31:45Z - User viewed dashboard"
            )
            |> Snapshot.withScrubbers [ Scrubber.timestamp ]

        -- String test with GUID scrubber
        , Snapshot.test "record with GUIDs"
            (\() ->
                """{ "id": "550e8400-e29b-41d4-a716-446655440000", "parentId": "550e8400-e29b-41d4-a716-446655440000", "siblingId": "6ba7b810-9dad-11d1-80b4-00c04fd430c8" }"""
            )
            |> Snapshot.withScrubbers [ Scrubber.guid ]

        -- JSON with sorted keys (keys defined in z-a order, printed in a-z)
        , Snapshot.json 2 "json keys sorted alphabetically" <|
            \() ->
                Encode.object
                    [ ( "zebra", Encode.string "last letter" )
                    , ( "apple", Encode.string "first letter" )
                    , ( "middle", Encode.int 42 )
                    , ( "nested"
                      , Encode.object
                            [ ( "z", Encode.bool True )
                            , ( "a", Encode.bool False )
                            ]
                      )
                    ]
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
