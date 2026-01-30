module Snapshot.Scrubber exposing
    ( Scrubber
    , all
    , custom
    , guid
    , regex
    , timestamp
    )

{-| Scrubbers clean non-deterministic content from snapshot output.

"Fundamentally, a scrubber is a function that takes a string and returns a string."
— ApprovalTests documentation

Scrubbers are applied AFTER the printer converts your value to a string.
They replace variable content (timestamps, GUIDs, etc.) with stable placeholders.

-}

import Dict exposing (Dict)
import Regex


{-| A scrubber transforms a string, typically replacing non-deterministic
content with stable placeholders.
-}
type alias Scrubber =
    String -> String


{-| Create a custom scrubber from any String -> String function.

    myCustomScrubber : Scrubber
    myCustomScrubber =
        Scrubber.custom (String.replace "secret" "[REDACTED]")

-}
custom : (String -> String) -> Scrubber
custom fn =
    fn


{-| Compose multiple scrubbers into one. Applied left to right.

    Scrubber.all
        [ Scrubber.timestamp
        , Scrubber.guid
        , Scrubber.regex "v\\d+\\.\\d+\\.\\d+" "[VERSION]"
        ]

-}
all : List Scrubber -> Scrubber
all scrubbers input =
    List.foldl (\scrubber acc -> scrubber acc) input scrubbers


{-| Replace all matches of a regex pattern with a fixed string.

    -- Replace version numbers
    Scrubber.regex "v\\d+\\.\\d+\\.\\d+" "[VERSION]"

    -- Replace port numbers
    Scrubber.regex ":\\d{4,5}" ":[PORT]"

Note: Uses Elm's Regex module. Invalid patterns are silently ignored.

-}
regex : String -> String -> Scrubber
regex pattern replacement input =
    case Regex.fromString pattern of
        Just compiledRegex ->
            Regex.replace compiledRegex (\_ -> replacement) input

        Nothing ->
            -- Invalid regex, return unchanged
            input


{-| Replace ISO 8601 timestamps with `[TIMESTAMP]`.

Matches patterns like:

  - `2024-01-15T10:30:00Z`
  - `2024-01-15T10:30:00.123Z`
  - `2024-01-15T10:30:00+05:00`
  - `2024-01-15 10:30:00`

-}
timestamp : Scrubber
timestamp input =
    let
        -- ISO 8601 with optional milliseconds and timezone
        isoPattern =
            "\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|[+-]\\d{2}:?\\d{2})?"
    in
    regex isoPattern "[TIMESTAMP]" input


{-| Replace GUIDs/UUIDs with stable placeholders like `[GUID-1]`, `[GUID-2]`.

Preserves referential equality: the same GUID appearing multiple times
in the output will get the same placeholder number.

Matches standard UUID format: `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`

-}
guid : Scrubber
guid input =
    let
        guidPattern =
            "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}"
    in
    case Regex.fromString guidPattern of
        Just compiledRegex ->
            let
                matches =
                    Regex.find compiledRegex input
                        |> List.map .match

                uniqueGuids =
                    matches
                        |> List.foldl
                            (\match acc ->
                                if List.member match acc then
                                    acc

                                else
                                    acc ++ [ match ]
                            )
                            []

                guidMap =
                    uniqueGuids
                        |> List.indexedMap (\i g -> ( g, "[GUID-" ++ String.fromInt (i + 1) ++ "]" ))
                        |> Dict.fromList
            in
            Regex.replace compiledRegex
                (\match ->
                    Dict.get match.match guidMap
                        |> Maybe.withDefault match.match
                )
                input

        Nothing ->
            input
