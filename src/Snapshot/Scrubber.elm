module Snapshot.Scrubber exposing
    ( Scrubber
    , timestamp, guid, date
    , regex, numbered, lines
    , all
    )

{-| Scrubbers clean non-deterministic content from snapshot output.

"Fundamentally, a scrubber is a function that takes a string and returns a string."
— [ApprovalTests documentation](https://approvaltests.com/docs/reference/Scrubbers/)

Scrubbers are applied AFTER the printer converts your value to a string.
They replace variable content (timestamps, GUIDs, etc.) with stable placeholders.

Since `Scrubber` is just a type alias for `String -> String`, any function
with that signature works as a scrubber:

    myScrubber : Scrubber
    myScrubber =
        String.replace "secret" "[REDACTED]"


# Definition

@docs Scrubber


# Built-in Scrubbers

@docs timestamp, guid, date


# Custom Scrubbers

@docs regex, numbered, lines


# Composition

@docs all

-}

import Dict exposing (Dict)
import Regex


{-| A scrubber transforms a string, typically replacing non-deterministic
content with stable placeholders.
-}
type alias Scrubber =
    String -> String


{-| Compose multiple scrubbers into one. Applied first to last.

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


{-| Replace regex matches with numbered placeholders like `[LABEL-1]`, `[LABEL-2]`.

Preserves referential equality: the same value appearing multiple times
gets the same placeholder number.

    -- User IDs: "user-123" and "user-456" → "[USER-1]" and "[USER-2]"
    Scrubber.numbered "user-\\d+" "USER"

    -- Request IDs that appear multiple times get consistent numbers
    Scrubber.numbered "req-[a-z0-9]+" "REQUEST"

The label is wrapped in brackets with a number: `[LABEL-1]`, `[LABEL-2]`, etc.

-}
numbered : String -> String -> Scrubber
numbered pattern label input =
    case Regex.fromString pattern of
        Just compiledRegex ->
            let
                matches =
                    Regex.find compiledRegex input
                        |> List.map .match

                uniqueMatches =
                    matches
                        |> List.foldl
                            (\match acc ->
                                if List.member match acc then
                                    acc

                                else
                                    acc ++ [ match ]
                            )
                            []

                replacementMap =
                    uniqueMatches
                        |> List.indexedMap
                            (\i match ->
                                ( match, "[" ++ label ++ "-" ++ String.fromInt (i + 1) ++ "]" )
                            )
                        |> Dict.fromList
            in
            Regex.replace compiledRegex
                (\match ->
                    Dict.get match.match replacementMap
                        |> Maybe.withDefault match.match
                )
                input

        Nothing ->
            input


{-| Remove or keep lines based on a predicate.

    -- Remove all lines containing "DEBUG"
    Scrubber.lines (not << String.contains "DEBUG:")

    -- Keep only lines starting with "ERROR"
    Scrubber.lines (String.startsWith "ERROR")

The predicate returns `True` for lines to keep, `False` for lines to remove.

-}
lines : (String -> Bool) -> Scrubber
lines predicate input =
    input
        |> String.lines
        |> List.filter predicate
        |> String.join "\n"


{-| Replace ISO 8601 timestamps with `[TIMESTAMP]`.

Matches patterns like:

  - `2024-01-15T10:30:00Z`
  - `2024-01-15T10:30:00.123Z`
  - `2024-01-15T10:30:00+05:00`
  - `2024-01-15 10:30:00`

-}
timestamp : Scrubber
timestamp =
    let
        -- ISO 8601 with optional milliseconds and timezone
        isoPattern =
            "\\d{4}-\\d{2}-\\d{2}[T ]\\d{2}:\\d{2}:\\d{2}(\\.\\d+)?(Z|[+-]\\d{2}:?\\d{2})?"
    in
    regex isoPattern "[TIMESTAMP]"


{-| Replace GUIDs/UUIDs with stable placeholders like `[GUID-1]`, `[GUID-2]`.

Preserves referential equality: the same GUID appearing multiple times
in the output will get the same placeholder number.

Matches standard UUID format: `xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx`

-}
guid : Scrubber
guid =
    numbered "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}" "GUID"


{-| Replace dates matching a format pattern with `[DATE-1]`, `[DATE-2]`, etc.

Provide a format string using these tokens:

  - `YYYY` - 4-digit year
  - `MM` - 2-digit month (01-12)
  - `DD` - 2-digit day (01-31)
  - `hh` - 2-digit hour (00-23)
  - `mm` - 2-digit minute (00-59)
  - `ss` - 2-digit second (00-59)

Examples:

    -- Match "2024-01-15"
    Scrubber.date "YYYY-MM-DD"

    -- Match "01/15/2024"
    Scrubber.date "MM/DD/YYYY"

    -- Match "15-Jan-2024"
    Scrubber.date "DD-Mon-YYYY"

    -- Match "2024-01-15 10:30:00"
    Scrubber.date "YYYY-MM-DD hh:mm:ss"

Like `guid`, the same date appearing multiple times gets the same number.

-}
date : String -> Scrubber
date format =
    let
        -- Convert format tokens to regex patterns
        pattern =
            format
                |> String.replace "YYYY" "\\d{4}"
                |> String.replace "MM" "\\d{2}"
                |> String.replace "DD" "\\d{2}"
                |> String.replace "Mon" "(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
                |> String.replace "hh" "\\d{2}"
                |> String.replace "mm" "\\d{2}"
                |> String.replace "ss" "\\d{2}"
                -- Escape special regex chars that might be in the format
                |> escapeRegexSpecials
    in
    numbered pattern "DATE"


{-| Escape regex special characters except for already-escaped patterns.
-}
escapeRegexSpecials : String -> String
escapeRegexSpecials str =
    -- We need to be careful not to escape the \d patterns we just inserted
    -- So we only escape characters that are likely format separators
    str
        |> String.replace "." "\\."
        |> String.replace "/" "\\/"
