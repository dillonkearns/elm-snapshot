module Formatter exposing (compact, json, plain)

{-| Format log entries in various styles.
-}

import Json.Encode as Encode
import LogEntry exposing (Level(..), LogEntry)


{-| Plain text format with level prefix.

    [INFO] 2024-01-15T10:30:00Z - User logged in

-}
plain : LogEntry -> String
plain entry =
    let
        levelStr =
            case entry.level of
                Info ->
                    "[INFO]"

                Warn ->
                    "[WARN]"

                Error ->
                    "[ERROR]"

        contextStr =
            case entry.context of
                Just ctx ->
                    " (" ++ ctx ++ ")"

                Nothing ->
                    ""
    in
    levelStr ++ " " ++ entry.timestamp ++ " - " ++ entry.message ++ contextStr


{-| JSON format for structured logging.
-}
json : LogEntry -> Encode.Value
json entry =
    LogEntry.encode entry


{-| Compact single-line format for high-volume logs.

    I|10:30:00|User logged in

-}
compact : LogEntry -> String
compact entry =
    let
        levelChar =
            case entry.level of
                Info ->
                    "I"

                Warn ->
                    "W"

                Error ->
                    "E"

        -- Extract just the time portion if it's an ISO timestamp
        time =
            entry.timestamp
                |> String.split "T"
                |> List.drop 1
                |> List.head
                |> Maybe.withDefault entry.timestamp
                |> String.split "Z"
                |> List.head
                |> Maybe.withDefault entry.timestamp
    in
    levelChar ++ "|" ++ time ++ "|" ++ entry.message
