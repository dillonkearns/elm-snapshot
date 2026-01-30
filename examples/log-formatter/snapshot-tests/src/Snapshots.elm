module Snapshots exposing (run)

{-| Snapshot tests for the log formatter.

Demonstrates:

  - String output with `Snapshot.test`
  - JSON output with `Snapshot.json`
  - Timestamp scrubbing with `Scrubber.timestamp`
  - Grouping with `Snapshot.describe`

-}

import Formatter
import LogEntry exposing (Level(..), LogEntry)
import Pages.Script exposing (Script)
import Snapshot
import Snapshot.Scrubber as Scrubber


run : Script
run =
    Snapshot.run "Snapshots"
        [ -- Test all formatters with the same log entries
          Snapshot.describe "plain format"
            [ Snapshot.test "info message" <|
                \() -> Formatter.plain infoEntry
            , Snapshot.test "warning with context" <|
                \() -> Formatter.plain warnEntry
            , Snapshot.test "error message" <|
                \() -> Formatter.plain errorEntry
            ]
        , Snapshot.describe "json format"
            [ Snapshot.json "info message" <|
                \() -> Formatter.json infoEntry
            , Snapshot.json "warning with context" <|
                \() -> Formatter.json warnEntry
            , Snapshot.json "error message" <|
                \() -> Formatter.json errorEntry
            ]
        , Snapshot.describe "compact format"
            [ Snapshot.test "info message" <|
                \() -> Formatter.compact infoEntry
            , Snapshot.test "warning" <|
                \() -> Formatter.compact warnEntry
            , Snapshot.test "error" <|
                \() -> Formatter.compact errorEntry
            ]

        -- Demonstrate timestamp scrubbing
        , Snapshot.describe "with scrubbed timestamps"
            [ Snapshot.test "plain format" (\() -> Formatter.plain dynamicEntry)
                |> Snapshot.withScrubbers [ Scrubber.timestamp ]
            , Snapshot.json "json format" (\() -> Formatter.json dynamicEntry)
                |> Snapshot.withScrubbers [ Scrubber.timestamp ]
            ]
        ]



-- Sample log entries


infoEntry : LogEntry
infoEntry =
    { level = Info
    , message = "User logged in"
    , timestamp = "2024-01-15T10:30:00Z"
    , context = Nothing
    }


warnEntry : LogEntry
warnEntry =
    { level = Warn
    , message = "Rate limit approaching"
    , timestamp = "2024-01-15T10:31:45Z"
    , context = Just "user-123"
    }


errorEntry : LogEntry
errorEntry =
    { level = Error
    , message = "Database connection failed"
    , timestamp = "2024-01-15T10:32:00Z"
    , context = Nothing
    }


{-| Entry with a "dynamic" timestamp that would change between runs.
We use scrubbers to make this deterministic.
-}
dynamicEntry : LogEntry
dynamicEntry =
    { level = Info
    , message = "Request processed"
    , timestamp = "2024-06-20T14:22:33.456Z"
    , context = Just "request-abc"
    }
