module Snapshot.Printer exposing (Printer, json, string)

{-| Printers convert domain objects to strings for snapshot comparison.

Every snapshot test has a printer (even if it's just identity).
The printer's job is to create a human-readable, diffable representation.

**Best practices (Emily Bache):**

  - Keep lines short for easy diff viewing
  - JSON can be tricky - prefer pretty-printing
  - Include inputs in output for context
  - Change printers in separate commits from logic changes


# Definition

@docs Printer


# Common Printers

@docs string, json

-}

import Json.Decode as Decode
import Json.Encode as Encode


{-| A printer converts a value of type `a` to a String.
-}
type alias Printer a =
    a -> String


{-| Identity printer - use when your test already returns a String.

    Snapshot.expect Printer.string "log output" <|
        \() -> formatLog entry

Most of the time you'll use `Snapshot.test` directly for string output.
Use `Printer.string` with `Snapshot.expect` when you want to be explicit.

-}
string : Printer String
string =
    identity


{-| JSON pretty-printer with sorted keys and configurable indentation.

    -- Using the convenience function:
    Snapshot.json 2 "user data" <|
        \() -> User.encode user

    -- Or with explicit printer:
    Snapshot.expect (Printer.json 4) "user data" <|
        \() -> User.encode user

The indent parameter controls spaces per indentation level.
Use 2 or 4 for readable output with short lines.

**Keys are sorted alphabetically** at all nesting levels for deterministic
output. This matches the behavior of Go's `json.Marshal` and Jest's
`pretty-format` - the modern consensus for testing tools.

-}
json : Int -> Printer Encode.Value
json indent value =
    value
        |> sortJsonKeys
        |> Encode.encode indent


{-| Recursively sort all object keys in a JSON value.
-}
sortJsonKeys : Encode.Value -> Encode.Value
sortJsonKeys value =
    case Decode.decodeValue jsonValueDecoder value of
        Ok sortedValue ->
            sortedValue

        Err _ ->
            -- If decoding fails somehow, return original
            value


{-| Decoder that reconstructs JSON with sorted keys.
-}
jsonValueDecoder : Decode.Decoder Encode.Value
jsonValueDecoder =
    Decode.oneOf
        [ Decode.null Encode.null
        , Decode.bool |> Decode.map Encode.bool
        , Decode.int |> Decode.map Encode.int
        , Decode.float |> Decode.map Encode.float
        , Decode.string |> Decode.map Encode.string
        , Decode.list (Decode.lazy (\_ -> jsonValueDecoder))
            |> Decode.map (Encode.list identity)
        , Decode.keyValuePairs (Decode.lazy (\_ -> jsonValueDecoder))
            |> Decode.map
                (\pairs ->
                    pairs
                        |> List.sortBy Tuple.first
                        |> Encode.object
                )
        ]
