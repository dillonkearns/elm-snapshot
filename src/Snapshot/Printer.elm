module Snapshot.Printer exposing (Printer, json, string, withExtension)

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


# Customization

@docs withExtension

-}

import Json.Decode as Decode
import Json.Encode as Encode


{-| A printer converts a value of type `a` to a String and specifies
the file extension for syntax highlighting in diff tools.

The extension is used to generate snapshot filenames like:

    test_name.approved.json
    test_name.received.txt

-}
type alias Printer a =
    { extension : String
    , print : a -> String
    }


{-| Identity printer for string output.

    Snapshot.expect Printer.string "log output" <|
        \() -> formatLog entry

Produces `.txt` files by default.

-}
string : Printer String
string =
    { extension = "txt"
    , print = identity
    }


{-| JSON pretty-printer with sorted keys and 2-space indentation.

    Snapshot.json "user data" <|
        \() -> User.encode user

**Keys are sorted alphabetically** at all nesting levels for deterministic
output. This matches the behavior of Go's `json.Marshal` and Jest's
`pretty-format` - the modern consensus for testing tools.

Uses 2-space indentation, which is the most common convention for JSON
and produces readable diffs with reasonable line lengths.

Produces `.json` files for syntax highlighting in diff tools.

-}
json : Printer Encode.Value
json =
    { extension = "json"
    , print =
        \value ->
            value
                |> sortJsonKeys
                |> Encode.encode 2
    }


{-| Change the file extension of a printer.

    xmlPrinter : Printer String
    xmlPrinter =
        Printer.string
            |> Printer.withExtension "xml"

Useful when the content type doesn't match the default extension.

-}
withExtension : String -> Printer a -> Printer a
withExtension ext printer =
    { printer | extension = ext }


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
