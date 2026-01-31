module Snapshot.Printer exposing (Printer, elm, json, string, withExtension)

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

@docs string, json, elm


# Customization

@docs withExtension

-}

import DebugParser
import DebugParser.ElmValue as ElmValue exposing (ElmValue)
import ElmSyntaxParserLenient
import ElmSyntaxPrint
import Json.Decode as Decode
import Json.Encode as Encode
import Set exposing (Set)


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

    Snapshot.custom Printer.string "log output" <|
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


{-| Pretty-prints any Elm value using elm-format style.

Since published packages cannot use `Debug.toString` directly, you must pass
it in as the first argument:

    Snapshot.custom (Printer.elm Debug.toString) "user model" <|
        \() -> model.user

The output is formatted as valid Elm syntax, making diffs easy to read.

Produces `.elm` files for syntax highlighting in diff tools.

**Dependencies:** This printer relies on `kraklin/elm-debug-parser` and
`lue-bird/elm-syntax-format` to parse and format the debug output.

-}
elm : (a -> String) -> Printer a
elm debugToString =
    { extension = "elm"
    , print = prettifyValue debugToString
    }


prettifyValue : (a -> String) -> a -> String
prettifyValue debugToString value =
    let
        valueAsString : String
        valueAsString =
            debugToString value
    in
    case DebugParser.parse DebugParser.defaultConfig ("a: " ++ valueAsString) of
        Ok parsed ->
            case
                parsed.value
                    |> elmValueToString Set.empty
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                    |> Maybe.map
                        (\syntaxModule ->
                            syntaxModule.syntax
                                |> ElmSyntaxPrint.expressionNotParenthesized []
                                |> ElmSyntaxPrint.toString
                        )
            of
                Just formatted ->
                    formatted

                Nothing ->
                    "-- Error when formatting value\n" ++ valueAsString

        Err _ ->
            "-- Could not parse value, but here it is:\n" ++ valueAsString


elmValueToString : Set String -> ElmValue -> String
elmValueToString fieldsToSkip elmValue =
    case elmValue of
        ElmValue.Plain plainValue ->
            plainValueToString plainValue

        ElmValue.Expandable _ (ElmValue.ElmSequence sequenceType values) ->
            sequenceTypeToString sequenceType (List.map (elmValueToString fieldsToSkip) values)

        ElmValue.Expandable _ (ElmValue.ElmType typeName values) ->
            (typeName :: List.map (elmValueToString fieldsToSkip) values)
                |> join "(" " " ")"

        ElmValue.Expandable _ (ElmValue.ElmRecord values) ->
            List.filterMap
                (\( field, fieldValue ) ->
                    if Set.member field fieldsToSkip then
                        Nothing

                    else
                        Just (field ++ " = " ++ elmValueToString fieldsToSkip fieldValue)
                )
                values
                |> join "{" ", " "}"

        ElmValue.Expandable _ (ElmValue.ElmDict values) ->
            if List.isEmpty values then
                "Dict.empty"

            else
                List.map
                    (\( key, dictValue ) ->
                        "( " ++ elmValueToString fieldsToSkip key ++ ", " ++ elmValueToString fieldsToSkip dictValue ++ " )"
                    )
                    values
                    |> join "(Dict.fromList [" ", " "])"


plainValueToString : ElmValue.PlainValue -> String
plainValueToString plainValue =
    case plainValue of
        ElmValue.ElmString str ->
            "\""
                ++ (str
                        |> String.replace "\\" "\\\\"
                        |> String.replace "\"" "\\\""
                        |> String.replace "\n" "\\n"
                        |> String.replace "\t" "\\t"
                        |> String.replace "\u{000D}" "\\r"
                   )
                ++ "\""

        ElmValue.ElmChar char ->
            "'" ++ String.fromChar char ++ "'"

        ElmValue.ElmNumber float ->
            String.fromFloat float

        ElmValue.ElmBool True ->
            "True"

        ElmValue.ElmBool False ->
            "False"

        ElmValue.ElmFunction ->
            "\"<function>\""

        ElmValue.ElmInternals ->
            "\"<internals>\""

        ElmValue.ElmUnit ->
            "()"

        ElmValue.ElmFile str ->
            "\"<file " ++ str ++ ">\""

        ElmValue.ElmBytes int ->
            "\"<bytes " ++ String.fromInt int ++ ">\""


sequenceTypeToString : ElmValue.SequenceType -> List String -> String
sequenceTypeToString sequenceType values =
    case sequenceType of
        ElmValue.SeqSet ->
            if List.isEmpty values then
                "Set.empty"

            else
                values
                    |> join "(Set.fromList [" ", " "])"

        ElmValue.SeqList ->
            if List.isEmpty values then
                "[]"

            else
                values
                    |> join "[" ", " "]"

        ElmValue.SeqArray ->
            if List.isEmpty values then
                "Array.empty"

            else
                values
                    |> join "(Array.fromList [" ", " "])"

        ElmValue.SeqTuple ->
            if List.isEmpty values then
                "()"

            else
                values
                    |> join "(" ", " ")"


join : String -> String -> String -> List String -> String
join prefix separator suffix items =
    prefix ++ String.join separator items ++ suffix
