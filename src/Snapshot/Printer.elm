module Snapshot.Printer exposing
    ( Printer
    , string, json, elm
    , withExtension
    )

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


{-| Pretty-prints any Elm value using elm-format style. This requires
`Debug.toString` to be passed as the first argument.

Since published packages cannot use `Debug.toString` directly, you must pass
it in as the first argument:

    Snapshot.custom (Printer.elm Debug.toString) "user model" <|
        \() -> model.user

A common pattern is to create a helper in your test file:

    elmPrinter : Printer a
    elmPrinter =
        Printer.elm Debug.toString

    -- Then use it in your tests:
    Snapshot.custom elmPrinter "user model" <|
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
            let
                -- First pass: convert to string (no line breaks)
                singleLine =
                    elmValueToStringSingleLine Set.empty parsed.value
            in
            -- Use elm-syntax-format for proper parenthesization
            case
                singleLine
                    |> ElmSyntaxParserLenient.run ElmSyntaxParserLenient.expression
                    |> Maybe.map
                        (\syntaxModule ->
                            syntaxModule.syntax
                                |> ElmSyntaxPrint.expressionNotParenthesized []
                                |> ElmSyntaxPrint.toString
                        )
            of
                Just formatted ->
                    -- Add line breaks for long lines
                    breakLongLines formatted

                Nothing ->
                    singleLine

        Err _ ->
            "-- Could not parse value, but here it is:\n" ++ valueAsString


{-| Add line breaks to formatted output when lines exceed 80 characters.
-}
breakLongLines : String -> String
breakLongLines input =
    if String.length input <= 80 then
        input

    else
        breakAtTopLevelCommas input


{-| Break at top-level record field commas only (not inside nested structures).
-}
breakAtTopLevelCommas : String -> String
breakAtTopLevelCommas input =
    let
        -- Find the position of the opening brace
        chars =
            String.toList input

        result =
            breakHelper chars 0 0 []
    in
    result


{-| Helper to break at commas only when at depth 1 (inside outer record).
-}
breakHelper : List Char -> Int -> Int -> List Char -> String
breakHelper chars depth braceDepth acc =
    case chars of
        [] ->
            String.fromList (List.reverse acc)

        '{' :: rest ->
            breakHelper rest depth (braceDepth + 1) ('{' :: acc)

        ' ' :: '}' :: rest ->
            if braceDepth == 1 then
                -- Closing brace of outer record - add newline before (trim trailing space)
                breakHelper rest depth 0 ('}' :: ' ' :: ' ' :: ' ' :: ' ' :: '\n' :: acc)

            else
                breakHelper rest depth (braceDepth - 1) ('}' :: ' ' :: acc)

        '}' :: rest ->
            if braceDepth == 1 then
                -- Closing brace of outer record - add newline before
                breakHelper rest depth 0 ('}' :: ' ' :: ' ' :: ' ' :: ' ' :: '\n' :: acc)

            else
                breakHelper rest depth (braceDepth - 1) ('}' :: acc)

        '[' :: rest ->
            breakHelper rest (depth + 1) braceDepth ('[' :: acc)

        ']' :: rest ->
            breakHelper rest (depth - 1) braceDepth (']' :: acc)

        '(' :: rest ->
            breakHelper rest (depth + 1) braceDepth ('(' :: acc)

        ')' :: rest ->
            breakHelper rest (depth - 1) braceDepth (')' :: acc)

        ',' :: ' ' :: rest ->
            if braceDepth == 1 && depth == 0 then
                -- Top-level comma in record - add newline
                breakHelper rest depth braceDepth (' ' :: ',' :: ' ' :: ' ' :: ' ' :: ' ' :: '\n' :: acc)

            else
                breakHelper rest depth braceDepth (' ' :: ',' :: acc)

        c :: rest ->
            breakHelper rest depth braceDepth (c :: acc)


{-| Convert ElmValue to a single-line string (for elm-syntax-format processing).
-}
elmValueToStringSingleLine : Set String -> ElmValue -> String
elmValueToStringSingleLine fieldsToSkip elmValue =
    case elmValue of
        ElmValue.Plain plainValue ->
            plainValueToString plainValue

        ElmValue.Expandable _ (ElmValue.ElmSequence sequenceType values) ->
            sequenceTypeToStringSingleLine sequenceType (List.map (elmValueToStringSingleLine fieldsToSkip) values)

        ElmValue.Expandable _ (ElmValue.ElmType typeName values) ->
            case values of
                [] ->
                    typeName

                _ ->
                    "(" ++ typeName ++ " " ++ String.join " " (List.map (elmValueToStringSingleLine fieldsToSkip) values) ++ ")"

        ElmValue.Expandable _ (ElmValue.ElmRecord values) ->
            let
                fields =
                    List.filterMap
                        (\( field, fieldValue ) ->
                            if Set.member field fieldsToSkip then
                                Nothing

                            else
                                Just (field ++ " = " ++ elmValueToStringSingleLine fieldsToSkip fieldValue)
                        )
                        values
            in
            "{ " ++ String.join ", " fields ++ " }"

        ElmValue.Expandable _ (ElmValue.ElmDict values) ->
            if List.isEmpty values then
                "Dict.empty"

            else
                let
                    items =
                        List.map
                            (\( key, dictValue ) ->
                                "( " ++ elmValueToStringSingleLine fieldsToSkip key ++ ", " ++ elmValueToStringSingleLine fieldsToSkip dictValue ++ " )"
                            )
                            values
                in
                "(Dict.fromList [ " ++ String.join ", " items ++ " ])"


sequenceTypeToStringSingleLine : ElmValue.SequenceType -> List String -> String
sequenceTypeToStringSingleLine sequenceType values =
    case sequenceType of
        ElmValue.SeqSet ->
            if List.isEmpty values then
                "Set.empty"

            else
                "(Set.fromList [ " ++ String.join ", " values ++ " ])"

        ElmValue.SeqList ->
            if List.isEmpty values then
                "[]"

            else
                "[ " ++ String.join ", " values ++ " ]"

        ElmValue.SeqArray ->
            if List.isEmpty values then
                "Array.empty"

            else
                "(Array.fromList [ " ++ String.join ", " values ++ " ])"

        ElmValue.SeqTuple ->
            if List.isEmpty values then
                "()"

            else
                "( " ++ String.join ", " values ++ " )"


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
            "\"<bytes " ++ String.fromInt int ++ "\""
