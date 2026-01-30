module LogEntry exposing (Level(..), LogEntry, decode, encode)

{-| A simple log entry type for demonstration.
-}

import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type Level
    = Info
    | Warn
    | Error


type alias LogEntry =
    { level : Level
    , message : String
    , timestamp : String
    , context : Maybe String
    }



-- ENCODING


encode : LogEntry -> Encode.Value
encode entry =
    Encode.object
        ([ ( "level", encodeLevel entry.level )
         , ( "message", Encode.string entry.message )
         , ( "timestamp", Encode.string entry.timestamp )
         ]
            ++ (case entry.context of
                    Just ctx ->
                        [ ( "context", Encode.string ctx ) ]

                    Nothing ->
                        []
               )
        )


encodeLevel : Level -> Encode.Value
encodeLevel level =
    Encode.string <|
        case level of
            Info ->
                "INFO"

            Warn ->
                "WARN"

            Error ->
                "ERROR"



-- DECODING


decode : Decoder LogEntry
decode =
    Decode.map4 LogEntry
        (Decode.field "level" decodeLevel)
        (Decode.field "message" Decode.string)
        (Decode.field "timestamp" Decode.string)
        (Decode.maybe (Decode.field "context" Decode.string))


decodeLevel : Decoder Level
decodeLevel =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "INFO" ->
                        Decode.succeed Info

                    "WARN" ->
                        Decode.succeed Warn

                    "ERROR" ->
                        Decode.succeed Error

                    _ ->
                        Decode.fail ("Unknown level: " ++ str)
            )
