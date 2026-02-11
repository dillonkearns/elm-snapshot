module CounterApp exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int
    }


init : Model
init =
    { count = 0 }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }


view : Model -> Html Msg
view model =
    div [ class "counter" ]
        [ button [ onClick Decrement ] [ text "--" ]
        , span [] [ text (String.fromInt model.count) ]
        , button [ onClick Increment ] [ text "++" ]
        ]
