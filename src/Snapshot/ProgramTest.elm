module Snapshot.ProgramTest exposing (view, model)

{-| High-level snapshot testing for `ProgramTest` programs.

    module Snapshots exposing (run)

    import Html exposing (Html, button, div, span, text)
    import Html.Events exposing (onClick)
    import Pages.Script exposing (Script)
    import ProgramTest exposing (ProgramTest)
    import Snapshot
    import Snapshot.ProgramTest
    import Test.Html.Selector as Selector

    startCounterApp : ProgramTest { count : Int } () ()
    startCounterApp =
        ProgramTest.createSandbox
            { init = { count = 0 }
            , update = \() model -> { model | count = model.count + 1 }
            , view =
                \model ->
                    div []
                        [ button [ onClick () ] [ text "++" ]
                        , span [] [ text (String.fromInt model.count) ]
                        ]
            }
            |> ProgramTest.start ()

    run : Script
    run =
        Snapshot.run "Snapshots"
            [ Snapshot.ProgramTest.view [] "initial view" <|
                \() -> startCounterApp
            , Snapshot.ProgramTest.view [ Selector.tag "span" ] "counter display" <|
                \() ->
                    startCounterApp
                        |> ProgramTest.clickButton "++"
            , Snapshot.ProgramTest.model Debug.toString "model after click" <|
                \() ->
                    startCounterApp
                        |> ProgramTest.clickButton "++"
            ]

After running `elm-pages run src/Snapshots.elm --approve=all`, you will have these files on disk:

`initial_view.approved.html`:

    <div>
        <button>++</button>
        <span>0</span>
    </div>

`counter_display.approved.html`:

    <span>1</span>

`model_after_click.approved.elm`:

    { count = 1 }

@docs view, model

-}

import Expect
import ProgramTest exposing (ProgramTest)
import Snapshot
import Snapshot.Printer as Printer
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner


{-| Create a snapshot test of the rendered view HTML from a `ProgramTest`.

The first argument is a list of selectors to scope the snapshot to a
subtree of the view. Pass an empty list to snapshot the entire view.

    Snapshot.ProgramTest.view [] "full view" <|
        \() -> myProgram

    Snapshot.ProgramTest.view [ Selector.tag "span" ] "counter display" <|
        \() ->
            myProgram
                |> ProgramTest.clickButton "++"

-}
view : List Selector.Selector -> String -> (() -> ProgramTest model msg effect) -> Snapshot.Test
view selectors name thunk =
    Snapshot.checkedCustom (Printer.string |> Printer.withExtension "html") name <|
        \() ->
            getViewHtml selectors (thunk ())


{-| Create a snapshot test of the model from a `ProgramTest`.

The model is pretty-printed as formatted Elm syntax using `Printer.elm`,
producing `.elm` snapshot files with proper syntax highlighting.

The first argument must be `Debug.toString` (published packages cannot
call it directly, so you must pass it in).

    Snapshot.ProgramTest.model Debug.toString "model state" <|
        \() ->
            myProgram
                |> ProgramTest.clickButton "++"

-}
model : (a -> String) -> String -> (() -> ProgramTest a msg effect) -> Snapshot.Test
model debugToString name thunk =
    Snapshot.checkedCustom (Printer.elm identity) name <|
        \() ->
            getModelResult debugToString (thunk ())


getViewHtml : List Selector.Selector -> ProgramTest model msg effect -> Result String String
getViewHtml selectors programTest =
    case selectors of
        [] ->
            getFullViewHtml "elm-snapshot ProgramTest.view is trying to force a failure %%" programTest

        _ ->
            getScopedViewHtml "elm-snapshot ProgramTest.view is trying to force a failure %%" selectors programTest


getFullViewHtml : String -> ProgramTest model msg effect -> Result String String
getFullViewHtml marker programTest =
    let
        expectation =
            programTest
                |> ProgramTest.expectViewHas [ Selector.text marker ]
    in
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            -- The impossible text actually matched! Retry with longer marker.
            getFullViewHtml (marker ++ "_") programTest

        Just reason ->
            case extractHtmlFromDescription reason.description of
                Just html ->
                    Ok html

                Nothing ->
                    -- The failure wasn't from our forced Query.has --
                    -- the ProgramTest was already in a failed state.
                    Err reason.description


getScopedViewHtml : String -> List Selector.Selector -> ProgramTest model msg effect -> Result String String
getScopedViewHtml marker selectors programTest =
    let
        expectation =
            programTest
                |> ProgramTest.within
                    (Query.find selectors)
                    (ProgramTest.ensureViewHas [ Selector.text marker ])
                |> ProgramTest.done
    in
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            -- The impossible text actually matched! Retry with longer marker.
            getScopedViewHtml (marker ++ "_") selectors programTest

        Just reason ->
            case extractScopedHtmlFromDescription reason.description of
                Just html ->
                    Ok html

                Nothing ->
                    -- The failure wasn't from our forced Query.has --
                    -- the ProgramTest was already in a failed state.
                    Err reason.description


getModelResult : (model -> String) -> ProgramTest model msg effect -> Result String String
getModelResult debugToString programTest =
    let
        marker =
            "elm-snapshot-model-extract:"

        expectation =
            programTest
                |> ProgramTest.expectModel
                    (\m ->
                        Expect.fail (marker ++ debugToString m)
                    )
    in
    case Test.Runner.getFailureReason expectation of
        Nothing ->
            -- This shouldn't happen since we always Expect.fail
            Err "Unexpected: forced failure did not produce a failure"

        Just reason ->
            let
                fullMarker =
                    "expectModel:\n" ++ marker
            in
            if String.startsWith fullMarker reason.description then
                Ok (String.dropLeft (String.length fullMarker) reason.description)

            else
                -- The ProgramTest was already in a failed state
                Err reason.description


extractScopedHtmlFromDescription : String -> Maybe String
extractScopedHtmlFromDescription description =
    let
        findMarkerPrefix =
            "▼ Query.find"
    in
    case firstIndexOf findMarkerPrefix description of
        Just findIndex ->
            let
                afterFind =
                    String.dropLeft findIndex description
            in
            case firstIndexOf "\n\n" afterFind of
                Just nlIndex ->
                    let
                        content =
                            String.dropLeft (nlIndex + 2) afterFind
                    in
                    case firstIndexOf "\n\n\n▼" content of
                        Just endIndex ->
                            Just (stripNumberedResult (String.left endIndex content))

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


{-| Strip the numbered prefix from Query.find results.

The format is:

        1)  <span>
                content
            </span>

The first line has "    N)  " prefix (4 spaces + number + ")  ").
Subsequent lines are indented by 8 spaces.

-}
stripNumberedResult : String -> String
stripNumberedResult text =
    case String.lines text of
        firstLine :: rest ->
            let
                strippedFirst =
                    case firstIndexOf ")  " firstLine of
                        Just i ->
                            String.dropLeft (i + 3) firstLine

                        Nothing ->
                            String.trimLeft firstLine
            in
            String.join "\n" (strippedFirst :: List.map (deindentLine 8) rest)

        [] ->
            text


deindentLine : Int -> String -> String
deindentLine n line =
    let
        prefix =
            String.repeat n " "
    in
    if String.startsWith prefix line then
        String.dropLeft n line

    else
        line


extractHtmlFromDescription : String -> Maybe String
extractHtmlFromDescription description =
    let
        startMarker =
            "▼ Query.fromHtml\n\n"

        endMarker =
            "\n\n\n▼"
    in
    case firstIndexOf startMarker description of
        Just startIndex ->
            let
                htmlStart =
                    startIndex + String.length startMarker

                remaining =
                    String.dropLeft htmlStart description
            in
            case firstIndexOf endMarker remaining of
                Just endIndex ->
                    Just (deindent 4 (String.left endIndex remaining))

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


firstIndexOf : String -> String -> Maybe Int
firstIndexOf needle haystack =
    case String.indexes needle haystack of
        i :: _ ->
            Just i

        [] ->
            Nothing


deindent : Int -> String -> String
deindent n text =
    text
        |> String.lines
        |> List.map (deindentLine n)
        |> String.join "\n"
