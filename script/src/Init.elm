module Init exposing (run)

{-| Initialize a new elm-snapshot project.

This script creates the minimal setup for snapshot testing:

  - snapshot-tests/elm.json
  - snapshot-tests/src/Snapshots.elm
  - src/Example.elm (sample module)
  - package.json with test scripts

Run with:

    npx elm-pages run github:dillonkearns/elm-snapshot:script/src/Init.elm

-}

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions
        (Script.log "Initializing elm-snapshot project..."
            |> Script.doThen
                (Script.writeFile
                    { path = "snapshot-tests/elm.json"
                    , body = snapshotTestsElmJson
                    }
                    |> BackendTask.allowFatal
                )
            |> Script.doThen (Script.log "  Created snapshot-tests/elm.json")
            |> Script.doThen
                (Script.writeFile
                    { path = "snapshot-tests/src/Snapshots.elm"
                    , body = snapshotsElm
                    }
                    |> BackendTask.allowFatal
                )
            |> Script.doThen (Script.log "  Created snapshot-tests/src/Snapshots.elm")
            |> Script.doThen
                (Script.writeFile
                    { path = "src/Example.elm"
                    , body = exampleElm
                    }
                    |> BackendTask.allowFatal
                )
            |> Script.doThen (Script.log "  Created src/Example.elm")
            |> Script.doThen
                (Script.writeFile
                    { path = "package.json"
                    , body = packageJson
                    }
                    |> BackendTask.allowFatal
                )
            |> Script.doThen (Script.log "  Created package.json")
            |> Script.doThen
                (Script.log
                    """
Done! elm-snapshot initialized.

Next steps:
  1. npm install
  2. npm test         # Run tests (first run will fail - no approved snapshots yet)
  3. npm run test:approve  # Approve the snapshots
  4. npm test         # Tests should pass now

Edit src/Example.elm and snapshot-tests/src/Snapshots.elm to add your own tests.
"""
                )
        )


snapshotTestsElmJson : String
snapshotTestsElmJson =
    """{
    "type": "application",
    "source-directories": [
        "../src",
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "dillonkearns/elm-cli-options-parser": "3.2.0",
            "dillonkearns/elm-pages": "10.2.2",
            "dillonkearns/elm-snapshot": "1.0.0",
            "elm/core": "1.0.5",
            "elm/json": "1.1.4",
            "elm/regex": "1.0.0",
            "miniBill/elm-diff": "1.1.0"
        },
        "indirect": {
            "Chadtech/elm-bool-extra": "2.4.2",
            "avh4/elm-color": "1.0.0",
            "danfishgold/base64-bytes": "1.1.0",
            "danyx23/elm-mimetype": "4.0.1",
            "dillonkearns/elm-bcp47-language-tag": "2.0.0",
            "dillonkearns/elm-date-or-date-time": "2.0.0",
            "dillonkearns/elm-form": "3.0.1",
            "elm/browser": "1.0.2",
            "elm/bytes": "1.0.8",
            "elm/file": "1.0.5",
            "elm/html": "1.0.1",
            "elm/http": "2.0.0",
            "elm/parser": "1.1.0",
            "elm/random": "1.0.0",
            "elm/time": "1.0.0",
            "elm/url": "1.0.0",
            "elm/virtual-dom": "1.0.5",
            "elm-community/basics-extra": "4.1.0",
            "elm-community/list-extra": "8.7.0",
            "elm-community/maybe-extra": "5.3.0",
            "elmcraft/core-extra": "2.2.0",
            "fredcy/elm-parseint": "2.0.1",
            "jluckyiv/elm-utc-date-strings": "1.0.0",
            "justinmimbs/date": "4.1.0",
            "mdgriffith/elm-codegen": "5.2.0",
            "miniBill/elm-codec": "2.3.0",
            "miniBill/elm-unicode": "1.1.1",
            "noahzgordon/elm-color-extra": "1.0.2",
            "robinheghan/fnv1a": "1.0.0",
            "robinheghan/murmur3": "1.0.0",
            "rtfeldman/elm-css": "18.0.0",
            "rtfeldman/elm-hex": "1.0.0",
            "rtfeldman/elm-iso8601-date-strings": "1.1.4",
            "stil4m/elm-syntax": "7.3.9",
            "stil4m/structured-writer": "1.0.3",
            "the-sett/elm-pretty-printer": "3.3.0",
            "the-sett/elm-syntax-dsl": "6.0.3",
            "wolfadex/elm-ansi": "3.0.1"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {}
    }
}
"""


snapshotsElm : String
snapshotsElm =
    """module Snapshots exposing (run)

{-| Snapshot tests for your project.

Run with:

    npm test

-}

import Example
import Pages.Script exposing (Script)
import Snapshot


run : Script
run =
    Snapshot.run "Snapshots"
        [ Snapshot.test "greeting" <|
            \\() -> Example.greet "World"
        , Snapshot.test "greeting with name" <|
            \\() -> Example.greet "Elm"
        ]
"""


exampleElm : String
exampleElm =
    """module Example exposing (greet)

{-| An example module to demonstrate snapshot testing.
-}


{-| Create a greeting for a name.

    greet "World" == "Hello, World!"

-}
greet : String -> String
greet name =
    "Hello, " ++ name ++ "!"
"""


packageJson : String
packageJson =
    """{
  "name": "my-elm-project",
  "private": true,
  "scripts": {
    "test": "cd snapshot-tests && elm-pages run src/Snapshots.elm",
    "test:approve": "cd snapshot-tests && elm-pages run src/Snapshots.elm --approve"
  },
  "devDependencies": {
    "elm-pages": "^3.0.0"
  }
}
"""
