module Init exposing (run)

{-| Initialize elm-snapshot in an existing project.

This script creates the snapshot-tests folder with minimal setup:

  - snapshot-tests/package.json
  - snapshot-tests/elm.json
  - snapshot-tests/src/Snapshots.elm
  - snapshot-tests/.gitignore
  - snapshots/.gitignore

Run with:

    npx elm-pages run github:dillonkearns/elm-snapshot:script/src/Init.elm

-}

import BackendTask
import Pages.Script as Script exposing (Script)


run : Script
run =
    Script.withoutCliOptions
        (Script.log "Initializing elm-snapshot..."
            |> Script.doThen
                (Script.writeFile
                    { path = "snapshot-tests/package.json"
                    , body = snapshotTestsPackageJson
                    }
                    |> BackendTask.allowFatal
                )
            |> Script.doThen (Script.log "  Created snapshot-tests/package.json")
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
                    { path = "snapshot-tests/.gitignore"
                    , body = snapshotTestsGitignore
                    }
                    |> BackendTask.allowFatal
                )
            |> Script.doThen (Script.log "  Created snapshot-tests/.gitignore")
            |> Script.doThen
                (Script.writeFile
                    { path = "snapshots/.gitignore"
                    , body = snapshotsGitignore
                    }
                    |> BackendTask.allowFatal
                )
            |> Script.doThen (Script.log "  Created snapshots/.gitignore")
            |> Script.doThen
                (Script.log
                    """
Done! Created snapshot-tests/ folder.

Next steps:

  1. Install dependencies:
     cd snapshot-tests && npm install

  2. Edit snapshot-tests/src/Snapshots.elm to import your modules and add tests.

  3. Run tests:
     npm test              # First run will fail (no approved snapshots)
     npm run test:approve  # Approve the snapshots
     npm test              # Tests should pass now
"""
                )
        )


snapshotTestsPackageJson : String
snapshotTestsPackageJson =
    """{
  "name": "snapshot-tests",
  "private": true,
  "scripts": {
    "test": "elm-pages run src/Snapshots.elm",
    "test:approve": "elm-pages run src/Snapshots.elm --approve"
  },
  "devDependencies": {
    "elm-pages": "^10.2.2"
  }
}
"""


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
            "kraklin/elm-debug-parser": "2.0.0",
            "lue-bird/elm-syntax-format": "1.1.0",
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


snapshotTestsGitignore : String
snapshotTestsGitignore =
    """# Build artifacts
elm-stuff/
node_modules/
.elm-pages/
"""


snapshotsGitignore : String
snapshotsGitignore =
    """# Ignore .received files - these are generated on test failure
# and should not be committed to source control.
# Only .approved files should be committed.
*.received.*
"""


snapshotsElm : String
snapshotsElm =
    """module Snapshots exposing (run)

{-| Snapshot tests for your project.

Run with:

    npm test

-}

import Pages.Script exposing (Script)
import Snapshot


run : Script
run =
    Snapshot.run "Snapshots"
        [ Snapshot.todo "my first snapshot test"
        ]
"""
