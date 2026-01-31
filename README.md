# elm-snapshot

Snapshot testing framework for Elm. Approve output once to save you from unexpected changes.

![Failing tests showing diff](https://github.com/dillonkearns/elm-snapshot/blob/main/documentation/images/failing-tests.png?raw=true)


## How It Works

1. **Run your test** - elm-snapshot captures the output
2. **Review the result** - approve it as your expected "golden master"
3. **Future runs compare** - if output changes, you see a diff and decide: fix the code or approve the new output

This approach captures human judgment about what correct output looks like. Instead of writing assertions by hand, you verify actual output once and let the tool enforce it forever.

## Quick Start

Run the init script to create a self-contained `snapshot-tests/` folder

```bash
npx elm-pages run github:dillonkearns/elm-snapshot:script/src/Init.elm
cd snapshot-tests
npm install
```

Then run your tests:

```bash
npm test              # First run will fail (no approved snapshots yet)
npm run test:approve  # Approve the snapshots, then check them in to git
npm test              # Tests pass now
```

Edit `snapshot-tests/src/Snapshots.elm` to add your tests.

## Examples

- **[minimal](https://github.com/dillonkearns/elm-snapshot/tree/main/examples/minimal)** - Bare-bones setup showing the simplest possible snapshot test
- **[log-formatter](https://github.com/dillonkearns/elm-snapshot/tree/main/examples/log-formatter)** - Fuller example demonstrating scrubbers, test grouping with `describe`, and multiple test files
- **[features](https://github.com/dillonkearns/elm-snapshot/tree/main/examples/features)** - Advanced example using `Printer.elm` to snapshot complex Elm data structures

## About elm-pages Scripts

**Prerequisite:** This project is an Elm package which gives you an API similar to `elm-test` for defining test suites. Rather than running with `elm-test`, however, you run your snapshot test files with the [`elm-pages`](https://elm-pages.com/) CLI via the `elm-pages run` command. Snapshot tests run as [elm-pages scripts](https://elm-pages.com/docs/elm-pages-scripts) - standalone Elm programs that can perform file operations, HTTP, etc.

**Why elm-pages?** Standard Elm test frameworks cannot write to the filesystem. elm-pages scripts bridge this gap.

**New to elm-pages?**
- [elm-pages Scripts Guide](https://elm-pages.com/docs/elm-pages-scripts)
- [elm-pages Package Docs](https://package.elm-lang.org/packages/dillonkearns/elm-pages/latest/)

You do NOT need a full elm-pages app - just the script runner (`npm install elm-pages`).


## Writing Tests

```elm
-- snapshot-tests/src/Snapshots.elm
module Snapshots exposing (run)

import Pages.Script exposing (Script)
import Snapshot

run : Script
run =
    Snapshot.run "Snapshots"
        [ Snapshot.test "greeting" <|
            \() -> greet "World"
        , Snapshot.json "user data" <|
            \() -> User.encode alice
        ]

greet : String -> String
greet name =
    "Hello, " ++ name ++ "!"
```

Snapshots are saved to `snapshots/Snapshots/`. Commit the `.approved` files to source control.

## Organizing Tests

As your test suite grows, split tests into modules that each expose a `List Snapshot.Test`:

```elm
-- snapshot-tests/src/Snapshots.elm (entry point)
module Snapshots exposing (run)

import Pages.Script exposing (Script)
import Snapshot
import Snapshots.Auth as Auth
import Snapshots.Api as Api

run : Script
run =
    Snapshot.run "Snapshots"
        (Auth.tests ++ Api.tests)
```

```elm
-- snapshot-tests/src/Snapshots/Auth.elm
module Snapshots.Auth exposing (tests)

import Snapshot

tests : List Snapshot.Test
tests =
    [ Snapshot.describe "Auth"
        [ Snapshot.test "login success" <| \() -> ...
        , Snapshot.test "login failure" <| \() -> ...
        ]
    ]
```

This keeps a single entry point while organizing tests by domain. The `describe` blocks create subdirectories in the snapshots folder.

## Usage

### String Output (Most Common)

```elm
Snapshot.test "format date" <|
    \() -> Date.toHumanString myDate
```

### JSON with Sorted Keys

```elm
Snapshot.json "api response" <|
    \() -> Response.encode response
```

Keys are sorted alphabetically at all nesting levels for deterministic output.

### Scrubbing Timestamps and GUIDs

```elm
import Snapshot.Scrubber as Scrubber

Snapshot.test "log entry" (\() -> formatLog entry)
    |> Snapshot.withScrubbers [ Scrubber.timestamp ]

-- Add scrubbers to a test with non-deterministic output
Snapshot.test "user record" (\() -> formatUser user)
    |> Snapshot.withScrubbers [ Scrubber.guid, Scrubber.timestamp ]
```

Scrubbers replace non-deterministic values with stable placeholders:
- `2024-01-15T10:30:00Z` becomes `[TIMESTAMP]`
- `550e8400-e29b-41d4-a716-446655440000` becomes `[GUID-1]`

Multiple occurrences of the same GUID get the same placeholder (`[GUID-1]`), different GUIDs get different numbers (`[GUID-2]`).

### Grouping with `describe`

```elm
Snapshot.describe "Date formatting"
    [ Snapshot.test "ISO format" <|
        \() -> Date.toIsoString date
    , Snapshot.test "human readable" <|
        \() -> Date.toHumanString date
    ]
```

Grouped tests create subdirectories: `snapshots/Snapshots/Date_formatting/ISO_format.approved`

### BackendTask for IO

```elm
import BackendTask
import BackendTask.File as File

Snapshot.taskTest "config file contents" <|
    File.rawFile "config.json"
        |> BackendTask.allowFatal
```

Use `taskTest`, `taskJson`, or `taskCustom` when your test needs file IO, HTTP requests, or other effects.

### Custom Printers

```elm
import Snapshot.Printer as Printer

xmlPrinter : Printer.Printer String
xmlPrinter =
    Printer.string
        |> Printer.withExtension "xml"

Snapshot.custom xmlPrinter "config xml" <|
    \() -> Xml.toString (buildConfig options)
```

## CLI Options

```bash
# Run all tests
elm-pages run src/Snapshots.elm

# Approve all new/changed snapshots
elm-pages run src/Snapshots.elm --approve

# Interactive per-snapshot approval
elm-pages run src/Snapshots.elm --approve=prompt

# Approve a specific test
elm-pages run src/Snapshots.elm --approve-only "test name"

# CI mode (compact output, strict)
elm-pages run src/Snapshots.elm --ci

# List all test names
elm-pages run src/Snapshots.elm --list

# Remove obsolete snapshots
elm-pages run src/Snapshots.elm --prune

# Open diff tool for failures
elm-pages run src/Snapshots.elm --reporter=code      # VS Code
elm-pages run src/Snapshots.elm --reporter=opendiff  # macOS FileMerge
elm-pages run src/Snapshots.elm --reporter=meld      # Meld
elm-pages run src/Snapshots.elm --reporter=ksdiff    # Kaleidoscope
elm-pages run src/Snapshots.elm --reporter=kdiff3    # KDiff3
elm-pages run src/Snapshots.elm --reporter=diff      # Unix diff
```

## File Structure

Typical project layout with snapshot tests:

```
my-project/
├── src/
│   └── MyModule.elm
├── snapshot-tests/
│   ├── elm.json              # elm-pages script project
│   └── src/
│       └── Snapshots.elm     # Your snapshot tests
├── snapshots/                # Generated (commit to git)
│   └── Snapshots/
│       ├── greeting.approved.txt
│       └── user_data.approved.json
└── elm.json                  # Main project
```

The `snapshots/` directory contains:
- `.approved.<ext>` files - The golden master (commit these)
- `.received.<ext>` files - Actual output on failure (gitignore these)

## Modules

- [`Snapshot`](https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/1.0.0/Snapshot/) - Core API for creating and running tests
- [`Snapshot.Printer`](https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/1.0.0/Snapshot-Printer/) - Convert values to strings
- [`Snapshot.Scrubber`](https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/1.0.0/Snapshot-Scrubber/) - Clean non-deterministic output

## Inspiration

This package is inspired by [ApprovalTests](https://approvaltests.com/) by Llewellyn Falco and the teachings of [Emily Bache](https://www.emilybache.com/). The core philosophy - "approve the output" - makes testing complex behavior straightforward and review-friendly.

## License

BSD-3-Clause
