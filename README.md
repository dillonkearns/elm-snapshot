# elm-snapshot

Snapshot testing for elm-pages scripts - capture output once, verify it forever.

## What is Snapshot Testing?

Snapshot testing (also called approval testing) captures your code's output and saves it as a "golden master." On subsequent runs, the output is compared against this approved snapshot. If they match, the test passes. If they differ, you review the change and either fix your code or approve the new output.

This approach is particularly powerful for:

- **Complex output** - Testing formatters, encoders, or views where manually writing expectations is tedious
- **Legacy code** - Capturing existing behavior before refactoring
- **Visual review** - When it's easier to look at output than to specify it programmatically

## When to Use Snapshot Testing

**Good fit:** Formatters, encoders, code generators, complex transformations, characterizing legacy code

**Consider alternatives:** Simple pure functions (elm-test), behavior testing, frequently-changing output

## Design Goals

- **Elm-native** - Built on elm-pages, feels natural to Elm developers
- **Deterministic** - JSON keys sorted alphabetically, scrubbers for timestamps/GUIDs
- **Reviewable** - Human-readable snapshots, clear diffs on failure
- **Minimal** - Small API surface, sensible defaults

## Installation

```bash
elm install dillonkearns/elm-snapshot
```

**Prerequisite:** This package requires [elm-pages](https://elm-pages.com/) v10+. Snapshot tests run as elm-pages scripts.

## About elm-pages Scripts

Snapshot tests run as [elm-pages scripts](https://elm-pages.com/docs/elm-pages-scripts) - standalone Elm programs that can perform IO (file operations, HTTP, etc.).

**Why elm-pages?** Standard Elm test frameworks cannot write to the filesystem. elm-pages scripts bridge this gap.

**New to elm-pages?**
- [elm-pages Scripts Guide](https://elm-pages.com/docs/elm-pages-scripts)
- [elm-pages Package Docs](https://package.elm-lang.org/packages/dillonkearns/elm-pages/latest/)

You do NOT need a full elm-pages app - just the script runner (`npm install elm-pages`).

## Setting Up a New Project

1. **Create directory structure:**
   ```bash
   mkdir -p snapshot-tests/src
   cd snapshot-tests
   ```

2. **Initialize npm and install elm-pages:**
   ```bash
   npm init -y
   npm install elm-pages
   ```

3. **Copy the minimal example** as your starting point:
   ```bash
   cp -r path/to/elm-snapshot/examples/minimal/snapshot-tests .
   ```
   Adjust `source-directories` in elm.json to include your project's src.

4. **Add npm scripts** to your `package.json`:
   ```json
   "scripts": {
     "test": "elm-pages run snapshot-tests/src/Snapshots.elm",
     "test:approve": "elm-pages run snapshot-tests/src/Snapshots.elm --approve"
   }
   ```

See `examples/minimal/` for a complete minimal setup, or `examples/log-formatter/` for a fuller example with scrubbers and test grouping.

## Quick Start

Create a snapshot test script:

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
        , Snapshot.json 2 "user data" <|
            \() -> User.encode alice
        ]

greet : String -> String
greet name =
    "Hello, " ++ name ++ "!"
```

Run the tests:

```bash
elm-pages run snapshot-tests/src/Snapshots.elm
```

On first run, tests will fail because no approved snapshots exist. Review the output and approve:

```bash
elm-pages run snapshot-tests/src/Snapshots.elm --approve
```

Snapshots are saved to `snapshots/Snapshots/`. Commit these `.approved` files to source control.

## Examples

### String Output (Most Common)

```elm
Snapshot.test "format date" <|
    \() -> Date.toHumanString myDate
```

### JSON with Sorted Keys

```elm
Snapshot.json 2 "api response" <|
    \() -> Response.encode response
```

Keys are sorted alphabetically at all nesting levels for deterministic output.

### Scrubbing Timestamps and GUIDs

```elm
import Snapshot.Scrubber as Scrubber

Snapshot.test "log entry" (\() -> formatLog entry)
    |> Snapshot.withScrubbers [ Scrubber.timestamp ]

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

Use `taskTest`, `taskJson`, or `taskExpect` when your test needs file IO, HTTP requests, or other effects.

### Custom Printers

```elm
import Snapshot.Printer as Printer

myXmlPrinter : Printer.Printer MyXml
myXmlPrinter xml =
    Xml.toString xml

Snapshot.expect myXmlPrinter "config xml" <|
    \() -> buildConfig options
```

## CLI Options

```bash
# Run all tests
elm-pages run Snapshots.elm

# Approve all new/changed snapshots
elm-pages run Snapshots.elm --approve

# Approve a specific test
elm-pages run Snapshots.elm --approve-only "test name"

# CI mode (compact output, strict)
elm-pages run Snapshots.elm --ci

# List all test names
elm-pages run Snapshots.elm --list

# Remove obsolete snapshots
elm-pages run Snapshots.elm --prune

# Open diff tool for failures
elm-pages run Snapshots.elm --reporter=code      # VS Code
elm-pages run Snapshots.elm --reporter=opendiff  # macOS FileMerge
elm-pages run Snapshots.elm --reporter=meld      # Meld
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
│       ├── greeting.approved
│       └── user_data.approved
└── elm.json                  # Main project
```

The `snapshots/` directory contains:
- `.approved` files - The golden master (commit these)
- `.received` files - Actual output on failure (gitignore these)

## Running the Example

A complete working example is available in `examples/log-formatter/`:

```bash
cd examples/log-formatter
npm install
npm test
```

## Modules

- [`Snapshot`](https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/latest/Snapshot) - Core API for creating and running tests
- [`Snapshot.Printer`](https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/latest/Snapshot-Printer) - Convert values to strings
- [`Snapshot.Scrubber`](https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/latest/Snapshot-Scrubber) - Clean non-deterministic output

## Inspiration

This package is inspired by [ApprovalTests](https://approvaltests.com/) by Llewellyn Falco and the teachings of [Emily Bache](https://www.emilybache.com/). The core philosophy - "approve the output" - makes testing complex behavior straightforward and review-friendly.

## License

BSD-3-Clause
