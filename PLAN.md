# Elm Snapshot Test - Design Plan

## Vision

An Elm-native approval testing tool built on elm-pages, inspired by [Llewellyn Falco's ApprovalTests](https://approvaltests.com/) and [Emily Bache's teachings](https://leanpub.com/approval-testing-by-example).

**Core philosophy**: Leverage Elm's purity and elm-pages' BackendTask to create a snapshot testing experience that feels native to Elm while providing the powerful "approve the output" workflow that makes approval testing so effective for both legacy code and new development.

---

## The Approval Testing Model

### The Four Steps (Emily Bache)

1. **Arrange** - Set up test inputs
2. **Act** - Execute the code under test
3. **Print** - Convert result to a string representation
4. **Verify** - Compare against approved output

Every approval test has a **Printer**. Even if it's just identity, the printer is what converts your result to a diffable string.

### Qualities of Good Approval Tests

From Emily Bache's workshop:

- **One failure = one problem** - It's okay if one problem causes multiple failures, but each failure should point to one issue
- **Aggregate information** - Approval tests shine when capturing complex output, not single simple values
- **Self-explanatory output** - The snapshot alone should explain what the test is checking
- **Include inputs for context** - Even if you don't assert on them, showing inputs in the output helps understanding

### When to Use Approval Tests

**Good cues:**
- Too many assertions (more than 2)
- Complex objects that are tedious to assert field-by-field
- Output that's easier to review visually than to specify programmatically
- Legacy code where you want to capture existing behavior

**The "Drawer" concept:** Find a unit of logic (a "drawer"), feed in various inputs (perhaps using combinations), create a printer that shows relevant outputs, then verify all results. From there, refactor with confidence.

---

## Key Design Decisions

### 1. elm-pages as Foundation

Use elm-pages Script as the meta-framework. This gives us:

- **BackendTask** for file I/O (reading/writing snapshots) and command execution
- **Unified CLI** via `elm-pages run`
- **Elm-native experience** - no context switching to JS tooling
- **Future potential** for build-time analysis of test structure

### 2. Two Test Modes

| Mode | Use Case | API |
|------|----------|-----|
| **Pure** | Encoders, view functions, data transformations | `Snapshot.test`, `Snapshot.json`, `Snapshot.expect` |
| **BackendTask** | CLI output, generated files, external tool results | `Snapshot.taskTest`, `Snapshot.taskJson`, `Snapshot.taskExpect` |

Each mode has variants for string output (most common), JSON output (sorted keys), and custom printers. Scrubbers chain with `|> Snapshot.withScrubbers [...]`.

### 3. ApprovalTests-Style File Workflow

Following Falco's original design:

- `.approved` files = golden master (committed to source control)
- `.received` files = actual output on failure (gitignored)
- Approval = reviewing and renaming `.received` → `.approved`

**Workflow tip:** Use `--approve` to approve all, then use `git diff` to review what changed. This leverages git's excellent diff tooling.

This is more explicit than Vitest's single `.snap` file approach and aligns with the approval testing philosophy.

### 4. Diff Library

Use [miniBill/elm-diff](https://github.com/miniBill/elm-diff) for computing and displaying differences. Features:

- Wu's O(NP) algorithm (efficient)
- `Similar` type for near-matches (whitespace differences)
- `diffWith` for custom equality functions
- Well-maintained

### 5. No Watch Mode (v1)

Manual re-runs for the initial version. Watch mode can be added later or achieved externally with tools like `watchexec`.

### 6. Own Test Type (not elm-test)

We use our own `Snapshot.Test` type rather than integrating with elm-test's `Test` type.

**Rationale**: The value proposition is different:
- **elm-test**: "I know what the output should be, let me assert it"
- **snapshot testing**: "I don't want to manually write the expected output, let me capture and approve it"

Users don't write expectations in snapshot tests - the approved file *is* the expectation. elm-test's `Expect` module doesn't add value here.

**Note**: elm-pages has a pattern for running elm-test via BackendTask (see `examples/end-to-end/script/src/BackendTaskTest.elm`):

```elm
testScript : String -> List (BackendTask FatalError Test.Test) -> Script
```

This allows IO before returning a `Test`. We could potentially offer a converter `Snapshot.toElmTest : Test -> BackendTask FatalError Test.Test` in the future if integration is desired, but it's not a priority.

### 7. Printers and Scrubbers

The output pipeline has two transformation stages:

```
Result → Printer → Formatted String → Scrubber → Final String → Compare
```

#### Printers

**Every test has a printer** - the function that converts your result to a string. In Elm, since users return a String directly, they're implicitly writing the printer. But we can provide **printer helpers** for common formats.

**Printer best practices (Emily Bache):**
- **Nicely diffable** - Use short lines for easy diff viewing
- **JSON can be tricky** - Long lines make diffs hard to read; prefer pretty-printing
- **Include context** - Show inputs in the output even if you don't assert on them
- **Change printers in separate commits** - Don't mix printer changes with logic changes

#### Scrubbers

**Scrubbers** replace non-deterministic values (timestamps, GUIDs) with placeholders *after* printing.

| Aspect | Printer | Scrubber |
|--------|---------|----------|
| **Purpose** | Format for readability | Remove non-determinism |
| **When applied** | First | Second |
| **Examples** | Pretty-print JSON, format tables | Replace timestamps, normalize GUIDs |

#### Scrubbers vs Custom Comparators

| Aspect | Scrubber | Custom Comparator |
|--------|----------|-------------------|
| **Transform** | Before storage | At comparison time |
| **`.approved` contains** | Normalized output | Raw output |
| **`.received` contains** | Normalized output | Raw output |
| **Diff display** | Meaningful (normalized vs normalized) | Noisy (shows irrelevant differences) |
| **Complexity** | Simple (`String -> String`) | Complex (custom equality + custom diff) |

**We prefer scrubbers** because:
- Snapshot files remain human-readable
- Standard string diff works correctly
- Simpler mental model
- Aligns with ApprovalTests philosophy

#### Common Use Cases

**Printers:**
1. **JSON pretty-print** - Consistent indentation, short lines
2. **Table formatting** - Align columns for readability
3. **Include inputs** - Show test inputs alongside outputs for context

**Scrubbers:**
1. **Timestamps/dates** - Replace with `[TIMESTAMP]` placeholder
2. **GUIDs/random IDs** - Replace with `[ID-1]`, `[ID-2]`, etc.
3. **File paths** - Normalize path separators, remove absolute prefixes
4. **Floating point** - Round to consistent precision

#### Type Contracts

Based on [ApprovalTests research](https://approvaltestscpp.readthedocs.io/en/latest/generated_docs/explanations/Scrubbers.html):

```elm
-- Printer: converts domain object to string (T → String)
type alias Printer a = a -> String

-- Scrubber: cleans string output (String → String)
-- "Fundamentally, a scrubber is a function that takes a string and returns a string"
type alias Scrubber = String -> String
```

#### Idiomatic Elm API

Rather than passing printer configuration objects (an OOP pattern), we use an idiomatic FP approach with specialized functions:

```elm
-- String output (most common)
Snapshot.test : String -> (() -> String) -> Test

-- JSON output with pretty printing (keys sorted alphabetically)
Snapshot.json : Int -> String -> (() -> Encode.Value) -> Test

-- Custom printer
Snapshot.expect : Printer a -> String -> (() -> a) -> Test

-- Add scrubbers to any test (pipeline modifier)
Snapshot.withScrubbers : List Scrubber -> Test -> Test

-- BackendTask versions
Snapshot.taskTest : String -> BackendTask FatalError String -> Test
Snapshot.taskJson : Int -> String -> BackendTask FatalError Encode.Value -> Test
Snapshot.taskExpect : Printer a -> String -> BackendTask FatalError a -> Test
```

**Why this design?**
- OOP frameworks use interfaces (`Printer<T>`) because languages like Java/C# don't have first-class functions
- In Elm, `Printer a = a -> String` is just a function type - no interface wrapper needed
- `Snapshot.json 2` is already a partially applied function - clean and composable
- Scrubbers chain naturally with `|> Snapshot.withScrubbers [...]`

#### Built-in Printers (Snapshot.Printer module)

```elm
-- Identity printer (for when you already have a String)
string : Printer String
string = identity

-- JSON pretty-printer with sorted keys and configurable indent
json : Int -> Printer Json.Encode.Value
json indent value = value |> sortJsonKeys |> Json.Encode.encode indent

-- JSON without key sorting (rare - use when insertion order matters)
jsonRaw : Int -> Printer Json.Encode.Value
```

#### Built-in Scrubbers (Snapshot.Scrubber module)

```elm
-- Replace regex matches with a fixed string
regex : String -> String -> Scrubber

-- Normalize GUIDs to [GUID-1], [GUID-2], etc. (preserves referential equality)
guid : Scrubber

-- Replace ISO timestamps with [TIMESTAMP]
timestamp : Scrubber

-- Compose multiple scrubbers (applied left to right)
all : List Scrubber -> Scrubber
```

---

## Testing "Real Stuff"

One powerful pattern from Emily Bache's workshop: use **real artifacts** as test inputs and outputs.

### Input as Real Stuff
- File structures
- Source code
- Configuration files
- Database dumps

### Output as Real Stuff
- Generated code
- API responses
- Command-line output
- File contents

### Why This Matters

When your output is "real stuff," you can **run additional checks**:
- **Compiler** - Does the generated code compile?
- **Linter** - Does it pass lint rules?
- **Formatter** - Is it properly formatted?
- **Execute it** - Does the generated code actually work?

This is where `Snapshot.taskTest` shines - you can generate code, then compile it, then snapshot the result.

---

## API Design

### User's Perspective

```elm
-- tests/Snapshots.elm
module Snapshots exposing (run)

import BackendTask exposing (BackendTask)
import Pages.Script exposing (Script)
import Snapshot exposing (Test)
import Snapshot.Scrubber as Scrubber


run : Script
run =
    Snapshot.run
        [ -- String output (most common)
          Snapshot.test "greeting" <|
            \() ->
                greet "World"

        -- JSON output with pretty printing
        , Snapshot.json 2 "User JSON encoding" <|
            \() ->
                User.encode { name = "Alice", age = 30 }

        -- String test with scrubber for timestamps
        , Snapshot.test "Log entry" (\() -> formatLogEntry entry)
            |> Snapshot.withScrubbers [ Scrubber.timestamp ]

        -- Including inputs in output for context
        , Snapshot.test "process order" <|
            \() ->
                let
                    order = { items = [...], customer = ... }
                    result = processOrder order
                in
                -- Show both input and output
                "Input:\n" ++ Order.toString order ++
                "\n\nOutput:\n" ++ Result.toString result

        -- BackendTask-powered test
        , Snapshot.taskTest "elm-review JSON output" <|
            BackendTask.Custom.run "elm-review"
                [ "--report=json" ]
                |> BackendTask.map .stdout

        -- Grouped tests (Phase 6)
        , Snapshot.describe "Date formatting"
            [ Snapshot.test "ISO format" <|
                \() -> Date.toIsoString sampleDate
            , Snapshot.test "human readable" <|
                \() -> Date.toHumanString sampleDate
            ]
        ]
```

### Module Structure

```
Snapshot
    run : List Test -> Script
    test : String -> (() -> String) -> Test
    json : Int -> String -> (() -> Encode.Value) -> Test
    expect : Printer a -> String -> (() -> a) -> Test
    taskTest : String -> BackendTask FatalError String -> Test
    taskJson : Int -> String -> BackendTask FatalError Encode.Value -> Test
    taskExpect : Printer a -> String -> BackendTask FatalError a -> Test
    withScrubbers : List Scrubber -> Test -> Test
    describe : String -> List Test -> Test  -- Phase 6

Snapshot.Printer
    type alias Printer a = a -> String
    string : Printer String                -- identity
    json : Int -> Printer Encode.Value     -- pretty-print with sorted keys
    jsonRaw : Int -> Printer Encode.Value  -- pretty-print without sorting

Snapshot.Scrubber
    type alias Scrubber = String -> String
    regex : String -> String -> Scrubber
    guid : Scrubber
    timestamp : Scrubber
    all : List Scrubber -> Scrubber
    custom : (String -> String) -> Scrubber
```

---

## File Structure

### Project Layout

```
my-elm-project/
├── src/
├── tests/
│   ├── Snapshots.elm              # Test definitions
│   └── snapshots/                 # Generated snapshot files
│       ├── User_JSON_encoding.approved
│       ├── User_JSON_encoding.received    # Only on failure
│       ├── elm-review_JSON_output.approved
│       └── Date_formatting/
│           ├── ISO_format.approved
│           └── human_readable.approved
├── elm.json
└── .gitignore                     # Include *.received
```

### Snapshot File Naming

- Test name → filename: spaces and special chars replaced with `_`
- `describe` blocks create subdirectories
- Extension: `.approved` / `.received` (plain text, no `.txt`)

---

## Failure Behavior

### Scenario 1: Test Passes

```
$ elm-pages run tests/Snapshots.elm

✓ User JSON encoding
✓ elm-review JSON output
✓ Date formatting / ISO format
✓ Date formatting / human readable

4 passing
```

### Scenario 2: New Test (No Approved File)

```
$ elm-pages run tests/Snapshots.elm

✗ User JSON encoding

  New snapshot (no .approved file found)

  Received:
  ┌────────────────────────────────
  │ {
  │   "name": "Alice",
  │   "age": 30
  │ }
  └────────────────────────────────

  To approve this snapshot:
    elm-pages run tests/Snapshots.elm --approve "User JSON encoding"

  Or manually:
    mv tests/snapshots/User_JSON_encoding.received \
       tests/snapshots/User_JSON_encoding.approved

1 failing
```

### Scenario 3: Snapshot Mismatch

```
$ elm-pages run tests/Snapshots.elm

✗ User JSON encoding

  Snapshot mismatch:

    {
  -   "name": "Alice",
  +   "name": "Bob",
      "age": 30
    }

  To approve this change:
    elm-pages run tests/Snapshots.elm --approve "User JSON encoding"

1 failing
```

### Scenario 4: CI Mode

```
$ elm-pages run tests/Snapshots.elm --ci

✗ User JSON encoding - snapshot mismatch
✗ Date formatting / ISO format - no approved file

2 failing
Exit code: 1
```

Minimal output, no approval instructions (can't approve in CI).

---

## CLI Interface

```bash
# Run all snapshot tests
elm-pages run tests/Snapshots.elm

# Approve all pending/changed snapshots
elm-pages run tests/Snapshots.elm --approve

# Approve specific test
elm-pages run tests/Snapshots.elm --approve-only "User JSON encoding"

# CI mode (strict, minimal output)
elm-pages run tests/Snapshots.elm --ci

# List all snapshots and their status
elm-pages run tests/Snapshots.elm --list
```

### Exit Codes

| Code | Meaning |
|------|---------|
| 0 | All tests pass |
| 1 | One or more tests failed |

---

## Implementation Phases

### Phase 1: Core Foundation ✅

**Goal**: Minimal working snapshot test runner

- [x] Set up elm-pages script structure
- [x] Implement `Snapshot.test` for pure Elm tests
- [x] File I/O: read `.approved`, write `.received`
- [x] Basic pass/fail logic
- [x] Simple terminal output (no diff yet)

**Deliverable**: Can run pure Elm snapshot tests with manual file comparison

### Phase 2: Diff Display ✅

**Goal**: Rich diff output in terminal

- [x] Integrate miniBill/elm-diff
- [x] ANSI color formatting for diffs
- [x] Line-by-line diff display
- [ ] Handle `Similar` cases (whitespace-only changes) - N/A with basic `diff`

**Deliverable**: Clear, colorful diff output on failures

### Phase 3: Approval Workflow ✅

**Goal**: Streamlined approval experience

- [x] `--approve` flag implementation
- [x] `--approve-only "test name"` for selective approval
- [x] `--ci` mode
- [x] Clear messaging for all scenarios

**Deliverable**: Full ApprovalTests-style workflow

### Phase 4: BackendTask Tests ✅

**Goal**: Support for IO-based tests

- [x] Implement `Snapshot.taskTest`
- [x] Handle BackendTask errors gracefully (uses FatalError)
- [ ] Timeout handling for long-running tasks (deferred - can use BackendTask.timeout)

**Deliverable**: Can snapshot CLI output, file contents, etc.

### Phase 5: Printers & Scrubbers ✅

**Goal**: Output transformation pipeline

- [x] Define `Printer` and `Scrubber` types
- [x] Implement idiomatic API: `Snapshot.json`, `Snapshot.expect`, `Snapshot.withScrubbers`
- [x] Implement BackendTask variants: `Snapshot.taskJson`, `Snapshot.taskExpect`
- [x] Built-in `Printer.json` (pretty-print with sorted keys)
- [x] Built-in `Printer.jsonRaw` (pretty-print without sorting)
- [x] Built-in `Printer.string` (identity)
- [x] Built-in `Scrubber.regex` for pattern replacement
- [x] Built-in `Scrubber.guid` for GUID normalization (preserves referential equality)
- [x] Built-in `Scrubber.timestamp` for ISO 8601 date/time normalization
- [x] Built-in `Scrubber.all` to compose multiple scrubbers

**API Evolution**: Refactored from OOP-style `testWith { printer = ... }` to idiomatic Elm functions (`Snapshot.json`, `Snapshot.expect`) with pipeline scrubber composition (`|> Snapshot.withScrubbers`).

**Deliverable**: Can handle JSON formatting, timestamps, etc.

### Phase 6: Polish & DX

**Goal**: Production-ready developer experience

- [ ] `describe` for grouping tests
- [ ] `--list` command
- [ ] Better error messages
- [ ] Documentation
- [ ] Example project

**Deliverable**: Ready for real-world use

---

## Future Considerations (Out of Scope for v1)

- **Watch mode**: Auto-rerun on file changes
- **Pairwise/combinatorial testing**: Generate test cases from input combinations (see [pairwise.org](http://www.pairwise.org/), ApprovalTests' `verifyBestCoveringPairs`)
- **Custom reporters**: HTML output, JSON for tooling integration
- **Inline snapshots**: Store expected value in test file itself
- **Snapshot serializers**: Custom formatting for specific types
- **Interactive approval**: TUI for reviewing changes one-by-one
- **IDE integration**: VS Code extension for approving from editor
- **elm-test integration**: `Snapshot.toElmTest : Test -> BackendTask FatalError Test.Test`

---

## Best Practices

From Emily Bache's workshop:

1. **Change printers in separate commits** - Don't mix printer changes with logic changes; it makes review confusing
2. **Approve all, then git diff** - Use `--approve` liberally, then review changes with git's diff tools
3. **Include inputs in output** - Even if you don't assert on them, they provide context
4. **Keep lines short** - Makes diffs much easier to read
5. **One failure = one problem** - Design printers so each test failure points to one issue
6. **Find the "drawer"** - Identify a unit of logic, feed it combinations of inputs, verify all outputs

---

## Open Questions

1. **Snapshot directory location**: Configurable or always `tests/snapshots/`?
2. **Large snapshots**: Should we truncate display for very large outputs?
3. **Binary snapshots**: Support for images, PDFs, etc. (probably out of scope)
4. **Parallel test execution**: Worth the complexity for v1?

---

## References

### Core Inspiration
- [ApprovalTests](https://approvaltests.com/) - Llewellyn Falco's original framework
- [Approval Testing by Example](https://leanpub.com/approval-testing-by-example) - Emily Bache's book
- [Emily Bache on Approval Testing (Medium)](https://medium.com/97-things/approval-testing-33946cde4aa8) - Concise overview
- [Llewellyn Falco: ApprovalTests Talk](https://www.youtube.com/watch?v=O1h9ho2G85Q&t=404s) - Derivation approach

### Printers & Scrubbers
- [ApprovalTests.cpp Scrubbing Guide](https://approvaltestscpp.readthedocs.io/en/latest/generated_docs/how_tos/ScrubNonDeterministicOutput.html) - Detailed scrubber patterns
- [ApprovalTests.cpp Options](https://approvaltestscpp.readthedocs.io/en/latest/generated_docs/Options.html) - Configuration options
- [ApprovalTests.Net.Json](https://github.com/approvals/ApprovalTests.Net.Json) - JSON-specific approval testing
- [Samman Coaching: Approval Testing Intro](https://sammancoaching.org/learning_hours/legacy/approval_testing_intro.html) - Learning hour format

### Combinatorial Testing
- [Pairwise Testing](http://www.pairwise.org/) - All-pairs testing resource
- [Wikipedia: All-pairs testing](https://en.wikipedia.org/wiki/All-pairs_testing) - 80-20 rule for test coverage

### Elm Ecosystem
- [miniBill/elm-diff](https://github.com/miniBill/elm-diff) - Elm diff library
- [elm-pages Scripts](https://elm-pages.com/docs/scripts) - BackendTask scripting
- [Vitest Snapshots](https://vitest.dev/guide/snapshot) - Modern JS snapshot testing (for comparison)

### elm-pages BackendTask + elm-test Pattern
See `elm-pages/examples/end-to-end/script/src/BackendTaskTest.elm` for a pattern of running elm-test tests via BackendTask:
```elm
testScript : String -> List (BackendTask FatalError Test.Test) -> Script
run : BackendTask FatalError Test -> BackendTask FatalError ()
```

---

*Plan authored: January 2025*
*Last updated: January 2025*
