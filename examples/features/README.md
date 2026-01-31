# Features Example

This example demonstrates advanced elm-snapshot features, particularly the `Printer.elm` custom printer for snapshotting Elm data structures.

## What's Demonstrated

### Elm Printer (`Printer.elm`)

The Elm printer pretty-prints any Elm value using elm-format style. This is particularly useful when:

1. **Testing parsers** - Snapshot the parsed AST to catch regressions in parser behavior
2. **Testing query builders** - Verify that fluent APIs produce the expected data structures
3. **Testing state machines** - Verify state after a series of transitions

### Running Tests

```bash
npm install
npm test
```

To approve new snapshots:

```bash
npm run test:approve
```
