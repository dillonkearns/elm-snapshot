# Log Formatter Example

A demonstration of `elm-snapshot` for testing log formatting functions.

## Project Structure

```
log-formatter/
├── src/
│   ├── LogEntry.elm      # Domain type: level, message, timestamp, context
│   └── Formatter.elm     # Format functions: plain, json, compact
├── snapshot-tests/
│   ├── elm.json          # elm-pages script project
│   ├── src/
│   │   └── Snapshots.elm # Snapshot tests
│   └── snapshots/        # Generated approved snapshots
│       └── Snapshots/
└── package.json
```

## Running the Tests

```bash
# Install dependencies
npm install

# Run tests
npm test

# Approve new/changed snapshots
npm run test:approve

# List all test names
npm run test:list
```

## What This Demonstrates

1. **String output** - Testing `Formatter.plain` and `Formatter.compact`
2. **JSON output** - Testing `Formatter.json` with sorted keys
3. **Timestamp scrubbing** - Using `Scrubber.timestamp` for deterministic output
4. **Test grouping** - Using `Snapshot.describe` to organize tests
