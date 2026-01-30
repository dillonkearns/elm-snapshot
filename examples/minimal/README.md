# Minimal elm-snapshot Example

This is the simplest possible elm-snapshot setup. Use it as a starting point for adding snapshot tests to your project.

## Structure

```
minimal/
├── package.json              # elm-pages dependency + test scripts
├── src/
│   └── Greeting.elm          # Simple module to test
└── snapshot-tests/
    ├── elm.json              # Test project dependencies
    ├── src/
    │   └── Snapshots.elm     # Snapshot tests
    └── snapshots/
        └── Snapshots/
            └── greeting.approved.txt
```

## Running

```bash
npm install
npm test           # Run tests
npm run test:approve  # Approve new/changed snapshots
```

## Adapting for Your Project

1. Copy this directory to your project
2. Update `snapshot-tests/elm.json` source-directories to include your project's src
3. Import your modules in `Snapshots.elm` and add tests
