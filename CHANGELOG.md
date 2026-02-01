# Changelog

[![Elm package](https://img.shields.io/elm-package/v/dillonkearns/elm-snapshot.svg)](https://package.elm-lang.org/packages/dillonkearns/elm-snapshot/latest/)

All notable changes to
[the `dillonkearns/elm-snapshot` elm package](http://package.elm-lang.org/packages/dillonkearns/elm-snapshot/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2026-01-31

Initial release of elm-snapshot, a snapshot testing framework for Elm.

### Added

- **Core API** (`Snapshot` module)
  - `test`, `json`, `custom` for creating snapshot tests
  - `taskTest`, `taskJson`, `taskCustom` for tests with BackendTask effects
  - `describe` for grouping tests into nested suites
  - `todo` for placeholder tests

- **CLI Options**
  - `--approve` to approve all new/changed snapshots
  - `--approve=prompt` for interactive single-keypress approval
  - `--approve-only "name"` to approve specific tests
  - `--ci` for CI-friendly compact output
  - `--list` to list all test names
  - `--prune` to remove obsolete snapshots
  - `--reporter` to open external diff tools (VS Code, Kaleidoscope, etc.)

- **Printers** (`Snapshot.Printer` module)
  - Built-in printers for strings, JSON, and Debug output
  - Custom file extensions (`.xml`, `.elm`, etc.)
  - Pretty-printed Elm syntax formatting

- **Scrubbers** (`Snapshot.Scrubber` module)
  - `timestamp` for ISO 8601 date/time strings
  - `guid` for UUIDs with stable numbered placeholders
  - `date` for ISO 8601 dates
  - `regex` for custom pattern matching

- **Init Script**
  - Run `npx elm-pages run github:dillonkearns/elm-snapshot:script/src/Init.elm` to scaffold a project
