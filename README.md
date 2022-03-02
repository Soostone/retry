# README [![Build Status](https://travis-ci.org/Soostone/retry.svg?branch=master)](https://travis-ci.org/Soostone/retry) [![Coverage Status](https://coveralls.io/repos/Soostone/retry/badge.png?branch=master)](https://coveralls.io/r/Soostone/retry?branch=master)

retry - combinators for monadic actions that may fail

## About

Monadic action combinators that add delayed-retry functionality,
potentially with exponential-backoff, to arbitrary actions.

The main purpose of this package is to make it easy to work reliably
with IO and similar actions that often fail. Common examples are
database queries and large file uploads.

## Documentation

Please see haddocks for documentation.

## Changes

See [https://github.com/Soostone/retry/blob/master/changelog.md](changelog.md).

## Author

Ozgun Ataman, Soostone Inc

## Contributors

Contributors, please list yourself here.

- Mitsutoshi Aoe (@maoe)
- John Wiegley
- Michael Snoyman
- Michael Xavier
- Toralf Wittner
- Marco Zocca (@ocramz)
