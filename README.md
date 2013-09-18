# retry - combinators for monadic actions that may fail

## About

Monadic action combinators that add delayed-retry functionality,
potentially with exponential-backoff, to arbitrary actions.

The main purpose of this package is to make it easy to work reliably
with IO and similar actions that often fail. Common examples are
database queries and large file uploads.


## Documentation

Please see haddocks for documentation.

## Changes

### 0.3

Thanks to John Wiegley and Michael Snoyman for their contributions:

- Now using monad-control instead of MonadCatchIO, which is widely
  agreed to be broken. 
- Now using transformers instead of mtl, which was a broader than
  needed dependency.

## Author

Ozgun Ataman, Soostone Inc

