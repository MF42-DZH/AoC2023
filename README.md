# AoC2023

[Advent of Code 2023](https://adventofcode.com/2023/) solutions in Haskell (on GHC 9.8.1). Ideally I want to minimise the total dependencies.

## How to run

- `cabal test`: Runs some test cases on the examples given in the day's question (and some extra if necessary). Set the
environment variable `CONTINUOUS` to any value to run all tests instead of accepting from stdin.
- `cabal run`: Runs the solution on some actual input. Supply input files in the `input` directory named `dayN.txt` where `N`
  is the respective day number (1-25, no leading or trailing zeroes, and `day` and the extension `txt` are in lowercase).

## Dependencies

This assumes the default set of libraries from GHC 9.8.1 (along with GHC 9.8.1 itself
and Cabal 3.10.2.0) are installed globally.

### Runtime

- [`parsec`](https://hackage.haskell.org/package/parsec-3.1.17.0) (3.1.17.0 or possibly later)

### Test

- [`hspec`](https://hackage.haskell.org/package/hspec-2.11.7) (2.11.7 or possibly later)
