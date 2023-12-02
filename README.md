# AoC2023

[Advent of Code 2023](https://adventofcode.com/2023/) solutions in Haskell (on GHC 9.8.1). Ideally this would be run without
any extra dependencies other than the included dependencies with GHC and GHCi (and `hspec`) for unit testing.

## How to run

- `cabal test`: Runs some test cases on the examples given in the day's question (and some extra if necessary). Set the
environment variable `CONTINUOUS` to any value to run all tests instead of accepting from stdin.
- `cabal run`: Runs the solution on some actual input. Supply input files in the `input` directory named `dayN.txt` where `N`
  is the respective day number (1-25, no leading or trailing zeroes, and `day` and the extension `txt` are in lowercase).
