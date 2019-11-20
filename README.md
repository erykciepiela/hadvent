# Hadvent2019

Utilities for [Advent of Code 2019](https://adventofcode.com/2019) puzzles for sulutions written in Haskell.

## Usage

### Initial setup

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/)
1. Fork and checkout this repository
1. Run `stack build` in this project directory

### Every advent day

1. Log in to [Advent of Code 2019](https://adventofcode.com/2019)
1. Copy `session` HTTP cookie value to `.session` file in this project directory
1. Go to `[day]/Main.hs` directory
1. Add test cases to `testCases` list
1. Implement `solution` function
1. Run `stack run [day]` - it will print you if test cases pass
    * if they pass it will print the solution for the puzzle for your specific puzzle input and (TODO) the solution will be submitted
    * if any fails it will print the first test case that fails
