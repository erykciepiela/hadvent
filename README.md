# Hadvent

Utilities for [Advent of Code](https://adventofcode.com/) puzzles for solutions written in Haskell.

## Usage

### Initial setup

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/)
1. Fork and checkout this repository
1. Run `stack build` in this project directory

### Every advent day

1. Log in to [Advent of Code](https://adventofcode.com/2019/auth/login)
1. Copy `session` HTTP cookie value to `.session` file in this project directory
1. Go to `[year]/[day]/Main.hs` directory
1. Add test cases to `testCases` list
1. Implement `solution` function
1. Run `stack run [year]-[day]` - it will print you if test cases pass
    * if they pass it will print the solution for the puzzle for your specific puzzle input
    * if any fails it will print the first test case that fails
