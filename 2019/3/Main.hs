module Main where

import Advent
import Data.Text as T
import Data.List as L

testCases1 :: [(String, String)]
testCases1 = [
    ("", ""),
    ("", ""),
    ("", ""),
    ("", ""),
    ("", "")
    ]

solution1 :: String -> String
solution1 input = input

testCases2 :: [(String, String)]
testCases2 = [
    ("", ""),
    ("", ""),
    ("", "")
    ]

solution2 :: String -> String
solution2 input = input

main :: IO ()
main = runAdvent 2019 3 solution1 testCases1

