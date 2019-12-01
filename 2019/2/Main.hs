module Main where

import Advent
import Data.Text as T

testCases1 :: [(String, String)]
testCases1 = [
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
main = runAdvent 2019 2 solution1 testCases1

