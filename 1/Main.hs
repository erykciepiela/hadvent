module Main where

import Advent

testCases :: [(String, String)]
testCases = [
    ("a", "a"),
    ("b", "b")
    ]

solution :: String -> String
solution = id

main :: IO ()
main = runAdvent 1 solution testCases
