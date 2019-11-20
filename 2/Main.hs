module Main where

import Advent

solution :: String -> String
solution = id

main :: IO ()
main = runAdvent 2 solution [
    ("a", "a"),
    ("b", "b")
    ]
