module Main where

import Advent
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative

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
    ("", ""),
    ("", "")
    ]

solution2 :: String -> String
solution2 input = input

main :: IO ()
-- main = runAdvent 2019 5 solution1 testCases1
main = runAdvent 2019 5 solution2 testCases2

