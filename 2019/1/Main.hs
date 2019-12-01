module Main where

import Advent
import Data.Text as T

testCases1 :: [(String, String)]
testCases1 = [
    ("12\n14\n1969", "658"),
    ("12", "2"),
    ("14", "2"),
    ("1969", "654")
    ]

solution1 :: String -> String
solution1 input = show . sum $ fuel . read <$> Prelude.lines input

fuel :: Int -> Int
fuel mass = floor (fromIntegral mass / 3) - 2 

testCases2 :: [(String, String)]
testCases2 = [
    ("14", "2"),
    ("1969", "966"),
    ("100756", "50346")
    ]

solution2 :: String -> String
solution2 input = show . sum $ fuel' . read <$> Prelude.lines input

fuel' :: Int -> Int
fuel' mass
  | mass < 9 = 0
  | otherwise = let fm = fuel mass in fm + fuel' fm

main :: IO ()
main = runAdvent 2019 1 solution2 testCases2

