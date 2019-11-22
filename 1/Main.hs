module Main where

import Advent
import qualified Data.List.Split as S
import qualified Data.Text as T
import Control.Monad
import Data.List
import Lists

testCases1 :: [(String, String)]
testCases1 = [
    ("0", "0"),
    ("+1", "1"),
    ("-1", "-1"),
    ("+1\n-1", "0"),
    ("+1\n-1\n+1", "1"),
    ("+1\n-6\n+7", "2")
    ]

solution1 :: String -> String
solution1 input = show . sum $ read . stripLeft " +" <$> words input

testCases2 :: [(String, String)]
testCases2 = [
    ("+1\n-1", "0"),
    ("+3\n+3\n+4\n-2\n-4", "10"),
    ("-6\n+3\n+8\n+5\n-6", "5"),
    ("+7\n+7\n-2\n-7\n-4", "14")
    ]

solution2 :: String -> String
solution2 input = let
    ns = cycle $ read . stripLeft " +" <$> words input
    in show $ firstRepeatedSum [0] ns 
        where
            firstRepeatedSum :: [Int] -> [Int] -> Int
            firstRepeatedSum sums (n:ns) = let 
                nsum = head sums + n 
                in if nsum `elem` sums then nsum else firstRepeatedSum (nsum:sums) ns

main :: IO ()
main = runAdvent 1 solution1 testCases1
