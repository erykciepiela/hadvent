module Main where

import Advent
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative

testCases1 :: [(String, String)]
testCases1 = [
    ]

solution1 :: String -> String
solution1 input = let
    a = read $ take 6 input
    z = read $ drop 7 input
    in show $ length [(a1, a2, a3, a4, a5, a6) |
        a1 <- [0..9],
        a2 <- [0..9],
        a3 <- [0..9],
        a4 <- [0..9],
        a5 <- [0..9],
        a6 <- [0..9],
        a2 >= a1,
        a3 >= a2,
        a4 >= a3,
        a5 >= a4,
        a6 >= a5,
        let s = a1*100000 + a2*10000+a3*1000+a4*100+a5*10+a6,
        s >= a,
        s <= z,
        a1 == a2 && a2 /= a3|| a2 == a3 && a1 /= a2 && a3 /= a4 || a3 == a4 && a2 /= a3 && a4 /= a5|| a4 == a5 && a3 /= a4 && a5 /= a6|| a5 == a6 && a4 /= a5 
    ]

testCases2 :: [(String, String)]
testCases2 = [
    ]

solution2 :: String -> String
solution2 input = let
    a = read $ take 6 input
    z = read $ drop 7 input
    in show $ length [(a1, a2, a3, a4, a5, a6) |
        a1 <- [0..9],
        a2 <- [0..9],
        a3 <- [0..9],
        a4 <- [0..9],
        a5 <- [0..9],
        a6 <- [0..9],
        a2 >= a1,
        a3 >= a2,
        a4 >= a3,
        a5 >= a4,
        a6 >= a5,
        let s = a1*100000 + a2*10000+a3*1000+a4*100+a5*10+a6,
        s >= a,
        s <= z,
        a1 == a2|| a2 == a3 || a3 == a4 || a4 == a5 || a5 == a6
    ]

main :: IO ()
-- main = runAdvent 2019 4 solution1 testCases1
main = runAdvent 2019 4 solution2 testCases2

