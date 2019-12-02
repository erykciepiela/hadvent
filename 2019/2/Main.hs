module Main where

import Advent
import Data.Text as T
import Data.List as L

testCases1 :: [(String, String)]
testCases1 = [
    -- ("1,0,0,0,99", "2,0,0,0,99"),
    -- ("2,3,0,3,99", "2,3,0,6,99"),
    -- ("2,4,4,5,99,0", "2,4,4,5,99,9801"),
    -- ("1,1,1,4,99,5,6,0,99", "30,1,1,4,2,5,6,0,99")
    ]

interp :: Int -> [Int] -> [Int]
interp _ [] = []
interp c l = case l !! c of
    99 -> l
    1 -> let nl = set l (l !! (c + 3)) (l !! (l !! (c +1)) + l !! (l !! (c+2))) in interp (c + 4) nl
    2 -> let nl = set l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! (l !! (c+2))) in interp (c + 4) nl

set :: [Int] -> Int -> Int -> [Int]
set list pos val = L.take pos list ++ [val] ++ L.drop (pos + 1) list


solution1 :: String -> String
solution1 = show . foo 12 2

foo noun verb input = let
    (n0:n1:n2:ns) = read . unpack <$> T.splitOn "," (pack input)
    in L.head $ interp 0 (n0:noun:verb:ns)

testCases2 :: [(String, String)]
testCases2 = [
    ("", ""),
    ("", ""),
    ("", "")
    ]

solution2 :: String -> String
solution2 input = let (x, y):_ = [(n, v) | n <- [0..99], v <- [0..99], foo n v input == 19690720] in show $ x*100 +y

main :: IO ()
main = runAdvent 2019 2 solution2 []

