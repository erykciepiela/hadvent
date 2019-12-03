module Main where

import Advent
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative

testCases1 :: [(String, String)]
testCases1 = [
    ("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", "159"),
    ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7", "135")
    ]

solution1 :: String -> String
solution1 input = let
    lines = T.lines (T.pack input)
    traces = fmap (scanl (\(x, y) (x1, y1) -> (x + x1, y + y1)) (0, 0)) $ join . fmap (moves . T.unpack) . T.splitOn "," <$> lines
    [s1, s2] = S.fromList . tail <$> traces
    inters = S.toList $ S.intersection s1 s2
    dist = L.minimum $ (\(x, y) -> abs x + abs y) <$> inters
    in show dist

moves :: String -> [(Int, Int)]
moves code = let 
    vect = case head code of
        'R' -> (1, 0)
        'L' -> (-1, 0)
        'U' -> (0, 1)
        'D' -> (0, -1)
    len = read $ tail code
    in replicate len vect
    

testCases2 :: [(String, String)]
testCases2 = [
    ("R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83", "610"),
    ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7", "410")
    ]

solution2 :: String -> String
solution2 input = let
    lines = T.lines (T.pack input)
    traces = fmap (scanl (\(x, y) (x1, y1) -> (x + x1, y + y1)) (0, 0)) $ join . fmap (moves . T.unpack) . T.splitOn "," <$> lines
    [s1, s2] = S.fromList . tail <$> traces
    inters = S.toList $ S.intersection s1 s2
    len = L.minimum $ (\p -> head (L.elemIndices p (traces !! 0)) + head (L.elemIndices p (traces !! 1))) <$> inters
    in show len

main :: IO ()
-- main = runAdvent 2019 3 solution1 testCases1
main = runAdvent 2019 3 solution2 testCases2

