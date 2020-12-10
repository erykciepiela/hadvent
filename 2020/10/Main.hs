module Main where

import Advent

import Text.Parsec as P
import Text.Parsec.String
import Data.Functor
import Data.List as L
import Control.Monad
import Data.Set as S
-- import Data.Map as M
import Data.Maybe

solution1 :: String -> Int
solution1 input = let
  joltages :: [Int] = sort $ read <$> lines input
  deltas es = (\(a, b) -> b - a) <$> zip es (tail es)
  djs :: [Int] = [head joltages] <> deltas joltages <> [3]
  in length (L.filter (== 1) djs) * length (L.filter (==3) djs)


solution2 :: String -> Int
solution2 input = let
  joltages :: [Int] = sort $ read <$> lines input
  deltas es = (\(a, b) -> b - a) <$> zip es (tail es)
  djs :: [Int] = [head joltages] <> deltas joltages <> [3]
  in product $ waysToStepThrough <$> notZeroSpansOf 1 djs

notZeroSpansOf :: Eq a => a -> [a] -> [Int]
notZeroSpansOf a as = L.filter (/= 0) $ spansOf a as
  where
    spansOf :: Eq a => a -> [a] -> [Int]
    spansOf a as = let (ones, rest) = span (== a) as in length ones : case rest of
      [] -> []
      rest -> spansOf a (tail rest)

waysToStepThrough :: Int -> Int
waysToStepThrough 1 =       1
waysToStepThrough 2 =     1 + 1
waysToStepThrough 3 =   1 + 2 + 1
waysToStepThrough 4 = 0 + 3 + 3 + 1

main :: IO ()
main = advent 2020 10 [solution2] $ do
  return ()
