module Main where

import Advent

import Text.Parsec as P
import Text.Parsec.String
import Data.Functor
import Data.List as L
import Control.Monad
import Data.Set as S
import Data.Maybe
import Control.Comonad
import Data.Foldable as F
import Data.Monoid
import Text.Read
import Data.Bits
import Data.Map as M

import Debug.Trace

baz :: Int -> (Int, Int, Map Int Int) -> (Int, Int, Map Int Int)
baz limit (l, s, prevs) = if s == limit then (l, s, prevs) else case M.lookup l prevs of
  Nothing -> baz limit (0, s + 1, M.insert l (s - 1) prevs)
  Just i -> baz limit (s - i - 1, s + 1, M.insert l (s - 1) prevs)

solution1 :: String -> Int
solution1 input = let
  ns :: [Int] = either (error "wrong parser") id $ parse ((read <$> many1 digit) `sepBy` char ',') "?" input
  in (\(l, _, _) -> l) $ baz 2020 (last ns, length ns, M.fromList $ zip ns [0..(length ns) - 2])

solution2 :: String -> Int
solution2 input = let
  ns :: [Int] = either (error "wrong parser") id $ parse ((read <$> many1 digit) `sepBy` char ',') "?" input
  in (\(l, _, _) -> l) $ baz 30000000 (last ns, length ns, M.fromList $ zip ns [0..(length ns) - 2])

main :: IO ()
main = advent 2020 16 [solution1] $ do
  return ()
