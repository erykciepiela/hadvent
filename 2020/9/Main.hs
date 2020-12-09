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
  ns = read <$> lines input
  subseqs = reverse . L.take 26 <$> tails ns
  in head $ mapMaybe hasProperty subseqs
    where
      hasProperty :: [Int] -> Maybe Int
      hasProperty (n:ns) = case [(a , b) | a <- ns, b <- ns, a /= b, a + b == n] of
        [] -> Just n
        _ -> Nothing


solution2 :: String -> Integer
solution2 input = let
  ns = read <$> lines input
  ts = tails ns
  in head $ mapMaybe hasProperty ts
    where
      hasProperty :: [Integer] -> Maybe Integer
      hasProperty ns = case takeWhile (invalidNumber >=) $ tail $ scanl (+) 0 ns of
        [] -> Nothing
        [_] -> Nothing
        nss -> if last nss == invalidNumber then let subns = snd <$> zip nss ns in Just (minimum subns + maximum subns) else Nothing
      invalidNumber :: Integer
      invalidNumber = 258585477

main :: IO ()
main = advent 2020 9 [solution2] $ do
  return ()
