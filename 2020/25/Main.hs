module Main where

import Advent

-- import Text.Parsec as P
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
import qualified Data.Map as M

import Text.Parsec as P
import Text.Parsec.String

import Debug.Trace

import Data.Either

import Data.Char

iteration :: Int -> Int -> Int
iteration subjectNumber a = (a * subjectNumber) `mod` 20201227

solution1 :: String -> String
solution1 input = let
  [pubKey1 :: Int, pubKey2 :: Int] = read <$> lines input
  loopSize1 = fst $ head $ L.dropWhile ((/=) pubKey1 . snd) $ zip [0..] (iterate (iteration 7) 1)
  loopSize2 = fst $ head $ L.dropWhile ((/=) pubKey2 . snd) $ zip [0..] (iterate (iteration 7) 1)
  encKey1 = iterate (iteration pubKey1) 1 !! loopSize2
  encKey2 = iterate (iteration pubKey2) 1 !! loopSize1
  in show encKey1 <> " " <> show encKey2

main :: IO ()
main = advent 2020 25 [solution1] $ do
  return ()