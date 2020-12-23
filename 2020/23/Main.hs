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

newtype Ring = Ring [Int] deriving Show

turnRing :: Ring -> Ring
turnRing (Ring nums) = let
  cursor = head nums
  threesome = L.take 3 $ tail nums
  possibleDestinations = zip [5..] (L.drop 4 nums)
  (destinationIndex, destinationVal) = head $ sortOn ((`mod` length nums). (-) cursor. snd) possibleDestinations
  newNums = L.drop 4 (L.take destinationIndex nums) <> threesome <> L.drop destinationIndex nums <> [cursor]
  -- in trace (show newNums <> " " <> show destinationIndex <> " " <> show destinationVal) (Ring newNums)
  in (Ring newNums)

labels :: Ring -> String
labels (Ring nums) = let
  (post, pre) = L.break (== 1) nums
  in intToDigit <$> tail (pre <> post)

inputParser :: Parser Ring
inputParser = do
  nums <- fmap digitToInt <$> many1 digit
  return $ Ring nums

inputParser2 :: Parser Ring
inputParser2 = do
  nums <- fmap digitToInt <$> many1 digit
  return $ Ring $ nums <> [10..1000000]

solution1 :: String -> String
solution1 input = let
  ring = either (\e -> error $ "wrong input parser: " <> show e) id $ parse inputParser "" input
  in Main.labels $ iterate turnRing ring !! 100

solution2 :: String -> String
solution2 input = let
  ring = (either (\e -> error $ "wrong input parser: " <> show e) id $ parse inputParser2 "" input)
  in Main.labels $ iterate turnRing ring !! 10000000

main :: IO ()
main = advent 2020 23 [solution2] $ do
  return ()
