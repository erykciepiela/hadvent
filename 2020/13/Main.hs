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

type BusNo = Int
type Time = Int


parseInput :: String -> (Time, [BusNo])
parseInput input = let
  ls = lines input
  in (read (ls !! 0), catMaybes $ either (error "!") id $ parse ((readMaybe <$> many1 (noneOf ",")) `sepBy` char ',') "?" (ls !! 1))

solution1 :: String -> String
solution1 input = let
  (time, busNos) = parseInput input
  departs = busNos <&> (\busNo -> (busNo, let m = time `mod` busNo in if m == 0 then 0 else busNo - m))
  (firstDepartBus, waitTime) = head $ sortOn snd departs
  in show $ firstDepartBus * waitTime

parseInput2 :: String -> [Maybe BusNo]
parseInput2 input = let
  ls = lines input
  in either (error "!") id $ parse ((readMaybe <$> many1 (noneOf ",")) `sepBy` char ',') "?" (ls !! 1)


solution2 :: String -> String
solution2 input = let
  mBusNos = parseInput2 input
  in show mBusNos

main :: IO ()
main = advent 2020 13 [solution2] $ do
    return ()

-- (x -> a) -> (a -> (x -> b)) -> (x -> b)
-- (a, x) -> ((a, x) -> b) -> (b, x)

-- (x -> a) <-> (a, x)

-- w a -> (w a -> b) -> w b
-- m a -> (a -> m b) -> m b