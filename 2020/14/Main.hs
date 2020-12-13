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

solution2 :: String -> Integer
solution2 input = let
  mBusNos = parseInput2 input
  busOffs = mapMaybe (\(mbn, off) -> mbn <&> (\bn -> (off, fromIntegral bn))) $ zip mBusNos [0..]
  in fst $ F.foldl1 Main.findIndices busOffs

findIndices :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
findIndices (start, period) (offset', period') = (start'', period'')
  where
    start'' = head [start' | n <- [0..], let start' = start + n * period, (start' + offset') `mod` period' == 0]
    period'' = lcm period period'

main :: IO ()
main = advent 2020 14 [solution2] $ do
    return ()

