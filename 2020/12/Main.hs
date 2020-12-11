module Main where

import Advent
import Grid

import Text.Parsec as P
import Text.Parsec.String
import Data.Functor
import Data.List as L
import Control.Monad
import Data.Set as S
import Data.Maybe
import Control.Comonad
import Data.Foldable as F

data Content = Floor | EmptySeat | OccupiedSeat deriving Eq

parseContent :: Char -> Content
parseContent = \case
  '.' -> Floor
  'L' -> EmptySeat
  '#' -> OccupiedSeat

solution1 :: String -> String
solution1 input = let
  g = parseContent <$> grid '.' (lines input)
  step g = let s = extract g in case s of
    EmptySeat -> if numberOfOccupiedSeatsAround g == 0 then OccupiedSeat else s
    OccupiedSeat -> if numberOfOccupiedSeatsAround g >= 4 then EmptySeat else s
    _ -> s
  in show $ countOccupiedSeats $ stabilize step g

solution2 :: String -> String
solution2 input = let
  g = parseContent <$> grid '.' (lines input)
  step g = let s = extract g in case s of
    EmptySeat -> if numberOfOccupiedSeatsAtSight g == 0 then OccupiedSeat else s
    OccupiedSeat -> if numberOfOccupiedSeatsAtSight g >= 5 then EmptySeat else s
    _ -> s
  in show $ countOccupiedSeats $ stabilize step g

numberOfOccupiedSeatsAround :: Grid Content -> Int
numberOfOccupiedSeatsAround g = L.length $ L.filter (== OccupiedSeat) $ (flip pointAt g)
  <$> [
        (-1, -1), (0, -1), (1, -1),
        (-1, 0),           (1, 0),
        (-1, 1),  (0, 1),  (1, 1)
      ]

numberOfOccupiedSeatsAtSight :: Grid Content -> Int
numberOfOccupiedSeatsAtSight g = L.length $ L.filter occupiedSeatAtSight $ (flip lineTowards g)
  <$> [
        (-1, -1), (0, -1), (1, -1),
        (-1, 0),           (1, 0),
        (-1, 1),  (0, 1),  (1, 1)
      ]
    where
      occupiedSeatAtSight = maybe False (== OccupiedSeat) . listToMaybe . dropWhile (== Floor) . L.take 100

stabilize :: (Grid Content -> Content) -> Grid Content -> Grid Content
stabilize step g = let
    g' = extend step g
  in if areaOver (95, 99) g == areaOver (95, 99) g' then g' else stabilize step g'

countOccupiedSeats :: Grid Content -> Int
countOccupiedSeats g = length $ L.filter (== OccupiedSeat) $ mconcat $ areaOver (95, 99) g

main :: IO ()
main = advent 2020 12 [solution1] $ do
    return ()
