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
import Data.Map as M

import Debug.Trace

import Grid

solution1 :: String -> String
solution1 input = let
  space0 = space '.' [lines input]
  space6 = iterate (extend rules) space0 !! 6
  space6mx = spaceOver (33, 33, 33) ((shiftSpaceZUp 6 . shiftSpaceXUp 6 .shiftSpaceYUp 6) space6)
  foo = length$ L.filter (== '#') $ concat $ fmap concat space6mx
  in show $ foo

rules :: Space Char -> Char
rules space = let
  csss = spaceOver (3, 3, 3) ((shiftSpaceZUp 1 . shiftSpaceYUp 1 . shiftSpaceXUp 1) space)
  activeNeighborsNum = length $ L.filter (== '#') [
    csss !! 0 !! 0 !! 0, csss !! 0 !! 0 !! 1, csss !! 0 !! 0 !! 2,
    csss !! 0 !! 1 !! 0, csss !! 0 !! 1 !! 1, csss !! 0 !! 1 !! 2,
    csss !! 0 !! 2 !! 0, csss !! 0 !! 2 !! 1, csss !! 0 !! 2 !! 2,
    csss !! 1 !! 0 !! 0, csss !! 1 !! 0 !! 1, csss !! 1 !! 0 !! 2,
    csss !! 1 !! 1 !! 0,                           csss !! 1 !! 1 !! 2,
    csss !! 1 !! 2 !! 0, csss !! 1 !! 2 !! 1, csss !! 1 !! 2 !! 2,
    csss !! 2 !! 0 !! 0, csss !! 2 !! 0 !! 1, csss !! 2 !! 0 !! 2,
    csss !! 2 !! 1 !! 0, csss !! 2 !! 1 !! 1, csss !! 2 !! 1 !! 2,
    csss !! 2 !! 2 !! 0, csss !! 2 !! 2 !! 1, csss !! 2 !! 2 !! 2]
  in case extract space of
    '#' -> if activeNeighborsNum == 2 || activeNeighborsNum == 3 then '#' else '.'
    '.' -> if activeNeighborsNum == 3 then '#' else '.'

solution2 :: String -> String
solution2 input = let
  space0 = space4 '.' [[lines input]]
  space6 = iterate (extend rules') space0 !! 6
  space6mx = space4Over (21, 21, 21, 21) ((shiftSpace4WUp 6 . shiftSpace4ZUp 6 . shiftSpace4XUp 6 .shiftSpace4YUp 6) space6)
  foo = length$ L.filter (== '#') $ fmap (concat . concat . fmap concat) space6mx
  in show foo

rules' :: Space4 Char -> Char
rules' space = let
  csss = space4Over (3, 3, 3, 3) ((shiftSpace4WUp 1 . shiftSpace4ZUp 1 . shiftSpace4YUp 1 . shiftSpace4XUp 1) space)
  activeNeighborsNum = length $ L.filter (== '#') [
    csss !! 0 !! 0 !! 0 !! 0, csss !! 0 !! 0 !! 1 !! 0, csss !! 0 !! 0 !! 2 !! 0,
    csss !! 0 !! 1 !! 0 !! 0, csss !! 0 !! 1 !! 1 !! 0, csss !! 0 !! 1 !! 2 !! 0,
    csss !! 0 !! 2 !! 0 !! 0, csss !! 0 !! 2 !! 1 !! 0, csss !! 0 !! 2 !! 2 !! 0,
    csss !! 1 !! 0 !! 0 !! 0, csss !! 1 !! 0 !! 1 !! 0, csss !! 1 !! 0 !! 2 !! 0,
    csss !! 1 !! 1 !! 0 !! 0, csss !! 1 !! 1 !! 1 !! 0, csss !! 1 !! 1 !! 2 !! 0,
    csss !! 1 !! 2 !! 0 !! 0, csss !! 1 !! 2 !! 1 !! 0, csss !! 1 !! 2 !! 2 !! 0,
    csss !! 2 !! 0 !! 0 !! 0, csss !! 2 !! 0 !! 1 !! 0, csss !! 2 !! 0 !! 2 !! 0,
    csss !! 2 !! 1 !! 0 !! 0, csss !! 2 !! 1 !! 1 !! 0, csss !! 2 !! 1 !! 2 !! 0,
    csss !! 2 !! 2 !! 0 !! 0, csss !! 2 !! 2 !! 1 !! 0, csss !! 2 !! 2 !! 2 !! 0,

    csss !! 0 !! 0 !! 0 !! 1, csss !! 0 !! 0 !! 1 !! 1, csss !! 0 !! 0 !! 2 !! 1,
    csss !! 0 !! 1 !! 0 !! 1, csss !! 0 !! 1 !! 1 !! 1, csss !! 0 !! 1 !! 2 !! 1,
    csss !! 0 !! 2 !! 0 !! 1, csss !! 0 !! 2 !! 1 !! 1, csss !! 0 !! 2 !! 2 !! 1,
    csss !! 1 !! 0 !! 0 !! 1, csss !! 1 !! 0 !! 1 !! 1, csss !! 1 !! 0 !! 2 !! 1,
    csss !! 1 !! 1 !! 0 !! 1,                           csss !! 1 !! 1 !! 2 !! 1,
    csss !! 1 !! 2 !! 0 !! 1, csss !! 1 !! 2 !! 1 !! 1, csss !! 1 !! 2 !! 2 !! 1,
    csss !! 2 !! 0 !! 0 !! 1, csss !! 2 !! 0 !! 1 !! 1, csss !! 2 !! 0 !! 2 !! 1,
    csss !! 2 !! 1 !! 0 !! 1, csss !! 2 !! 1 !! 1 !! 1, csss !! 2 !! 1 !! 2 !! 1,
    csss !! 2 !! 2 !! 0 !! 1, csss !! 2 !! 2 !! 1 !! 1, csss !! 2 !! 2 !! 2 !! 1,

    csss !! 0 !! 0 !! 0 !! 2, csss !! 0 !! 0 !! 1 !! 2, csss !! 0 !! 0 !! 2 !! 2,
    csss !! 0 !! 1 !! 0 !! 2, csss !! 0 !! 1 !! 1 !! 2, csss !! 0 !! 1 !! 2 !! 2,
    csss !! 0 !! 2 !! 0 !! 2, csss !! 0 !! 2 !! 1 !! 2, csss !! 0 !! 2 !! 2 !! 2,
    csss !! 1 !! 0 !! 0 !! 2, csss !! 1 !! 0 !! 1 !! 2, csss !! 1 !! 0 !! 2 !! 2,
    csss !! 1 !! 1 !! 0 !! 2, csss !! 1 !! 1 !! 1 !! 2, csss !! 1 !! 1 !! 2 !! 2,
    csss !! 1 !! 2 !! 0 !! 2, csss !! 1 !! 2 !! 1 !! 2, csss !! 1 !! 2 !! 2 !! 2,
    csss !! 2 !! 0 !! 0 !! 2, csss !! 2 !! 0 !! 1 !! 2, csss !! 2 !! 0 !! 2 !! 2,
    csss !! 2 !! 1 !! 0 !! 2, csss !! 2 !! 1 !! 1 !! 2, csss !! 2 !! 1 !! 2 !! 2,
    csss !! 2 !! 2 !! 0 !! 2, csss !! 2 !! 2 !! 1 !! 2, csss !! 2 !! 2 !! 2 !! 2 ]
  in case extract space of
    '#' -> if activeNeighborsNum == 2 || activeNeighborsNum == 3 then '#' else '.'
    '.' -> if activeNeighborsNum == 3 then '#' else '.'

main :: IO ()
main = advent 2020 17 [solution2] $ do
  return ()
