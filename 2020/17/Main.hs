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

-- data Cell = Inactive | Active deriving (Show, Eq)

-- parseCell :: Char -> Cell
-- parseCell = \case
--   '.' -> Inactive
--   '#' -> Active

solution1 :: String -> String
solution1 input = let
  space0 = space '.' [lines input]
  space6 = iterate (extend rules) space0 !! 6
  space6mx = spaceOver (33, 33, 33) ((shiftSpaceZUp 6 . shiftSpaceXUp 6 .shiftSpaceYUp 6) space6)
  foo = length$ L.filter (== '#') $ concat $ fmap concat space6mx
  in show $ foo

-- If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active. Otherwise, the cube becomes inactive.
-- If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active. Otherwise, the cube remains inactive.
rules :: Space Char -> Char
rules space = let
  csss = spaceOver (3, 3, 3) ((shiftSpaceZUp 1 . shiftSpaceYUp 1 . shiftSpaceXUp 1) space)
  activeNeighborsNum = length $ L.filter (== '#') [
    csss !! 0 !! 0 !! 0, csss !! 0 !! 0 !! 1, csss !! 0 !! 0 !! 2,
    csss !! 0 !! 1 !! 0, csss !! 0 !! 1 !! 1, csss !! 0 !! 1 !! 2,
    csss !! 0 !! 2 !! 0, csss !! 0 !! 2 !! 1, csss !! 0 !! 2 !! 2,
    csss !! 1 !! 0 !! 0, csss !! 1 !! 0 !! 1, csss !! 1 !! 0 !! 2,
    csss !! 1 !! 1 !! 0,                      csss !! 1 !! 1 !! 2,
    csss !! 1 !! 2 !! 0, csss !! 1 !! 2 !! 1, csss !! 1 !! 2 !! 2,
    csss !! 2 !! 0 !! 0, csss !! 2 !! 0 !! 1, csss !! 2 !! 0 !! 2,
    csss !! 2 !! 1 !! 0, csss !! 2 !! 1 !! 1, csss !! 2 !! 1 !! 2,
    csss !! 2 !! 2 !! 0, csss !! 2 !! 2 !! 1, csss !! 2 !! 2 !! 2]
  in case extract space of
    '#' -> if activeNeighborsNum == 2 || activeNeighborsNum == 3 then '#' else '.'
    '.' -> if activeNeighborsNum == 3 then '#' else '.'


main :: IO ()
main = advent 2020 17 [solution1] $ do
  return ()
