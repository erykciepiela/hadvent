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

data Dir = E | SE | SW | W | NW | NE deriving Eq

data Tile = Tile {
  e :: Int,
  n :: Int
} deriving (Show, Eq, Ord)

neighTiles (Tile e n) = [
  Tile (e-1) n,
  Tile (e+1) n,
  Tile (e + if even (n+1) then 1 else 0) (n+1),
  Tile (e - if odd (n-1) then 1 else 0) (n-1),
  Tile (e - 1 + if even (n+1) then 1 else 0) (n+1),
  Tile (e + 1 - if odd (n-1) then 1 else 0) (n-1)
  ]

inputParser :: Parser [Tile]
inputParser = tileParser `sepEndBy` newline

tileParser :: Parser Tile
tileParser = do
  dirs <- many1 $
    (SE <$ try (string "se")) <|>
    (SW <$ try (string "sw")) <|>
    (E <$ try (string "e")) <|>
    (NE <$ try (string "ne")) <|>
    (NW <$ try (string "nw")) <|>
    (W <$ string "w")
  let se = length (L.filter (== SE) dirs) - length (L.filter (== NW) dirs)
  let ne = length (L.filter (== NE) dirs) - length (L.filter (== SW) dirs)
  return $ Tile
    (length (L.filter (== E) dirs) - length (L.filter (== W) dirs) + (ne + se) `div` 2)
    (ne - se)

foo :: Tile -> Tile -> Tile
foo t1 t2 = Tile (e t1 + e t2 + (n t1 + n t2) `div` 2) (n t1 - n t2)

solution1 :: String -> String
solution1 input = let
  tiles = either (\e -> error $ "wrong input parser: " <> show e) id $ parse inputParser "" input
  a = M.unionsWith (+) ((`M.singleton` 1) <$> tiles)
  in show $ length $ L.filter id $ odd <$> M.elems a

solution2 :: String -> String
solution2 input = let
  tiles = either (\e -> error $ "wrong input parser: " <> show e) id $ parse inputParser "" input
  a = M.unionsWith (+) ((`M.singleton` 1) <$> tiles)
  back = M.fromList [(Tile e n, 0) | e <- [-121, -120 .. 121], n <- [-121, -120 .. 121]]
  b = M.unionWith (+) back a
  in show $ length $ L.filter odd $ M.elems $ iterate foo b !! 100
  -- in show $ a
    where
      foo :: M.Map Tile Int -> M.Map Tile Int
      foo m = M.mapWithKey (\tile v -> let
        blackNeighs = L.length (L.filter odd (catMaybes ((`M.lookup` m) <$> neighTiles tile)))
        in case (v, blackNeighs) of
          (v, blackNeighs) | odd v && (blackNeighs == 0 || blackNeighs > 2) -> 0
          (v, blackNeighs) | even v && blackNeighs == 2 -> 1
          (v, _) -> v) m

main :: IO ()
main = advent 2020 25 [solution2] $ do
  return ()
