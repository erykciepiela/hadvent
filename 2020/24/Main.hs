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
    (length (L.filter (== E) dirs) - length (L.filter (== W) dirs) + (se + ne) `div` 2)
    (ne - se)

solution1 :: String -> String
solution1 input = let
  tiles = either (\e -> error $ "wrong input parser: " <> show e) id $ parse inputParser "" input
  a = M.unionsWith (+) ((`M.singleton` 1) <$> tiles)
  in show $ length $ L.filter id $ odd <$> M.elems a

main :: IO ()
main = advent 2020 24 [solution1] $ do
  return ()
