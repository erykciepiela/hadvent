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

data Tile = Tile {
  tileId :: Int,
  tileBorder :: [[Bool]]
} deriving Show

inputParser :: Parser [Tile]
inputParser = tile `sepEndBy` newline
  where
    tile :: Parser Tile
    tile = do
      tileId <- between (string "Tile ") (string ":\n") (read <$> many1 digit)
      border <- count 10 (count 10 symbol <* newline)
      return $ Tile tileId [head border, last <$> border, last border, head <$> border]
        where
          symbol :: Parser Bool
          symbol = (False <$ char '.') <|> (True <$ char '#')

solution1 :: String -> Int
solution1 input = let
  tiles = either (error "wrong input parser") id $ parse inputParser "" input
  in product $ tileId <$> L.filter (isOnCorner tiles) tiles
    where
      isOnCorner :: [Tile] -> Tile -> Bool
      isOnCorner tiles tile = let
        ts = L.filter (\tile' -> tileId tile' /= tileId tile) tiles
        bs = tileBorder tile <&> (\side -> any (any (\side' -> side' == side || reverse side' == side) . tileBorder) ts)
        in length (L.filter (== False) bs) == 2

main :: IO ()
main = advent 2020 20 [solution1] $ do
  return ()

-- 12 x 12 tiles
-- one tile 8 x 8
-- monster os 20 x 3
-- monster can occupy max 4 x max 2 tiles
-- monster has 15 parts
-- every # has 15 potential ways to be part of dragon in 4 directions in 4 flips - 720000 checks

