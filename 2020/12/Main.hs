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

data Direction = N | E | S | W deriving Show

type Position = (Int, Int)

---
type Move1 = (Int, Int, Direction) -> (Int, Int, Direction)

parseMoves :: Parser [Move1]
parseMoves = parseMove `sepEndBy` char '\n'
  where
    parseMove :: Parser Move1
    parseMove = choice [parseN, parseS, parseE, parseW, parseL, parseR, parseF]
      where
        parseN :: Parser Move1
        parseN = char 'N' >> (\n (x, y, d) -> (x, y - n, d)) <$> parseInt
        parseS :: Parser Move1
        parseS = char 'S' >> (\n (x, y, d) -> (x, y + n, d)) <$> parseInt
        parseE :: Parser Move1
        parseE = char 'E' >> (\n (x, y, d) -> (x + n, y, d)) <$> parseInt
        parseW :: Parser Move1
        parseW = char 'W' >> (\n (x, y, d) -> (x - n, y, d)) <$> parseInt
        parseL :: Parser Move1
        parseL = char 'L' >> (\n (x, y, d) -> (x, y, iterate moveL d  !! (n `div` 90))) <$> parseInt
          where
            moveL :: Direction -> Direction
            moveL N = W
            moveL W = S
            moveL S = E
            moveL E = N
        parseR :: Parser Move1
        parseR = char 'R' >> (\n (x, y, d) -> (x, y, iterate moveR d  !! (n `div` 90))) <$> parseInt
          where
            moveR :: Direction -> Direction
            moveR N = E
            moveR E = S
            moveR S = W
            moveR W = N
        parseF :: Parser Move1
        parseF = char 'F' >> (\n p -> case p of
          (x, y, N) -> (x, y - n, N)
          (x, y, S) -> (x, y + n, S)
          (x, y, E) -> (x + n, y, E)
          (x, y, W) -> (x - n , y, W)) <$> parseInt

solution1 :: String -> Int
solution1 input = let
  g :: [Move1] = either (error "wring parser") id $ parse parseMoves "?" input
  (x, y, _) = appEndo (mconcat (Endo <$> reverse g)) (0, 0, E)
  in abs x + abs y



---
type Move = (Position, Position) -> (Position, Position)

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseMoves2 :: Parser [Move]
parseMoves2 = parseMove `sepEndBy` char '\n'
  where
    parseMove :: Parser Move
    parseMove = choice [parseN, parseS, parseE, parseW, parseL, parseR, parseF]
      where
        parseN :: Parser Move
        parseN = char 'N' >> (\n (c, (dx, dy)) -> (c, (dx, dy - n))) <$> parseInt
        parseS :: Parser Move
        parseS = char 'S' >> (\n (c, (dx, dy)) -> (c, (dx, dy + n))) <$> parseInt
        parseE :: Parser Move
        parseE = char 'E' >> (\n (c, (dx, dy)) -> (c, (dx + n, dy))) <$> parseInt
        parseW :: Parser Move
        parseW = char 'W' >> (\n (c, (dx, dy)) -> (c, (dx - n, dy))) <$> parseInt
        parseL :: Parser Move
        parseL = char 'L' >> (\n (c, (dx, dy)) -> (c, iterate moveL (dx, dy)  !! (n `div` 90))) <$> parseInt
          where
            moveL :: Position -> Position
            moveL (x, y) = (y, -x)
        parseR :: Parser Move
        parseR = char 'R' >> (\n (c, (dx, dy)) -> (c, iterate moveR (dx, dy)  !! (n `div` 90))) <$> parseInt
          where
            moveR :: Position -> Position
            moveR (x, y) = (-y, x)
        parseF :: Parser Move
        parseF = char 'F' >> (\n ((x, y), (dx, dy)) -> ((x + n * dx, y + n * dy), (dx, dy))) <$> parseInt

solution2 :: String -> Int
solution2 input = let
  moves :: [Move] = either (error "wrong parser") id $ parse parseMoves2 "?" input
  ((x, y), _) = appEndo (mconcat (Endo <$> reverse moves)) ((0, 0), (10, -1))
  -- or:
  -- ((x, y), _) = F.foldl (\p m -> m p) ((0, 0), (10, -1)) moves
  in abs x + abs y

main :: IO ()
main = advent 2020 12 [solution2] $ do
    return ()
