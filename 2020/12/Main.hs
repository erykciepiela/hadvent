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

moveR :: Direction -> Direction
moveR N = E
moveR E = S
moveR S = W
moveR W = N

moveL :: Direction -> Direction
moveL N = W
moveL W = S
moveL S = E
moveL E = N

type Position = (Int, Int, Direction)

type Move = Position -> Position

parseMove :: Parser Move
parseMove = choice [parseN, parseS, parseE, parseW, parseL, parseR, parseF]

parseInt :: Parser Int
parseInt = read <$> many1 digit

parseN :: Parser Move
parseN = char 'N' >> (\n (x, y, d) -> (x, y - n, d)) <$> parseInt

parseS :: Parser Move
parseS = char 'S' >> (\n (x, y, d) -> (x, y + n, d)) <$> parseInt

parseE :: Parser Move
parseE = char 'E' >> (\n (x, y, d) -> (x + n, y, d)) <$> parseInt

parseW :: Parser Move
parseW = char 'W' >> (\n (x, y, d) -> (x - n, y, d)) <$> parseInt

parseL :: Parser Move
parseL = char 'L' >> (\n (x, y, d) -> (x, y, iterate moveL d  !! (n `div` 90))) <$> parseInt

parseR :: Parser Move
parseR = char 'R' >> (\n (x, y, d) -> (x, y, iterate moveR d  !! (n `div` 90))) <$> parseInt

parseF :: Parser Move
parseF = char 'F' >> (\n p -> case p of
  (x, y, N) -> (x, y - n, N)
  (x, y, S) -> (x, y + n, S)
  (x, y, E) -> (x + n, y, E)
  (x, y, W) -> (x - n , y, W)) <$> parseInt


parseMoves :: Parser [Move]
parseMoves = parseMove `sepEndBy` char '\n'

solution1 :: String -> Int
solution1 input = let
  g :: [Move] = either (error "wring parser") id $ parse parseMoves "?" input
  (x, y, _) = appEndo (mconcat (Endo <$> g)) (0, 0, E)
  in abs x + abs y

solution2 :: String -> Int
solution2 input = 1

main :: IO ()
main = advent 2020 12 [solution1] $ do
    return ()
