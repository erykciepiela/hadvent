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

type Deck = [Int]

inputParser :: Parser (Deck, Deck)
inputParser = do
  [d1, d2] <- deck `sepEndBy` newline
  return (d1, d2)
  where
    deck :: Parser Deck
    deck = do
      between (string "Player ") (string ":\n") num
      num `sepEndBy` newline
        where
          num :: Parser Int
          num = read <$> many1 digit

solution1 :: String -> Int
solution1 input = let
  (deck1, deck2) = either (\e -> error $ "wrong input parser: " <> show e) id $ parse inputParser "" input
  winningDeck = case playRound (deck1, deck2) of
    ([], d2) -> d2
    (d1, _) -> d1
  in sum $ zipWith (*) [1..] (reverse winningDeck)
    where
      playRound :: (Deck, Deck) -> (Deck, Deck)
      playRound ds@(d, []) = ds
      playRound ds@([], d) = ds
      playRound ds@(c1:d1, c2:d2) = if c1 > c2
          then playRound (d1 <> [c1, c2], d2)
          else playRound (d1, d2 <> [c2, c1])

solution2 :: String -> Int
solution2 input = let
  (deck1, deck2) = either (\e -> error $ "wrong input parser: " <> show e) id $ parse inputParser "" input
  winningDeck = case playRound [] (deck1, deck2) of
    ([], d2) -> d2
    (d1, _) -> d1
  in sum $ zipWith (*) [1..] (reverse winningDeck)
    where
      playRound :: [(Deck, Deck)] -> (Deck, Deck) -> (Deck, Deck)
      playRound _ ds@(d, []) = ds
      playRound _ ds@([], d) = ds
      playRound history ds = if ds `elem` history then ds else case ds of
        (c1:d1, c2:d2) | c1 <= length d1 && c2 <= length d2 -> case playRound [] (L.take c1 d1, L.take c2 d2) of
          ([], _) -> playRound (ds:history) (d1, d2 <> [c2, c1])
          (_, _) -> playRound (ds:history) (d1 <> [c1, c2], d2)
        ds@(c1:d1, c2:d2) -> if c1 > c2
          then playRound (ds:history) (d1 <> [c1, c2], d2)
          else playRound (ds:history) (d1, d2 <> [c2, c1])

main :: IO ()
main = advent 2020 23 [solution2] $ do
  return ()
