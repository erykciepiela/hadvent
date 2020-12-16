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
import Data.Bits
import Data.Map as M

import Debug.Trace


data Input = Input {
  rules :: [Rule],
  myTicket :: Ticket,
  nearbyTickets :: [Ticket]
} deriving Show

data Rule = Rule {
  fieldName :: String,
  ranges :: [(Int, Int)]
} deriving Show

ruleParser :: Parser Rule
ruleParser = do
  fieldName <- many1 $ noneOf "\n:"
  string ": "
  ranges <- (do
    from <- read <$> many1 digit
    char '-'
    to <- read <$> many1 digit
    return (from, to)) `sepBy` string " or "
  return $ Rule fieldName ranges

data Ticket = Ticket {
  ticketValues :: [Int]
 } deriving Show

ticketParser :: Parser Ticket
ticketParser = Ticket <$> (read <$> many1 digit) `sepBy` char ','

inputParser :: Parser Input
inputParser = do
  rules <- ruleParser `sepEndBy` char '\n'
  string "\nyour ticket:\n"
  myTicket <- ticketParser
  string "\n\nnearby tickets:\n"
  nearbyTickets <- ticketParser `sepBy` char '\n'
  return $ Input rules myTicket nearbyTickets


solution1 :: String -> String
solution1 input = let
  input' :: Input = either (error "wrong parser") id $ parse inputParser "?" input
  nearbyTicketsValues = mconcat (ticketValues <$> nearbyTickets input')
  allRulesRanges = mconcat $ ranges <$> rules input'
  -- in show nearbyTicketsValues <> "\n" <> show allRulesRanges
  in show $ sum $ L.filter (outsideRanges allRulesRanges) nearbyTicketsValues
  -- in show $ L.filter (outsideRanges allRulesRanges) nearbyTicketsValues

outsideRanges :: [(Int, Int)] -> Int -> Bool
outsideRanges [] _ = True
outsideRanges ((from, to):ranges) n = if (n >= from && n <= to) then False else outsideRanges ranges n


main :: IO ()
main = advent 2020 16 [solution1] $ do
  return ()
