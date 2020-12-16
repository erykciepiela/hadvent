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
import BipartiteMatch


data Input = Input {
  rules :: [Rule],
  myTicket :: Ticket,
  nearbyTickets :: [Ticket]
} deriving Show

data Rule = Rule {
  fieldName :: String,
  ranges :: [(Int, Int)]
} deriving Show

data Ticket = Ticket {
  ticketValues :: [Int]
 } deriving Show

inputParser :: Parser Input
inputParser = do
  rules <- ruleParser `sepEndBy` char '\n'
  string "\nyour ticket:\n"
  myTicket <- ticketParser
  string "\n\nnearby tickets:\n"
  nearbyTickets <- ticketParser `sepBy` char '\n'
  return $ Input rules myTicket nearbyTickets

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

ticketParser :: Parser Ticket
ticketParser = Ticket <$> (read <$> many1 digit) `sepBy` char ','

solution1 :: String -> Int
solution1 input = let
  input' = either (error "wrong parser") id $ parse inputParser "?" input
  nearbyTicketsValues = mconcat (ticketValues <$> nearbyTickets input')
  allRulesRanges = mconcat $ ranges <$> rules input'
  in sum $ L.filter (outsideAllRanges allRulesRanges) nearbyTicketsValues

outsideAllRanges :: [(Int, Int)] -> Int -> Bool
outsideAllRanges [] _ = True
outsideAllRanges ((from, to):ranges) n = (n < from || n > to) && outsideAllRanges ranges n

insideAnyOfRanges :: [(Int, Int)] -> Int -> Bool
insideAnyOfRanges [] _ = False
insideAnyOfRanges ((from, to):ranges) n = (n >= from && n <= to) || insideAnyOfRanges ranges n

solution2 :: String -> Int
solution2 input = let
  input' = either (error "wrong parser") id $ parse inputParser "?" input
  allTickets = myTicket input' : nearbyTickets input'
  allRules = rules input'
  allValidTickets = L.filter (\t -> not $ any (outsideAllRanges (mconcat $ ranges <$> allRules)) (ticketValues t) ) allTickets
  allMatches = S.fromList [(i, rn) | i <- [0..19], rn <- [0..19], all (insideAnyOfRanges (ranges (allRules !! rn))) ((!! i) . ticketValues <$> allValidTickets)]
  maxMatch = matching allMatches
  in product $ (\ix -> (ticketValues (myTicket input')) !! ix) <$> (snd <$> (L.take 6 $ M.toList $ maxMatch))

main :: IO ()
main = advent 2020 17 [solution2] $ do
  return ()
