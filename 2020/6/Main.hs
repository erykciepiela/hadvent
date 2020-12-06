module Main where

import Advent

import Text.Parsec as P
import Text.Parsec.String
import Data.Functor
import Data.List
import Control.Monad

type Question = Char

newtype Group = Group {
  questionsAnsweredYes :: [[Question]]
} deriving (Show, Eq)

groupsParser :: Parser [Group]
groupsParser = groupParser `sepBy` char '\n'

groupParser :: Parser Group
groupParser = do
  questions <- many1 (oneOf ['a' .. 'z']) `sepEndBy` char '\n'
  return $ Group $ questions

solution1 :: String -> Int
solution1 input = let
  groups = either (error "invalid parser") id $ parse groupsParser "?" input
  in sum $ groups <&> (length . nub . join . questionsAnsweredYes)

solution2 :: String -> Int
solution2 input = let
  groups = either (error "invalid parser") id $ parse groupsParser "?" input
  in sum $ groups <&> (length . foldl1 intersect . questionsAnsweredYes)

main :: IO ()
main = advent 2020 6 [solution2] $ do
  return ()

