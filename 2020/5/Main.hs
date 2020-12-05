module Main where

import Advent

import Text.Parsec as P
import Text.Parsec.String
import Data.Functor
import Data.Char
import Data.List

solution1 :: String -> String
solution1 input = let
  passes = lines input
  in show $ maximum $ passes <&> (either (error "invalid parser") id . parse parsePass "?")

solution2 :: String -> String
solution2 input = let
  passes = lines input
  sortedPasses = sort $ passes <&> (either (error "invalid parser") id . parse parsePass "?")
  in show $ filter (\(a, b) -> b - a /= 1) $ zip sortedPasses (tail sortedPasses)

parsePass :: Parser Int
parsePass = do
  rowNumberBin <- binStrToInt <$> count 7 ('0' <$ char 'F' <|> '1' <$ char 'B')
  columnNumberBin <- binStrToInt <$> count 3 ('0' <$ char 'L' <|> '1' <$ char 'R')
  return $ 8 * rowNumberBin + columnNumberBin

binStrToInt :: String -> Int
binStrToInt = foldl (\a n -> a * 2 + digitToInt n) 0

main :: IO ()
main = advent 2020 5 [solution1, solution2] $ do
  binStrToInt "101" `shouldBe` 5
  binStrToInt "1111111" `shouldBe` 127
  either (error "invalid parser") id (parse parsePass "?" "BFFFBBFRRR") `shouldBe` 567
  either (error "invalid parser") id (parse parsePass "?" "FFFBBBFRRR") `shouldBe` 119
  either (error "invalid parser") id (parse parsePass "?" "BBFFBBFRLL") `shouldBe` 820
  return ()

