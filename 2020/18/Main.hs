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
import Data.Map as M

import Text.Parsec as P
import Text.Parsec.String

import Debug.Trace

expr :: Parser Int
expr = factor `chainl1` (try plus <|> try star)

plus :: Parser (Int -> Int -> Int)
plus = (+) <$ string " + "

star :: Parser (Int -> Int -> Int)
star = (*) <$ string " * "

num :: Parser Int
num = read <$> many1 digit

factor :: Parser Int
factor = par <|> num

par :: Parser Int
par = between (char '(') (char ')') expr

---
expr2 :: Parser Int
expr2 = mult `chainl1` try star

mult :: Parser Int
mult = factor2 `chainl1` try plus

factor2 :: Parser Int
factor2 = par2 <|> num

par2 :: Parser Int
par2 = between (char '(') (char ')') expr2

solution1 :: String -> Int
solution1 input = let
  parseLine line = either (error "wrong parser") id (parse (expr <* eof) "" line)
  ls = lines input
  in sum $ parseLine <$> ls

solution2 :: String -> Int
solution2 input = let
  parseLine line = either (error "wrong parser") id (parse (expr2 <* eof) "" line)
  ls = lines input
  in sum $ parseLine <$> ls

main :: IO ()
main = advent 2020 18 [solution2] $ do
  either (error "wrong parser") id (parse (expr <* eof) "" "1 + 2 * 3") `shouldBe` 9
  either (error "wrong parser") id (parse (expr <* eof) "" "1 + (2 * 3)") `shouldBe` 7
  either (error "wrong parser") id (parse (expr <* eof) ""  "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") `shouldBe` 13632
  either (error "wrong parser") id (parse (expr2 <* eof) "" "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2") `shouldBe` 23340
  return ()
