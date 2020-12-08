module Main where

import Advent

import Text.Parsec as P
import Text.Parsec.String
import Data.Functor
import Data.List as L
import Control.Monad
import Data.Set as S
import Data.Map as M

type Color = String

data Instr = Instr {
  name :: String,
  arg :: Int
} deriving (Show, Eq)

instrsParser :: Parser [Instr]
instrsParser = instrParser `sepEndBy` char '\n'

instrParser :: Parser Instr
instrParser = do
    name <- count 3 letter
    char ' '
    sign <- 1 <$ char '+' <|> (-1) <$ char '-'
    number <- read <$> many1 digit
    return $ Instr name (sign * number)

foldInstrs :: [Instr] -> ([Int], Int) -> ([Int], Int)
foldInstrs is (cursors, acc) = if length (nub cursors) /= length cursors then (cursors, acc) else case is !! head cursors of
  (Instr "acc" n) -> foldInstrs is ((head cursors + 1): cursors, acc + n)
  (Instr "jmp" n) -> foldInstrs is ((head cursors + n): cursors, acc)
  (Instr "nop" _) -> foldInstrs is ((head cursors + 1): cursors, acc)

solution1 :: String -> Int
solution1 input = let
  is = either (error "invalid parser") id $ parse instrsParser "?" input
  -- in show instrs
  in snd $ foldInstrs is ([0], 0)

main :: IO ()
main = advent 2020 8 [solution1] $ do
  return ()

