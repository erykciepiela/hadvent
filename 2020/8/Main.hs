module Main where

import Advent

import Text.Parsec as P
import Text.Parsec.String
import Data.Functor
import Data.List as L
import Control.Monad
import Data.Set as S
-- import Data.Map as M
import Data.Maybe

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

interpret :: [Instr] -> ([Int], Int) -> ([Int], Int, Bool)
interpret is (cursors, acc)
  | length (nub cursors) /= length cursors = (cursors, acc, False)
  | head cursors == length is = (cursors, acc, True)
  | otherwise = case is !! head cursors of
      (Instr "acc" n) -> interpret is ((head cursors + 1): cursors, acc + n)
      (Instr "jmp" n) -> interpret is ((head cursors + n): cursors, acc)
      (Instr "nop" _) -> interpret is ((head cursors + 1): cursors, acc)

solution1 :: String -> Int
solution1 input = let
  is = either (error "invalid parser") id $ parse instrsParser "?" input
  (_, acc, False) = interpret is ([0], 0)
  in acc

variants :: [Instr] -> [[Instr]]
variants [] = []
variants (Instr "jmp" n:rest) = ((Instr "jmp" n:) <$> variants rest) <> ((Instr "nop" n:) <$> [rest])
variants (Instr "nop" n:rest) = ((Instr "nop" n:) <$> variants rest) <> ((Instr "jmp" n:) <$> [rest])
variants (i:rest) = (i:) <$> variants rest

solution2 :: String -> Int
solution2 input = let
  is = either (error "invalid parser") id $ parse instrsParser "?" input
  in head $ mapMaybe checkVariant (variants is)
    where
      checkVariant is = let (_, acc, r) = interpret is ([0], 0) in if r then Just acc else Nothing

main :: IO ()
main = advent 2020 8 [solution2] $ do
  return ()

