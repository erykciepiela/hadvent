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

type Addr = Int
type Value = Integer

createMask :: String -> Value -> Value
createMask mask i = F.foldl foo i (zip mask [(length mask - 1), (length mask - 2) .. 0])
  where
    foo :: Value -> (Char, Int) -> Value
    foo i ('1', n) = setBit i n
    foo i ('0', n) = clearBit i n
    foo i ('X', _) = i

inputParser :: Parser (Map Addr Value)
inputParser = do
  assocs <- many1 $ do
    string "mask = "
    mask <- createMask <$> count 36 (oneOf "01X")
    char '\n'
    try (do
      string "mem["
      addr <- read <$> many1 digit
      string "] = "
      val <- read <$> many1 digit
      return (addr, mask val)) `sepEndBy` char '\n'
  return $ M.fromList $ mconcat assocs

createMask' :: String -> Addr -> [Addr]
createMask' mask a = F.foldl foo [a] (zip mask [(length mask - 1), (length mask - 2) .. 0])
  where
    foo :: [Addr] -> (Char, Int) -> [Addr]
    foo as ('1', n) = flip setBit n <$> as
    foo as ('0', _) = as
    foo as ('X', n) = (flip setBit n <$> as) <> (flip clearBit n <$> as)

inputParser' :: Parser (Map Addr Value)
inputParser' = do
  assocs <- many1 $ do
    string "mask = "
    mask <- createMask' <$> count 36 (oneOf "01X")
    char '\n'
    mconcat <$> try (do
      string "mem["
      addr <- read <$> many1 digit
      string "] = "
      val <- read <$> many1 digit
      return (mask addr <&> (, val))) `sepEndBy` char '\n'
  return $ M.fromList $ mconcat assocs

solution1 :: String -> Integer
solution1 input = let
  mem = either (error "wrong parser") id $ parse inputParser "?" input
  in sum $ M.elems mem

solution2 :: String -> Integer
solution2 input = let
  mem = either (error "wrong parser") id $ parse inputParser' "?" input
  in sum $ M.elems mem

main :: IO ()
main = advent 2020 15 [solution2] $ do
  return ()

