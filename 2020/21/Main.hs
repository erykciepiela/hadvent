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

data Food = Food {
  ingredients :: [String],
  allergens :: [String]
} deriving Show

inputParser :: Parser [Food]
inputParser = food `sepEndBy` newline
  where
    food :: Parser Food
    food = do
      ingredients <- name `sepEndBy` space
      allergens <- between (string "(contains ") (string ")") (name `sepBy` string ", ")
      return $ Food ingredients allergens
        where
          name :: Parser String
          name = many1 letter

solution1 :: String -> String
solution1 input = let
  foods = either (\e -> error $ "wrong input parser: " <> show e) id $ parse inputParser "" input
  allergenToIngredients = M.fromListWith intersect [(a, is) | f <- foods, let is = ingredients f, a <- allergens f]
  allergicIngredients = determineAllergicIngredients allergenToIngredients
  safeIngredientsOccurences = sum $ foods <&> length . L.filter (`notElem` allergicIngredients) . ingredients
  in show $ safeIngredientsOccurences
    where
      determineAllergicIngredients :: M.Map String [String] -> [String]
      determineAllergicIngredients m = let
        (singletons, notSingletons) = M.partition ((== 1) . length) m
        in if F.null notSingletons then mconcat $ M.elems singletons else determineAllergicIngredients $ singletons <> (notSingletons <&> L.filter (`notElem` (mconcat $ M.elems singletons)))

main :: IO ()
main = advent 2020 21 [solution1] $ do
  return ()
