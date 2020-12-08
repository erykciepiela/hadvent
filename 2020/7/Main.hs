module Main where

import Advent

import Text.Parsec as P
import Text.Parsec.String
import Data.Functor
import Data.List as L
import Control.Monad
import Data.Set as S
import Data.Map as M

input :: String
input =
  "light red bags contain 1 bright white bag, 2 muted yellow bags.\n\
  \dark orange bags contain 3 bright white bags, 4 muted yellow bags.\n\
  \bright white bags contain 1 shiny gold bag.\n\
  \muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\n\
  \shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\n\
  \dark olive bags contain 3 faded blue bags, 4 dotted black bags.\n\
  \vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\n\
  \faded blue bags contain no other bags.\n\
  \dotted black bags contain no other bags.\n"

type Color = String

data Bag = Bag {
  color :: Color,
  subBagColors :: Map Color Int
} deriving (Show, Eq)

bagsParser :: Parser (M.Map Color Bag)
bagsParser = M.fromList . fmap (\bag -> (color bag, bag)) <$> (bagParser `sepEndBy` char '\n')

colorParser :: Parser String
colorParser = do
    mod <- many1 letter
    char ' '
    base <- many1 letter
    return $ mod <> base

bagParser :: Parser Bag
bagParser = do
  color <- colorParser
  string " bags contain "
  subBagColors <- (mempty <$ string "no other bags") <|> (do
    qty <- read <$> many1 digit
    char ' '
    color' <- colorParser
    string " bag"
    optional (char 's')
    return (color', qty)) `sepBy` string ", "
  char '.'
  return $ Bag color (M.fromList subBagColors)

trav :: [Bag] -> Set Color -> Set Color
trav [] ancestors =  ancestors
trav (bag:bags) ancestors =  trav bags (if S.size (S.intersection ancestors (keysSet (subBagColors bag))) > 0 then S.insert (color bag) ancestors else ancestors)

trav'' :: [Bag] -> Set Color -> Set Color
trav'' bags ancestors = let
    newancestors = trav bags ancestors
  in if S.size ancestors == S.size newancestors then ancestors else trav'' bags newancestors

tr :: M.Map Color Bag -> Bag -> Int
tr map bag = L.foldr (\(color, qty) acc -> acc + qty * tr map (subBag color)) 1 (M.toList (subBagColors bag))
  where
    subBag color = let Just bag = M.lookup color map in bag

solution1 :: String -> Int
solution1 input = let
  bags = either (error "invalid parser") id $ parse bagsParser "?" input
  in length (trav'' (M.elems bags) (S.singleton "shinygold")) - 1

solution2 :: String -> String
solution2 input = let
  bags = either (error "invalid parser") id $ parse bagsParser "?" input
  (Just bag) = M.lookup "shinygold" bags
  in show $ tr bags bag - 1

main :: IO ()
main = advent 2020 7 [solution2] $ do
  return ()

