module Main where

import Advent
import Data.Text as T
-- import Data.Text.Read as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Map as P
import Data.Maybe

solution2 :: String -> String
solution2 input = input

solution1 :: String -> String
solution1 input = let
    pairs = parsePair (pack input)
    in show $ sum $ S.size <$> ffix closure (P.fromAscList pairs)

closure :: Ord a => P.Map a (S.Set a) -> P.Map a (S.Set a)
closure m = (\sa -> L.foldr (\a s -> S.union s (fromMaybe S.empty (P.lookup a m))) sa sa) <$> m

ffix :: Eq a => (a -> a) -> a -> a
ffix f a = if f a == a then a else ffix f (f a)

type Orb = Text

parsePair :: Text -> [(Orb, S.Set Orb)]
parsePair t = (\os -> let [a, b] = splitOn ")" os in (b, S.singleton a)) <$> T.lines t

main :: IO ()
main = do
    print $ ffix closure (P.fromAscList [(2, S.singleton 1), (3, S.singleton 2), (4, S.singleton 3), (5, S.singleton 4)])
    runAdvent 2019 6 solution1 []

