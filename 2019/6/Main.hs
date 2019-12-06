module Main where

import Advent
import Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Map as P
import Data.Maybe

solution2 :: Text -> Text -> String -> String
solution2 a z input = let
    pairs = parsePair (pack input)
    m0 = P.fromList pairs
    m = ffix closure m0
    (Just start) = P.lookup a m
    (Just stop) = P.lookup z m
    in show $ snd $ L.head $ L.sortOn snd $ toList $ P.intersectionWith (+) start stop

solution1 :: String -> String
solution1 input = let
    pairs = parsePair (pack input)
    in show $ sum $ P.size <$> ffix closure (P.fromList pairs)

type Orb = Text

parsePair :: Text -> [(Orb, P.Map Orb Int)]
parsePair t = (\os -> let [a, b] = splitOn ")" os in (b, (P.singleton a 0))) <$> T.lines t

closure :: Ord a => P.Map a (P.Map a Int) -> P.Map a (P.Map a Int)
closure m = (\ma -> ma <> mconcat ((\(a, d) -> (+ (d + 1)) <$> fromMaybe mempty (P.lookup a m)) <$> P.toList ma)) <$> m

ffix :: Eq a => (a -> a) -> a -> a
ffix f a = if f a == a then a else ffix f (f a)

main :: IO ()
main = do
    runAdvent 2019 6 solution1 []
    runAdvent 2019 6 (solution2 "YOU" "SAN") []

