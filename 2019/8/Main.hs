module Main where

import Advent
import Utils
import qualified Data.Text as T
import Data.Text.Read as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Map as P
import Data.Maybe
import Control.Exception
import Debug.Trace
import Data.List.Split as LS

parseDigits :: String -> [Int]
parseDigits input = (\i -> read [i]) <$> strip input

solution1 :: Int -> Int -> String -> String
solution1 w h input = show $ snd $ L.head $ L.sortOn fst $ layerValues <$> LS.chunksOf (w * h) (parseDigits input)
    where
        layerValues :: [Int] -> (Int, Int)
        layerValues layer = (cnt 0, cnt 1 * cnt 2)
            where
                cnt :: Int -> Int
                cnt = countOccurences layer

newtype Color = Color { color :: Int } deriving (Eq, Show) via Int

instance Semigroup Color where
    (Color 2) <> c = c
    c <> _ = c

composeLayers :: Int -> Int -> String -> String
composeLayers w h input = L.unlines $ LS.chunksOf w $ mconcat $ fmap show $ slist $ L.foldl1 (<>) $ SList . fmap Color <$> LS.chunksOf (w * h) (parseDigits input)

solution2 :: Int -> Int -> String -> String
solution2 w h input = render <$> composeLayers w h input
    where
        render '0' = ' '
        render '1' = 'W'
        render c = c

main :: IO ()
main = advent 2019 8 [solution1 25 6, solution2 25 6] $ do
    parseDigits "123" `shouldBe` [1,2,3]
    parseDigits "123\n" `shouldBe` [1,2,3]
    LS.chunksOf 2 [1,2,3,4,5,6] `shouldBe` [[1,2], [3,4], [5,6]]
    solution1 3 2 "012121" `shouldBe` "6"
    composeLayers 2 2 "0222112222120000" `shouldBe` "01\n10\n"
    composeLayers 1 1 "01" `shouldBe` "0\n"
    composeLayers 1 1 "10" `shouldBe` "1\n"
    composeLayers 1 1 "21" `shouldBe` "1\n"
    composeLayers 2 2 "22221111" `shouldBe` "11\n11\n"
    composeLayers 2 2 "22221010" `shouldBe` "10\n10\n"
    solution2 2 2 "0011" `shouldBe` "  \nWW\n"