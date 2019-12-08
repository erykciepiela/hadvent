module Main where

import Advent
import Data.Text as T
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
parseDigits input = (\i -> read [i]) <$> (T.unpack . T.strip . T.pack) input

solution1 :: Int -> Int -> String -> String
solution1 w h input = show $ snd $ L.head $ L.sortOn fst $ layerValues <$> LS.chunksOf (w * h) (parseDigits input)
    where
        layerValues :: [Int] -> (Int, Int)
        layerValues layer = ((L.length . L.elemIndices 0) layer, (L.length . L.elemIndices 1) layer * (L.length . L.elemIndices 2) layer)

newtype Color = Color { color :: Int } deriving (Eq, Show) via Int

instance Semigroup Color where
    c1 <> c2 = case color c1 of
        0 -> c1
        1 -> c1
        2 -> c2
    
newtype SList a = SList { slist :: [a] } 

instance Semigroup a => Semigroup (SList a) where
    l1 <> l2 = SList $ L.foldl1 (<>) <$> L.transpose [slist l1, slist l2]

solution2 :: Int -> Int -> String -> String
solution2 w h input = L.unlines $ LS.chunksOf w $ mconcat $ fmap show $ slist $ L.foldl1 (<>) $ SList . fmap Color <$> LS.chunksOf (w * h) (parseDigits input)

main :: IO ()
main = advent 2019 8 [solution1 25 6, solution2 25 6] $ do
    parseDigits "123" `shouldBe` [1,2,3]
    parseDigits "123\n" `shouldBe` [1,2,3]
    LS.chunksOf 3 [1,2,3,4,5,6,7,8,9] `shouldBe` [[1,2,3], [4,5,6], [7,8,9]]
    solution1 3 2 "012121" `shouldBe` "6"
    solution2 2 2 "0222112222120000" `shouldBe` "01\n10\n"
    solution2 1 1 "01" `shouldBe` "0\n"
    solution2 1 1 "11" `shouldBe` "1\n"
    solution2 1 1 "21" `shouldBe` "1\n"
    solution2 2 2 "22221111" `shouldBe` "11\n11\n"
    solution2 2 2 "22221010" `shouldBe` "10\n10\n"

-- answer two pretty-printed
-- 1111 0 1001 0 0110 0 1001 0 10000
-- 1000 0 1010 0 1001 0 1001 0 10000
-- 1110 0 1100 0 1001 0 1111 0 10000
-- 1000 0 1010 0 1111 0 1001 0 10000
-- 1000 0 1010 0 1001 0 1001 0 10000
-- 1000 0 1001 0 1001 0 1001 0 11110