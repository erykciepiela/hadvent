module Main where

import Advent
import Utils
import Data.Text as T
import Data.Text.Read as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Map as M
import Data.Maybe
import Control.Exception
import Data.List.Split as LS
import Data.Angle
import Control.Comonad
import qualified Data.List.Safe as LSF
import Data.Semigroup
import Data.Foldable as F
import Debug.Trace
import Data.Functor
import Data.ByteString.Char8 as C
import Text.Parsec
import Text.Parsec.Char 
import Text.Parsec.Combinator as P

fft :: [Int] -> Int -> [Int]
fft is 0 = is
fft is phn = let
    phs = (\n -> L.take (L.length is) $ L.tail $ L.cycle (L.replicate n 0 <> L.replicate n 1 <> L.replicate n 0 <> L.replicate n (-1))) <$> [1..(L.length is)]
    rs = (\p -> (`mod` 10) $ abs $ L.sum $ L.zipWith (*) is p) <$> phs
    in (fft rs (phn - 1))

sol1 :: String -> Int -> String
sol1 input phn = let
    ints = read . (\c -> [c]) <$> input
    in L.take 8 $ mconcat $ show <$> fft ints phn

solution1 :: String -> String
solution1 input = L.take 8 $ sol1 input 100

fft' :: [Int] -> Int -> [Int]
fft' is 0 = is
fft' is phn = let
    rss = L.scanr1 (\a b -> a + b `mod` 10) is
    in fft' rss (phn - 1)

solution2 :: String -> String
solution2 input = let
    ints = read . (\c -> [c]) <$> input
    in mconcat $ show  <$> (L.take 8 $ L.drop 5975589 $ fft' (mconcat (L.replicate 10000 ints)) 100)
    
main :: IO ()
main = advent 2019 16 [solution1, solution2] $ do
    sol1 "12345678" 4 `shouldBe` "01029498"
    sol1 "80871224585914546619083218645595" 100 `shouldBe` "24176176"
    sol1 "19617804207202209144916044189917" 100 `shouldBe` "73745418"
    sol1 "69317163492948606335995924319873" 100 `shouldBe` "52432133"
    L.scanr1 (+) [4,3,2,1] `shouldBe` [10,6,3,1]
    return ()
