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
import Debug.Trace
import Data.List.Split as LS
import Data.Angle

asteroidMap = undefined

isEmpty = undefined '.'
containsAsteroid = undefined '#'

par :: String -> [[Bool]]
par s = fmap (== '#') <$> (L.lines s) 

sight :: [[Bool]] -> (Int, Int) -> Int
sight am (x, y) = f [(dx, dy) | 
    dx <- [(- (L.length am)) .. (L.length am)],
    dy <- [(- (L.length (L.head am))) .. (L.length (L.head am))],
    (dx, dy) /= (0, 0),
    dx + x < L.length am,
    dx + x >= 0,
    dy + y < L.length (L.head am),
    dy + y >= 0,
    (am !! (y+dy)) !! (x+dx)
    ]
    where
        f :: [(Int, Int)] -> Int
        f dxy =  L.length (L.nub $ rrr <$> dxy)

rrr :: (Int, Int) -> (Int, Int)
rrr (dx, dy) = fromMaybe undefined $ ggg ((\n -> if ((dx `div` n) * n == dx) && ((dy `div` n) * n == dy) then Just (dx `div` n, dy `div` n) else Nothing) <$> (L.reverse [1..(max (abs dx) (abs dy))]))

ggg :: [Maybe a] -> Maybe a
ggg [] = Nothing
ggg (Just a:mas) = Just a
ggg (Nothing:mas) = ggg mas

xxx :: [[Bool]] -> ((Int, Int), Int)
xxx am = L.head $ L.reverse $ L.sortOn snd $ (\c -> (c, sight am c)) <$> [(x, y) | 
    x <- [0 .. (L.length am - 1)],
    y <- [0 .. (L.length (L.head am) - 1)],
    (am !! y) !! x
    ] 

solution1 :: String -> String
solution1 input = show $ snd $ xxx $ par input

solution1' :: String -> (Int, Int)
solution1' input = fst $ xxx $ par input

solution2 :: Int -> String -> String
solution2 i str = let 
    am = (par str)
    c = fst $ xxx am
    (x, y) = s2 c am !! i 
    in show $ 100*x + y

s2 :: (Int, Int) -> [[Bool]] -> [(Int, Int)]
s2 c am = (5, 5):s2 c am

main :: IO ()
main = advent 2019 10 [solution2 200] $ do
    let testInput = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##" -- readFile "2019/10/inputt.txt"
    rrr (12, 24) `shouldBe` (1, 2)
    rrr (12, -24) `shouldBe` (1, -2)
    rrr (-12, -24) `shouldBe` (-1, -2)
    rrr (1, 3) `shouldBe` (1, 3)
    rrr (2, 6) `shouldBe` (1, 3)
    peek $ par ".#..#\n.....\n#####\n....#\n...##" 
    (snd . xxx . par) ".#..#\n.....\n#####\n....#\n...##" `shouldBe` 8
    (snd . xxx . par) testInput `shouldBe` 210
    solution1 testInput `shouldBe` "210"
    solution1' testInput `shouldBe` (11, 13)
    solution2 1 testInput `shouldBe` "1112"
    -- solution2 2 testInput `shouldBe` "1201"
    -- solution2 3 testInput `shouldBe` "1202"
    -- solution2 10 testInput `shouldBe` "1208"
    -- solution2 100 testInput `shouldBe` "1016"
    -- solution2 200 testInput `shouldBe` "802"
    return ()