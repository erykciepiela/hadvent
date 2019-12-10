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

g :: [(Int, Int)] -> [(Int, Int)]
g dxy = (L.nub $ rrr <$> dxy)


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
    (x, y) = s2 c am !! (i - 1) 
    in show $ 100*x + y

s2 :: (Int, Int) -> [[Bool]] -> [(Int, Int)]
s2 c am = let
    coords = coordsOrd c am 
    am' = zero am coords
    in coords <> undefined -- s2 c am'
        where 
            zero am coords = undefined

coordsOrd :: (Int, Int) -> [[Bool]] -> [(Int, Int)]
coordsOrd (x, y) am = let
    -- am = par s
    amw = L.length (L.head am)
    amh = L.length am
    coords = [(x1, y1) |
        x1 <- [-x .. (amw-x-1)],
        y1 <- [-y .. (amh-y-1)],
        (x1, y1) /= (0, 0),
        (am !! (y1 + y)) !! (x1 + x)
        ]
        -- L.nub $ rrr <$>
    in (\(x1, y1) -> (x1 + x, y1 + y)) <$> L.sortOn ang (L.nub $ rrr <$> coords)
        where
        ang (x, y) = let 
            a = angleValueRadians (atan2Angle (fromIntegral x) (fromIntegral (-y)))
            in if a < 0 then a + 2*pi else a
    

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
    angleValueRadians (atan2Angle 0 (1)) `shouldBe` 0
    coordsOrd (1, 1) (par "###\n###\n###") `shouldBe` [(1, 0), (2, 0), (2, 1), (2, 2), (1, 2), (0, 2), (0, 1), (0, 0)]
    coordsOrd (1, 1) (par "###\n###\n###\n # ") `shouldBe` [(1, 0), (2, 0), (2, 1), (2, 2), (1, 2), (0, 2), (0, 1), (0, 0)]
    coordsOrd (1, 1) (par "#  \n###\n###") `shouldBe` [(2, 1), (2, 2), (1, 2), (0, 2), (0, 1), (0, 0)]
    solution1 testInput `shouldBe` "210"
    solution1' testInput `shouldBe` (11, 13)
    solution2 1 testInput `shouldBe` "1112"
    solution2 2 testInput `shouldBe` "1201"
    solution2 3 testInput `shouldBe` "1202"
    solution2 10 testInput `shouldBe` "1208"
    solution2 20 testInput `shouldBe` "1600"
    solution2 50 testInput `shouldBe` "1609"
    solution2 100 testInput `shouldBe` "1016"
    solution2 200 testInput `shouldBe` "802"
    return ()