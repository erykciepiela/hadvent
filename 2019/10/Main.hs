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
import Control.Comonad
import qualified Data.List.Safe as LSF
import Data.Semigroup
import Data.Foldable as F

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
rrr (dx, dy) = fromMaybe undefined $ ggg ((\n -> if ((dx `div` n) * n == dx) && ((dy `div` n) * n == dy) then Just (dx `div` n, dy `div` n) else Nothing) <$> (L.reverse [1..((max (abs dx) (abs dy)) + 1)]))

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
    
data Line a = Line {
    lineLeft :: [a],
    lineCursor :: a,
    lineRight :: [a]
}

deriving instance (Show a) => Show (Line a)
deriving instance (Eq a) => Eq (Line a)

instance Semigroup a => Semigroup (Line a) where
    l1 <> l2 = Line (L.reverse (L.zipWith (<>) (L.reverse $ lineLeft l1) (L.reverse $ lineLeft l2))) (lineCursor l1 <> lineCursor l2) (L.zipWith (<>) (lineRight l1) (lineRight l2))

line :: [a] -> Maybe (Line a)
line as = Line <$> pure [] <*> LSF.head as <*> LSF.tail as

moveBack :: Line a -> Maybe (Line a)
moveBack (Line l c r) = Line <$> LSF.tail l <*> LSF.head l <*> pure (c:r)

iterM :: (a -> Maybe a) -> a -> [a]
iterM f a = case f a of
    Nothing -> []
    Just a' -> a':iterM f a'

repeatM :: (a -> Maybe a) -> Int -> a -> Maybe a
repeatM f 0 a = Just a
repeatM f n a = case f a of
    Nothing -> Nothing
    Just a' -> repeatM f (n - 1) a'

moveForward :: Line a -> Maybe (Line a)
moveForward (Line l c r) = Line <$> pure (c:l) <*> LSF.head r <*> LSF.tail r

instance Foldable Line where
    foldr abb b (Line l c r)= Prelude.foldr abb b (L.reverse l <> [c] <> r)

instance Traversable Line where
 sequenceA (Line l c r) = Line <$> sequenceA l <*> c <*> sequenceA r

instance Functor Line where
    fmap f (Line l c r) = Line (f <$> l) (f c) (f <$> r)

instance Applicative Line where
    pure a = Line (L.repeat a) a (L.repeat a)
    l1 <*> l2 = Line (L.reverse (L.zipWith ($) (L.reverse $ lineLeft l1) (L.reverse $ lineLeft l2))) (lineCursor l1 $ lineCursor l2) (L.zipWith ($) (lineRight l1) (lineRight l2))

instance Comonad Line where
    extract = lineCursor
    duplicate l = Line (iterM moveBack l) l (iterM moveForward l)

data Grid a = Grid {
    gridLines :: Line (Line a)
}

gridFromList :: [[a]] -> Maybe (Grid a)
gridFromList ass = let lines = catMaybes (line <$> ass) in Grid <$> (Line <$> pure [] <*> LSF.head lines <*> LSF.tail lines)

grid :: Int -> Int -> Maybe (Grid (Int, Int))
grid w h = gridFromList [[(x, y) | x <- [0..(w-1)]] | y <- [0..(h-1)]]

infinigrid :: Maybe (Grid (Int, Int))
infinigrid = gridFromList [[(x, y) | x <- [0..]] | y <- [0..]]

moveUp :: Grid a -> Maybe (Grid a)
moveUp (Grid l) = Grid <$> moveBack l

moveDown :: Grid a -> Maybe (Grid a)
moveDown (Grid l) = Grid <$> moveForward l

moveLeft :: Grid a -> Maybe (Grid a)
moveLeft (Grid l) = Grid <$> sequenceA (moveBack <$> l)

moveRight :: Grid a -> Maybe (Grid a)
moveRight (Grid l) = Grid <$> sequenceA (moveForward <$> l)

translate :: (Int, Int) -> (Int, Int) -> (Int, Int)
translate (dx, dy) (x, y) = (x-dx, y-dy)

instance Foldable Grid where
    foldr abb b g = F.foldr (\l b -> F.foldr abb b l) b (gridLines g)

instance Functor Grid where
    fmap f (Grid g) = Grid $ fmap f <$> g

instance Applicative Grid where
    pure a = Grid $ Line (L.repeat (pure a)) (pure a) (L.repeat (pure a))
    g1 <*> g2 = Grid $ (<*>) <$> gridLines g1 <*> gridLines g2 

instance Comonad Grid where
    extract = extract . extract . gridLines
    duplicate g = Grid $ duplicate (Grid <$> duplicate (gridLines g))

instance Semigroup a => Semigroup (Grid a) where
    g1 <> g2 = Grid $ gridLines g1 <> gridLines g2

deriving instance (Show a) => Show (Grid a)
deriving instance (Eq a) => Eq (Grid a)

index :: Grid a -> Grid (Int, Int)
index g = Grid $ undefined 

(Just i) = infinigrid
eee g = case extract g of
    False -> []
    -- _ -> L.length $ g
    _ -> L.nub $ rrr . fst <$> (L.filter snd $ F.toList $ (,) <$> i <*> g)
    -- _ -> L.length $ L.nub $ rrr <$> L.filter (/= (0, 0)) (fst <$> (L.filter snd $ F.toList $ (,) <$> i <*> g))

main :: IO ()
main = advent 2019 10 [] $ do
    let (Just g1) = grid 2 2
    let (Just g2) = grid 2 2
    -- peek $ (,) <$> g1 <*> pure 6
    (line [Sum 1,Sum 2,Sum 3] <> line [Sum 10, Sum 20, Sum 30]) `shouldBe` line [Sum 11, Sum 22, Sum 33]
    ((fmap (\(x, y) -> (Sum x, Sum y)) <$> grid 3 3) <> (fmap (\(x, y) -> (Sum x, Sum y)) <$> grid 3 3)) `shouldBe` (fmap (\(x, y) -> (Sum (2*x), Sum (2*y))) <$> grid 3 3)
    ((line [Sum 1,Sum 2,Sum 3] >>= repeatM moveForward 2) <> line [Sum 10, Sum 20, Sum 30]) `shouldBe` (line [Sum 13])
    let h = fmap (translate (1, 1)) <$> grid 3 3
    let (Just g) = h >>= moveDown >>= moveRight
    extract g `shouldBe` (0, 0)
    (extract <$> moveRight g) `shouldBe` Just (1, 0)
    (extract <$> moveLeft g) `shouldBe` Just (-1, 0)
    (extract <$> moveUp g) `shouldBe` Just (0, -1)
    (extract <$> moveDown g) `shouldBe` Just (0, 1)
    (extract <$> (pure g >>= repeatM moveDown 2)) `shouldBe` Nothing
    let testInput = ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##" -- readFile "2019/10/inputt.txt"
    let (Just g) = gridFromList (par testInput)
    peek $ extend eee g
    -- rrr (12, 24) `shouldBe` (1, 2)
    -- rrr (12, -24) `shouldBe` (1, -2)
    -- rrr (-12, -24) `shouldBe` (-1, -2)
    -- rrr (1, 3) `shouldBe` (1, 3)
    -- rrr (2, 6) `shouldBe` (1, 3)
    -- peek $ par ".#..#\n.....\n#####\n....#\n...##" 
    -- (snd . xxx . par) ".#..#\n.....\n#####\n....#\n...##" `shouldBe` 8
    -- (snd . xxx . par) testInput `shouldBe` 210
    -- angleValueRadians (atan2Angle 0 (1)) `shouldBe` 0
    -- coordsOrd (1, 1) (par "###\n###\n###") `shouldBe` [(1, 0), (2, 0), (2, 1), (2, 2), (1, 2), (0, 2), (0, 1), (0, 0)]
    -- coordsOrd (1, 1) (par "###\n###\n###\n # ") `shouldBe` [(1, 0), (2, 0), (2, 1), (2, 2), (1, 2), (0, 2), (0, 1), (0, 0)]
    -- coordsOrd (1, 1) (par "#  \n###\n###") `shouldBe` [(2, 1), (2, 2), (1, 2), (0, 2), (0, 1), (0, 0)]
    -- solution1 testInput `shouldBe` "210"
    -- solution1' testInput `shouldBe` (11, 13)
    -- solution2 1 testInput `shouldBe` "1112"
    -- solution2 2 testInput `shouldBe` "1201"
    -- solution2 3 testInput `shouldBe` "1202"
    -- solution2 10 testInput `shouldBe` "1208"
    -- solution2 20 testInput `shouldBe` "1600"
    -- solution2 50 testInput `shouldBe` "1609"
    -- solution2 100 testInput `shouldBe` "1016"
    -- solution2 200 testInput `shouldBe` "802"
    return ()