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
--
data Line a = Line {
    lineLeft :: [a],
    lineCursor :: a,
    lineRight :: [a]
}

deriving instance (Show a) => Show (Line a)
deriving instance (Eq a) => Eq (Line a)

instance Semigroup a => Semigroup (Line a) where
    l1 <> l2 = Line (L.zipWith (<>) (lineLeft l1) (lineLeft l2)) (lineCursor l1 <> lineCursor l2) (L.zipWith (<>) (lineRight l1) (lineRight l2))

line :: [a] -> Maybe (Line a)
line as = Line <$> pure [] <*> LSF.head as <*> LSF.tail as

moveBack :: Line a -> Maybe (Line a)
moveBack (Line l c r) = Line <$> LSF.tail l <*> LSF.head l <*> pure (c:r)

moveBack' :: Line a -> Line a
moveBack' (Line l c r) = Line (L.tail l) (L.head l) (c:r)

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

moveForward' :: Line a -> Line a
moveForward' (Line l c r) = Line (c:l) (L.head r) (L.tail r)

instance Foldable Line where
    foldr abb b (Line l c r)= Prelude.foldr abb b (L.reverse l <> [c] <> r)

instance Traversable Line where
 sequenceA (Line l c r) = Line <$> sequenceA l <*> c <*> sequenceA r

instance Functor Line where
    fmap f (Line l c r) = Line (f <$> l) (f c) (f <$> r)

instance Applicative Line where
    pure a = Line (L.repeat a) a (L.repeat a)
    l1 <*> l2 = Line (L.zipWith ($) (lineLeft l1) (lineLeft l2)) (lineCursor l1 $ lineCursor l2) (L.zipWith ($) (lineRight l1) (lineRight l2))

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
moveLeft (Grid l) = Just $ Grid (moveBack' <$> l)

moveRight :: Grid a -> Maybe (Grid a)
moveRight (Grid l) = Just $ Grid (moveForward' <$> l)

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

dropL :: a -> Line a -> Line a
dropL a (Line l c r) = Line l a r

dropG :: a -> Grid a -> Grid a
dropG a g = let (Line up l down) = gridLines g in Grid $ Line up (dropL a l) down

move :: (Int, Int) -> (Grid a -> Maybe (Grid a))
move (0, -1) = moveUp
move (0, 1) = moveDown
move (1, 0) = moveRight
move (-1, 0) = moveLeft

--

type IState = (Int, [Int])

interp :: Int -> [Int]
interp i = [0, 1] -- 1 - turn right, 0 - turn left

changeDir:: (Int, Int) -> Int -> (Int, Int)
changeDir (0, -1) 1 = (1, 0) 
changeDir (0, -1) 0 = (-1, 0) 
changeDir (0, 1) 1 = (-1, 0) 
changeDir (0, 1) 0 = (1, 0) 
changeDir (1, 0) 1 = (0, 1) 
changeDir (1, 0) 0 = (0, 1) 
changeDir (-1, 0) 1 = (0, -1) 
changeDir (-1, 0) 0 = (0, 1) 

solution1 :: String -> String
solution1 input = let
    g = pure (0, False) -- pure black grid unpainted
    dir = (0, -1)
    (dir', cnt', g') = foo (dir, 0, g)
    in show $ (\(_, c, _) -> c) ((L.iterate foo (dir, 0, g)) !! 100)

foo :: ((Int, Int), Int, Grid (Int, Bool)) -> ((Int, Int), Int, Grid (Int, Bool))
foo (dir, cnt, g) = let
    (intcode, painted) = extract g
    [intcode', lr] = interp intcode
    g' = dropG (intcode', True) g
    dir' = changeDir dir lr
    (Just g'') = move dir' g'
    in (dir', if painted then cnt else cnt + 1, g'')

solution2 :: String -> String
solution2 input = "?"

main :: IO ()
main = advent 2019 11 [] $ do
    -- peek $ dropL 9 <$> (line [0,1,2] >>= moveForward)
    -- peek $ dropG 9 <$> (gridFromList [[0,1,2], [0,1,2], [0,1,2]] >>= moveRight >>= moveDown)
    peek $ extract $ (pure True :: Grid Bool)
    peek $ extract <$> (moveDown $ pure True)
    peek $ extract <$> (moveUp $ pure True)
    peek $ extract <$> (moveLeft $ pure True)
    peek $ extract <$> (moveRight $ pure True)
    peek $ solution1 ""
    return ()