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
import Data.Char as Ch
-- import Text.Parsec
-- import Text.Parsec.Char 
-- import Text.Parsec.Combinator as P

--

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

line' :: [a] -> Line a
line' as = Line [] (L.head as) (L.tail as)

reverseL :: Line a -> Line a
reverseL (Line l c r) = Line r c l

moveBack :: Line a -> Maybe (Line a)
moveBack (Line l c r) = Line <$> LSF.tail l <*> LSF.head l <*> pure (c:r)

moveBack' :: Line a -> Line a
moveBack' (Line l c r) = Line (L.tail l) (L.head l) (c:r)

iterM :: (a -> Maybe a) -> a -> [a]
iterM f a = case f a of
    Nothing -> []
    Just a' -> a':iterM f a'

repeatM :: (a -> a) -> Int -> a -> a
repeatM f 0 a = a
repeatM f n a = repeatM f (n - 1) (f a)

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
--

data Dir = U | D | L | R

instance Semigroup Dir where
    U <> d = d
    d <> U = d
    L <> R = U
    L <> D = R
    L <> L = D
    D <> L = R
    D <> R = L
    D <> D = U
    R <> D = L
    R <> R = D
    R <> L = U

instance Monoid Dir where
    mempty = U

data OGrid b a = OGrid {
    ogrid :: Grid a,
    ogridUp :: Dir,
    ogridb :: b
}

instance Functor (OGrid b) where
    fmap f (OGrid g d b) = OGrid (fmap f g) d b

instance Comonad (OGrid b) where
    extract = extract . ogrid
    duplicate (OGrid g d b) = OGrid ((\g -> OGrid g d b) <$> duplicate g) d b

moveOG :: Dir -> OGrid b a -> OGrid b a
moveOG d' (OGrid g d b) = OGrid (moveG (d' <> d) g) d b

turnOG :: Dir -> OGrid b a -> OGrid b a
turnOG nd (OGrid g d b) = OGrid g (nd <> d) b

dropOG :: a -> OGrid b a -> OGrid b a
dropOG a (OGrid g d b) = OGrid (dropG a g) d b

modifyOG :: (b -> b) -> OGrid b a -> OGrid b a
modifyOG f (OGrid g d b) = (OGrid g d (f b))

writeOG :: b -> OGrid b a -> OGrid b a
writeOG b (OGrid g d _) = (OGrid g d b)

readOG :: OGrid b a -> b
readOG = ogridb

newtype Grid a = Grid {
    gridLines :: Line (Line a)
}

gridFromList :: [[a]] -> Grid a
gridFromList ass = let lines = (line' <$> ass) in Grid (Line [] (L.head lines) (L.tail lines))

grid :: Int -> Int -> (Grid (Int, Int))
grid w h = gridFromList [[(x, y) | x <- [0..(w-1)]] | y <- [0..(h-1)]]

infinigrid :: (Grid (Int, Int))
infinigrid = gridFromList [[(x, y) | x <- [0..]] | y <- [0..]]
-- infinigrid = Grid (Line (negate <$> [1..]) (infiniline) ([1..]))

moveG :: Dir -> Grid a -> Grid a
moveG U (Grid l) = Grid $ moveBack' l
moveG D (Grid l) = Grid $ moveForward' l
moveG L (Grid l) = Grid (moveBack' <$> l)
moveG R (Grid l) = Grid (moveForward' <$> l)


-- translate :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- translate (dx, dy) (x, y) = (x-dx, y-dy)

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

printG :: Grid Char -> String
printG g = F.foldr (\l b -> b <> "\n" <> F.foldr (\a s -> s <> [a]) "" l) "" (gridLines g)

dropL :: a -> Line a -> Line a
dropL a (Line l c r) = Line l a r

dropG :: a -> Grid a -> Grid a
dropG a g = let (Line up l down) = gridLines g in Grid $ Line up (dropL a l) down

--

data Walk = Walk {
    wPath :: [Grid ((Int, Int), Char)],
    wKeys :: [Char],
    wl :: Int
}

startWalk :: Grid ((Int, Int), Char) -> Walk
startWalk g = Walk [g] [] 0

commitWalk :: Walk -> Walk
commitWalk (Walk g k l) = Walk [L.head g] k (l + L.length g -1)

isKey :: Char -> Bool
isKey ch = ch `L.elem` ['a'..'z']

isDoor :: Char -> Bool
isDoor ch = ch `L.elem` ['A'..'Z']

walksToNextKey :: Walk -> [Walk]
walksToNextKey (Walk path@(cursor:_) keys l) = let
    foo = (\dir -> let 
        cursor' = moveG dir cursor 
        in if (fst (extract cursor')) `L.elem` (fst. extract <$> path) then [] else case snd (extract cursor') of
            '#' -> [] --wall
            '.' -> walksToNextKey (Walk (cursor':path) keys l) -- space
            '@' -> walksToNextKey (Walk (cursor':path) keys l) -- space
            ch -> if ch `L.elem` keys
                then walksToNextKey (Walk (cursor':path) keys l) -- space
                else if isKey ch 
                    then [(Walk (cursor':path) (ch:keys) l)] -- key
                    else if Ch.toLower ch `L.elem` keys
                        then walksToNextKey (Walk (cursor':path) keys l) -- open door
                        else []) <$> [U, R, D, L] -- locked door
    walks = mconcat foo
    w' = L.nubBy (\a b -> (L.head (wKeys a)) == (L.head (wKeys b))) $ L.sortOn (L.length . wPath) walks
    in w' >>= walksToNextKey . commitWalk
    
solution1 :: String -> String
solution1 input = let
    g = (,) <$> infinigrid <*> gridFromList (L.lines input)
    g' = (L.iterate (moveG D) g) !! 40
    g'' = (L.iterate (moveG R) g') !! 40
    -- in show $ extract g3
    w0 = startWalk g''
    -- in show $ extract $ L.head $ wPath w0
    w1 = walksToNextKey w0
    in show $ L.minimum $ wl <$> L.filter (\w -> L.length (wKeys w) == 26) w1
    -- -- in show $ (extract . L.head . wPath) <$> w1
    -- in show $ L.length w1


solution2 :: String -> String
solution2 input = "?"
    
main :: IO ()
main = advent 2019 18 [solution1] $ do
    return ()
