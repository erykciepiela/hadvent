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
import Debug.Trace
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

printG :: Grid Char -> String
printG g = F.foldr (\l b -> b <> "\n" <> F.foldr (\a s -> s <> [a]) "" l) "" (gridLines g)

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

setElem :: [Int] -> Int -> Int -> [Int]
setElem list pos val = L.take pos list <> [val] <> L.drop (pos + 1) list


interp :: [Int] -> Int -> Int -> [Int] -> ([Int], Int, Int, Maybe Int)
interp _ c rb [] = ([], c, rb, Nothing)
interp i c rb l = case l !! c of
    99 -> (l, 0, rb, Nothing)
    -- 0 0 0 01
    1 ->   let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) + l !! (l !! (c+2))) in interp i (c + 4) rb nl
    -- 2 2 2 01
    22201 ->   let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) + l !! (rb + l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 0 1 01
    101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! (l !! (c+2))) in interp i (c + 4) rb nl
    -- 2 2 1 01
    22101 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c + 1)) + l !! (rb + l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 1 0 01
    1001 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c + 1)) + l !! ((c+2))) in interp i (c + 4) rb nl
    -- 0 1 0 01
    1201 -> let nl = setElem l (l !! (c + 3)) (l !! (rb + l !! (c + 1)) + l !! ((c+2))) in interp i (c + 4) rb nl
    -- 2 1 0 01
    21201 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c + 1)) + l !! ((c+2))) in interp i (c + 4) rb nl
    -- 0 1 1 01
    1101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp i (c + 4) rb nl
    -- 2 1 1 01
    21101 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp i (c + 4) rb nl
    -- 2 1 1 01
    2101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! (rb + l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 0 0 02
    2 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! (l !! (c+2))) in interp i (c + 4) rb nl
    -- 2 2 2 02
    22202 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! (rb + l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 0 1 02
    102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! (l !! (c+2))) in interp i (c + 4) rb nl
    -- 2 2 1 02
    22102 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c +1)) * l !! (rb + l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 2 1 02
    2102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! (rb + l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 1 0 02
    1002 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! ((c+2))) in interp i (c + 4) rb nl
    -- 0 1 2 02
    1202 -> let nl = setElem l (l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! ((c+2))) in interp i (c + 4) rb nl
    -- 2 1 2 02
    21202 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! ((c+2))) in interp i (c + 4) rb nl
    -- 0 1 1 02
    1102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp i (c + 4) rb nl
    -- 2 1 1 02
    21102 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp i (c + 4) rb nl
    -- 0 0 0 03
    3 -> case i of
        [] -> (l, c, rb, Nothing)
        (fi:ri) -> let nl = setElem l (l !! (c + 1)) fi in interp ri (c + 2) rb nl
    -- 0 0 2 03
    203 -> case i of
        [] -> (l, c, rb, Nothing)
        (fi:ri) -> let nl = setElem l (rb + (l !! (c + 1))) fi in interp ri (c + 2) rb nl
    -- 0 0 0 04 
    4 -> let o = (l !! (l !! (c + 1))) in (l, (c + 2), rb, Just o)
    -- 0 0 2 04
    204 -> let o = (l !! (rb + l !! (c + 1))) in (l, (c + 2), rb, Just o)
    -- 0 0 1 04
    104 -> let o = (l !! (c + 1)) in (l, (c + 2), rb, Just o)
    -- 0 0 0 05
    5 -> if l !! (l !! (c + 1)) /= 0 then interp i (l !! (l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 0 1 05
    105 -> if l !! ((c + 1)) /= 0 then interp i (l !! (l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 2 1 05
    2105 -> if l !! ((c + 1)) /= 0 then interp i (l !! (rb + l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 1 0 05
    1005 -> if l !! (l !! (c + 1)) /= 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 0 1 2 05
    1205 -> if l !! (rb + l !! (c + 1)) /= 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 1 1 0 05
    1105 -> if l !! ((c + 1)) /= 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 0 0 0 06
    6 -> if l !! (l !! (c + 1)) == 0 then interp i (l !! (l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 0 1 06
    106 -> if l !! ((c + 1)) == 0 then interp i (l !! (l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 2 1 06
    2106 -> if l !! ((c + 1)) == 0 then interp i (l !! (rb + l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 1 0 06
    1006 -> if l !! (l !! (c + 1)) == 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 0 1 2 06
    1206 -> if l !! (rb + l !! (c + 1)) == 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 1 1 0 06
    1106 -> if l !! ((c + 1)) == 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 0 0 0 07
    7 -> if l !! (l !! (c + 1)) < (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 2 2 07
    2207 -> if l !! (rb + l !! (c + 1)) < (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 0 1 07
    107 -> if l !! ((c + 1)) < (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 2 1 07
    2107 -> if l !! ((c + 1)) < (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 0 07
    1007 -> if l !! (l !! (c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 2 07
    1207 -> if l !! (rb + l !! (c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 1 07
    1107 -> if l !! ((c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 2 1 1 07
    21107 -> if l !! ((c + 1)) < (l !! ((c + 2))) then let nl = setElem l (rb + l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (rb + l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 0 0 08
    8 -> if l !! (l !! (c + 1)) == (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 0 1 08
    108 -> if l !! ((c + 1)) == (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 2 1 08
    2108 -> if l !! ((c + 1)) == (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 0 08
    1008 -> if l !! (l !! (c + 1)) == (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 2 08
    1208 -> if l !! (rb + l !! (c + 1)) == (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 1 08
    1108 -> if l !! ((c + 1)) == (l !!((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 2 1 1 08
    21108 -> if l !! ((c + 1)) == (l !!((c + 2))) then let nl = setElem l (rb + l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (rb + l !! (c + 3)) 0 in interp i (c + 4) rb nl
    9 -> interp i (c + 2) (rb + (l !! (l !! (c+1)))) l
    109 -> interp i (c + 2) (rb + (l !! (c+1))) l
    209 -> interp i (c + 2) (rb + (l !! (rb + (l !! (c+1))))) l
    opcode -> error ("Opcode not found " <> show opcode)

interp' :: [Int] -> Int -> Int -> [Int] -> [Int] -> [Int]
interp' ins c rb prog os = let 
    (prog1, c1, rb1, mo) = interp ins c rb prog
    in case mo of
        Nothing -> os
        Just o -> interp' ins c1 rb1 prog1 (os<>[o])

-- interp :: Int -> [Int]
-- interp i = [0, 1] -- 1 - turn right, 0 - turn left

changeDir:: (Int, Int) -> Int -> (Int, Int)
changeDir (0, -1) 1 = (1, 0) 
changeDir (0, -1) 0 = (-1, 0) 
changeDir (0, 1) 1 = (-1, 0) 
changeDir (0, 1) 0 = (1, 0) 
changeDir (1, 0) 1 = (0, 1) 
changeDir (1, 0) 0 = (0, -1) 
changeDir (-1, 0) 1 = (0, -1) 
changeDir (-1, 0) 0 = (0, 1) 

par :: String -> [Int]
par input = read . T.unpack <$> T.splitOn "," (T.strip (T.pack input))

rap :: [Int] -> String
rap is = L.intercalate "," $ show <$> is

solution1 :: String -> String
solution1 input = let
    g = pure (0, False) -- pure black grid unpainted
    dir = (0, -1)
    prog = par input <> L.repeat 0
    in show $ fst $ foo (prog, 0, 0, dir, 0, g, (0, 0))

solution2 :: String -> String
solution2 input = let
    ig = pure (0, False) -- pure black grid unpainted
    g = dropG (1, False) ig
    dir = (0, -1)
    prog = par input <> L.repeat 0
    gf = (\x -> if x == 0 then ' ' else '#') . fst <$> (snd $ foo (prog, 0, 0, dir, 0, g, (0,0)))
    (Just back) = grid 100 10 >>= repeatM moveRight 50 >>= repeatM moveDown 5
    in printG $ back *> gf

foo :: ([Int], Int, Int, (Int, Int), Int, Grid (Int, Bool), (Int, Int)) -> (Int, Grid (Int, Bool))
foo (prog, c, rb, dir, cnt, g, p) = let
    (intcode, painted) = extract g
    (prog', c', rb', mintcode') = interp [intcode] c rb prog 
    in case mintcode' of
        Nothing -> (cnt, g)
        Just intcode' -> let
            (prog'', c'', rb'', mdir') = interp [intcode] c' rb' prog'
            in case mdir' of
                Nothing -> (cnt, g)
                Just lr -> let
                    g' = dropG (intcode', True) g
                    dir' = changeDir dir lr
                    (Just g'') = move dir' g'
                    in foo (prog'', c'', rb'', dir', if painted then cnt else cnt + 1, g'', (\(px, py) (dx, dy) -> (px + dx, py + dy)) p dir')

main :: IO ()
main = advent 2019 11 [solution1, solution2] $ do
    -- peek $ dropL 9 <$> (line [0,1,2] >>= moveForward)
    -- peek $ dropG 9 <$> (gridFromList [[0,1,2], [0,1,2], [0,1,2]] >>= moveRight >>= moveDown)
    peek $ extract $ (pure True :: Grid Bool)
    peek $ extract <$> (moveDown $ pure True)
    peek $ extract <$> (moveUp $ pure True)
    peek $ extract <$> (moveLeft $ pure True)
    peek $ extract <$> (moveRight $ pure True)
    return ()