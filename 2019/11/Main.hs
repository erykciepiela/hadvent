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

section :: [a] -> Line a
section as = Line [] (L.head as) (L.tail as)

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
gridFromList ass = let lines = (section <$> ass) in Grid (Line [] (L.head lines) (L.tail lines))

grid :: Int -> Int -> (Grid (Int, Int))
grid w h = gridFromList [[(x, y) | x <- [0..(w-1)]] | y <- [0..(h-1)]]

infinigrid :: (Grid (Int, Int))
infinigrid = gridFromList [[(x, y) | x <- [0..]] | y <- [0..]]

-- rotateRight :: Grid a -> Grid a
-- rotateRight (Grid ll@(Line upls cl downls)) = Grid (Line upls' cl' downls')
--     where
--         upls' = reverseL . fmap extract <$> L.tail (L.iterate (fmap moveBack') ll)-- L.tail $ extract <$> (L.iterate (fmap moveForward') ll)
--         cl' = diagr ll
--         downls' = reverseL . fmap extract <$> L.tail (L.iterate (fmap moveForward') ll)

-- diagr :: Line (Line a) -> Line a
-- diagr (Line uls cl dls) = Line (extract <$> uls) (extract cl) (extract <$> dls)

-- diagl :: Line (Line a) -> Line a
-- diagl (Line uls cl dls) = Line (extract <$> dls) (extract cl) (extract <$> uls)

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

setElem :: [Int] -> Int -> Int -> [Int]
setElem list pos val = L.take pos list <> [val] <> L.drop (pos + 1) list

data Intcode = Intcode {
    icCursor :: Int,
    icReadBase :: Int,
    icInstrs :: [Int]
}

interp :: [Int] -> Intcode -> Maybe (Intcode, Int)
interp i intcode = let (Intcode c rb l) = intcode in case l !! c of
    99 -> Nothing
    -- 0 0 0 01
    1 ->   let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) + l !! (l !! (c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 2 2 2 01
    22201 ->   let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) + l !! (rb + l !! (c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 0 1 01
    101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! (l !! (c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 2 2 1 01
    22101 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c + 1)) + l !! (rb + l !! (c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 1 0 01
    1001 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c + 1)) + l !! ((c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 1 0 01
    1201 -> let nl = setElem l (l !! (c + 3)) (l !! (rb + l !! (c + 1)) + l !! ((c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 2 1 0 01
    21201 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c + 1)) + l !! ((c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 1 1 01
    1101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 2 1 1 01
    21101 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 2 1 1 01
    2101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! (rb + l !! (c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 0 0 02
    2 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! (l !! (c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 2 2 2 02
    22202 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! (rb + l !! (c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 0 1 02
    102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! (l !! (c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 2 2 1 02
    22102 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c +1)) * l !! (rb + l !! (c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 2 1 02
    2102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! (rb + l !! (c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 1 0 02
    1002 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! ((c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 1 2 02
    1202 -> let nl = setElem l (l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! ((c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 2 1 2 02
    21202 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! ((c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 1 1 02
    1102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 2 1 1 02
    21102 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp i $ Intcode (c + 4) rb nl
    -- 0 0 0 03
    3 -> case i of
        [] -> Nothing
        (fi:ri) -> let nl = setElem l (l !! (c + 1)) fi in interp ri $ Intcode (c + 2) rb nl
    -- 0 0 2 03
    203 -> case i of
        [] -> Nothing
        (fi:ri) -> let nl = setElem l (rb + (l !! (c + 1))) fi in interp ri $ Intcode (c + 2) rb nl
    -- 0 0 0 04
    4 -> let o = (l !! (l !! (c + 1))) in Just (Intcode (c + 2) rb l, o)
    -- 0 0 2 04
    204 -> let o = (l !! (rb + l !! (c + 1))) in Just (Intcode (c + 2) rb l, o)
    -- 0 0 1 04
    104 -> let o = (l !! (c + 1)) in Just (Intcode (c + 2) rb l, o)
    -- 0 0 0 05
    5 -> if l !! (l !! (c + 1)) /= 0 then interp i $ Intcode (l !! (l !! (c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 0 0 1 05
    105 -> if l !! ((c + 1)) /= 0 then interp i $ Intcode (l !! (l !! (c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 0 2 1 05
    2105 -> if l !! ((c + 1)) /= 0 then interp i $ Intcode (l !! (rb + l !! (c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 0 1 0 05
    1005 -> if l !! (l !! (c + 1)) /= 0 then interp i $ Intcode (l !! ((c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 0 1 2 05
    1205 -> if l !! (rb + l !! (c + 1)) /= 0 then interp i $ Intcode (l !! ((c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 1 1 0 05
    1105 -> if l !! ((c + 1)) /= 0 then interp i $ Intcode (l !! ((c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 0 0 0 06
    6 -> if l !! (l !! (c + 1)) == 0 then interp i $ Intcode (l !! (l !! (c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 0 0 1 06
    106 -> if l !! ((c + 1)) == 0 then interp i $ Intcode (l !! (l !! (c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 0 2 1 06
    2106 -> if l !! ((c + 1)) == 0 then interp i $ Intcode (l !! (rb + l !! (c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 0 1 0 06
    1006 -> if l !! (l !! (c + 1)) == 0 then interp i $ Intcode (l !! ((c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 0 1 2 06
    1206 -> if l !! (rb + l !! (c + 1)) == 0 then interp i $ Intcode (l !! ((c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 1 1 0 06
    1106 -> if l !! ((c + 1)) == 0 then interp i $ Intcode (l !! ((c + 2))) rb l else interp i $ Intcode (c + 3) rb l
    -- 0 0 0 07
    7 -> if l !! (l !! (c + 1)) < (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 2 2 07
    2207 -> if l !! (rb + l !! (c + 1)) < (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 0 1 07
    107 -> if l !! ((c + 1)) < (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 2 1 07
    2107 -> if l !! ((c + 1)) < (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 1 0 07
    1007 -> if l !! (l !! (c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 1 2 07
    1207 -> if l !! (rb + l !! (c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 1 1 07
    1107 -> if l !! ((c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 2 1 1 07
    21107 -> if l !! ((c + 1)) < (l !! ((c + 2))) then let nl = setElem l (rb + l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (rb + l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 0 0 08
    8 -> if l !! (l !! (c + 1)) == (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 0 1 08
    108 -> if l !! ((c + 1)) == (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 2 1 08
    2108 -> if l !! ((c + 1)) == (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 1 0 08
    1008 -> if l !! (l !! (c + 1)) == (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 1 2 08
    1208 -> if l !! (rb + l !! (c + 1)) == (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 1 1 08
    1108 -> if l !! ((c + 1)) == (l !!((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 2 1 1 08
    21108 -> if l !! ((c + 1)) == (l !!((c + 2))) then let nl = setElem l (rb + l !! (c + 3)) 1 in interp i $ Intcode (c + 4) rb nl else let nl = setElem l (rb + l !! (c + 3)) 0 in interp i $ Intcode (c + 4) rb nl
    -- 0 0 0 09
    9 -> interp i $ Intcode (c + 2) (rb + (l !! (l !! (c+1)))) l
    -- 0 0 1 09
    109 -> interp i $ Intcode (c + 2) (rb + (l !! (c+1))) l
    -- 0 0 2 09
    209 -> interp i $ Intcode (c + 2) (rb + (l !! (rb + (l !! (c+1))))) l
    opcode -> error ("Opcode not found " <> show opcode)

walk :: (Intcode, OGrid Int (Int, Bool)) -> OGrid Int (Int, Bool)
walk (intcode, g) = let
    (i, painted) = extract g
    in case interp [i] intcode of
        Nothing -> g
        Just (intcode', color) -> case interp [color] intcode' of
            Nothing -> g
            Just (intcode'', lr) -> let
                d = case lr of 0 -> L; 1 -> R
                g' = (modifyOG (if painted then id else (+ 1)) . moveOG U . turnOG d . dropOG (color, True)) g
                in walk (intcode'', g')

parseInput :: String -> [Int]
parseInput input = read . T.unpack <$> T.splitOn "," (T.strip (T.pack input))

solution1 :: String -> String
solution1 input = let
    prog = parseInput input

    in show $ readOG $ walk (Intcode 0 0 prog, OGrid (pure (0, False)) U 0)

solution2 :: String -> String
solution2 input = let
    prog = parseInput input <> L.repeat 0
    g = dropOG (1, False) (OGrid (pure (0, False)) U 0)
    g' = (\x -> if x == 0 then ' ' else '#') . fst <$> walk (Intcode 0 0 prog, g)
    back = (repeatM (moveG R) 50  . repeatM (moveG D) 5) (grid 100 10)
    in printG $ back *> ogrid g'

main :: IO ()
main = advent 2019 11 [solution1, solution2] $ do
    return ()