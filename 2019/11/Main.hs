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

data OGrid a = OGrid {
    ogrid :: Grid a,
    ogridUp :: Dir
}

instance Functor OGrid where
    fmap f (OGrid g d) = OGrid (fmap f g) d

instance Comonad OGrid where
    extract = extract . ogrid
    duplicate (OGrid g d) = OGrid ((\g -> OGrid g d) <$> duplicate g) d

moveOG :: Dir -> OGrid a -> OGrid a
moveOG d' (OGrid g d) = OGrid (moveG (d' <> d) g) d

turnOG :: Dir -> OGrid a -> OGrid a
turnOG nd (OGrid g d) = OGrid g (nd <> d)

dropOG :: a -> OGrid a -> OGrid a
dropOG a (OGrid g d) = OGrid (dropG a g) d

data Grid a = Grid {
    gridLines :: Line (Line a)
}

gridFromList :: [[a]] -> Maybe (Grid a)
gridFromList ass = let lines = catMaybes (line <$> ass) in Grid <$> (Line <$> pure [] <*> LSF.head lines <*> LSF.tail lines)

grid :: Int -> Int -> Maybe (Grid (Int, Int))
grid w h = gridFromList [[(x, y) | x <- [0..(w-1)]] | y <- [0..(h-1)]]

infinigrid :: Maybe (Grid (Int, Int))
infinigrid = gridFromList [[(x, y) | x <- [0..]] | y <- [0..]]

rotateRight :: Grid a -> Grid a
rotateRight (Grid ll@(Line upls cl downls)) = Grid (Line upls' cl' downls')
    where
        upls' = reverseL . fmap extract <$> L.tail (L.iterate (fmap moveBack') ll)-- L.tail $ extract <$> (L.iterate (fmap moveForward') ll)
        cl' = diagr ll
        downls' = reverseL . fmap extract <$> L.tail (L.iterate (fmap moveForward') ll)

diagr :: Line (Line a) -> Line a
diagr (Line uls cl dls) = Line (extract <$> uls) (extract cl) (extract <$> dls)

diagl :: Line (Line a) -> Line a
diagl (Line uls cl dls) = Line (extract <$> dls) (extract cl) (extract <$> uls)

moveG :: Dir -> Grid a -> Grid a
moveG U = moveUp'
moveG D = moveDown'
moveG L = moveLeft'
moveG R = moveRight'

moveUp :: Grid a -> Maybe (Grid a)
moveUp (Grid l) = Grid <$> moveBack l

moveUp' :: Grid a -> (Grid a)
moveUp' (Grid l) = Grid $ moveBack' l

moveDown :: Grid a -> Maybe (Grid a)
moveDown (Grid l) = Grid <$> moveForward l

moveDown' :: Grid a -> Grid a
moveDown' (Grid l) = Grid $ moveForward' l

moveLeft :: Grid a -> Maybe (Grid a)
moveLeft (Grid l) = Just $ Grid (moveBack' <$> l)

moveLeft' :: Grid a -> Grid a
moveLeft' (Grid l) = Grid (moveBack' <$> l)

moveRight :: Grid a -> Maybe (Grid a)
moveRight (Grid l) = Just $ Grid (moveForward' <$> l)

moveRight' :: Grid a -> Grid a
moveRight' (Grid l) = Grid (moveForward' <$> l)

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

data Intcode = Intcode {
    icCursor :: Int,
    icReadBase :: Int,
    icInstrs :: [Int]
}

interp :: [Int] -> Intcode -> (Intcode, Maybe Int)
-- interp _ intcode = let (Intcode c rb l) = intcode in (Intcode 0 rb l, Nothing)
interp i intcode = let (Intcode c rb l) = intcode in case l !! c of
    99 -> (Intcode 0 rb l, Nothing)
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
        [] -> (Intcode c rb l, Nothing)
        (fi:ri) -> let nl = setElem l (l !! (c + 1)) fi in interp ri $ Intcode (c + 2) rb nl
    -- 0 0 2 03
    203 -> case i of
        [] -> (Intcode c rb l, Nothing)
        (fi:ri) -> let nl = setElem l (rb + (l !! (c + 1))) fi in interp ri $ Intcode (c + 2) rb nl
    -- 0 0 0 04 
    4 -> let o = (l !! (l !! (c + 1))) in (Intcode (c + 2) rb l, Just o)
    -- 0 0 2 04
    204 -> let o = (l !! (rb + l !! (c + 1))) in (Intcode (c + 2) rb l, Just o)
    -- 0 0 1 04
    104 -> let o = (l !! (c + 1)) in (Intcode (c + 2) rb l, Just o)
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
    9 -> interp i $ Intcode (c + 2) (rb + (l !! (l !! (c+1)))) l
    109 -> interp i $ Intcode (c + 2) (rb + (l !! (c+1))) l
    209 -> interp i $ Intcode (c + 2) (rb + (l !! (rb + (l !! (c+1))))) l
    opcode -> error ("Opcode not found " <> show opcode)

par :: String -> [Int]
par input = read . T.unpack <$> T.splitOn "," (T.strip (T.pack input))

solution1 :: String -> String
solution1 input = let
    prog = par input
    in show $ fst $ foo (Intcode 0 0 prog, 0, OGrid (pure (0, False)) U)

solution2 :: String -> String
solution2 input = let
    g = dropOG (1, False) (OGrid (pure (0, False)) U)
    prog = par input <> L.repeat 0
    gf = (\x -> if x == 0 then ' ' else '#') . fst <$> (snd $ foo (Intcode 0 0 prog, 0, g))
    (Just back) = grid 100 10 >>= repeatM moveRight 50 >>= repeatM moveDown 5
    in printG $ back *> ogrid gf

foo :: (Intcode, Int, OGrid (Int, Bool)) -> (Int, OGrid (Int, Bool))
foo (intcode, cnt, g) = let
    (i, painted) = extract g
    (intcode', mintcode') = interp [i] intcode
    in case mintcode' of
        Nothing -> (cnt, g)
        Just color -> let
            (intcode'', mlr) = interp [color] intcode'
            in case mlr of
                Nothing -> (cnt, g)
                Just lr -> let
                    d = case lr of
                        0 -> L
                        1 -> R
                    g' = (moveOG U . turnOG d . dropOG (color, True)) g
                    in foo (intcode'', if painted then cnt else cnt + 1, g')

main :: IO ()
main = advent 2019 11 [solution1, solution2] $ do
    peek $ extract <$> (gridFromList [['1','2','3'],['4','5','6'],['7','8','9']] >>= moveRight >>= moveDown >>= Just . rotateRight >>= moveUp >>= moveLeft)
    peek $ extract <$> (gridFromList [['1','2','3'],['4','5','6'],['7','8','9']] >>= moveRight >>= moveDown >>= Just . rotateRight >>= moveUp)
    peek $ extract <$> (gridFromList [['1','2','3'],['4','5','6'],['7','8','9']] >>= moveRight >>= moveDown >>= Just . rotateRight >>= moveUp >>= moveRight)
    peek $ extract <$> (gridFromList [['1','2','3'],['4','5','6'],['7','8','9']] >>= moveRight >>= moveDown >>= Just . rotateRight >>= moveDown >>= moveLeft)
    peek $ extract <$> (gridFromList [['1','2','3'],['4','5','6'],['7','8','9']] >>= moveRight >>= moveDown >>= Just . rotateRight >>= moveDown)
    peek $ extract <$> (gridFromList [['1','2','3'],['4','5','6'],['7','8','9']] >>= moveRight >>= moveDown >>= Just . rotateRight >>= moveDown >>= moveRight)
    return ()