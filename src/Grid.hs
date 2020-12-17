module Grid where

import Control.Comonad
import Data.Distributive
-- import Data.List as L
-- import Data.Foldable as F
import Data.Maybe

-- Line
data Line a = Line { lineCursor :: a, lineBackward :: [a], lineForward :: [a] } deriving Functor

-- with smart constructor we guarantee lineBackward and lineForward infinite
line :: a -> [a] -> Line a
line def as = Line (fromMaybe def (listToMaybe as)) (repeat def) (tail (as <> repeat def))

instance Distributive Line where
  -- distribute :: Functor f => f (Line a) -> Line (f a)
  distribute fl = Line (lineCursor <$> fl) (foo $ lineBackward <$> fl) (foo $ lineForward <$> fl)
    where
      foo :: Functor f => f [a] -> [f a]
      foo fa = (head <$> fa) : foo (tail <$> fa)

shiftForward :: Line a -> Line a
shiftForward (Line c b f) = Line (head f) (c:b) (tail f)

shiftBackward :: Line a -> Line a
shiftBackward (Line c b f) = Line (head b) (tail b) (c:f)

section :: Int -> Line a -> [a]
section n (Line c _ f) = take n (c:f)

instance Comonad Line where
  extract = lineCursor
  duplicate l = Line l (tail (iterate shiftBackward l)) (tail (iterate shiftForward l))

-- Grid
newtype Grid a = Grid (Line (Line a)) deriving Functor

grid :: a -> [[a]] -> Grid a
grid def ass = let lines = line def <$> ass in Grid $ line (line def []) lines

instance Comonad Grid where
  extract (Grid l) = (extract . extract) l
  duplicate (Grid l) = Grid . fmap (fmap Grid) $ (fmap distribute . duplicate . fmap duplicate) l

instance Distributive Grid where
  distribute fl = Grid $ distribute <$> distribute ((\(Grid lines) -> lines) <$> fl)
    where
      foo :: Functor f => f [a] -> [f a]
      foo fa = (head <$> fa) : foo (tail <$> fa)


shiftUp :: Grid a -> Grid a
shiftUp (Grid l) = Grid (shiftBackward l)

shiftDown :: Grid a -> Grid a
shiftDown (Grid l) = Grid (shiftForward l)

shiftLeft :: Grid a -> Grid a
shiftLeft (Grid l) = Grid (shiftBackward <$> l)

shiftRight :: Grid a -> Grid a
shiftRight (Grid l) = Grid (shiftForward <$> l)

pointAt :: (Int, Int) -> Grid a -> a
pointAt (x, y)
  | x >= 0 && y >= 0 = extract . (!! x) . iterate shiftRight . (!! y) . iterate shiftDown
  | x >= 0 && y < 0 = extract . (!! x) . iterate shiftRight . (!! abs y) . iterate shiftUp
  | x < 0 && y < 0 = extract . (!! abs x) . iterate shiftLeft . (!! abs y) . iterate shiftUp
  | x < 0 && y >= 0 = extract . (!! abs x) . iterate shiftLeft . (!! y) . iterate shiftDown

areaOver :: (Int, Int) -> Grid a -> [[a]]
areaOver (x, y) (Grid l) = section x <$> section y l

lineTowards :: (Int, Int) -> Grid a -> [a]
lineTowards (x, y)
  | x >= 0 && y >= 0 = fmap extract . tail . iterate ((!! x) . iterate shiftRight . (!! y) . iterate shiftDown)
  | x >= 0 && y < 0 = fmap extract .  tail . iterate ((!! x) . iterate shiftRight . (!! abs y) . iterate shiftUp)
  | x < 0 && y < 0 = fmap extract .   tail . iterate ((!! abs x) . iterate shiftLeft . (!! abs y) . iterate shiftUp)
  | x < 0 && y >= 0 = fmap extract .  tail . iterate ((!! abs x) . iterate shiftLeft . (!! y) . iterate shiftDown)

-- Space

newtype Space a = Space (Line (Grid a)) deriving Functor

space :: a -> [[[a]]] -> Space a
space def ass = let grids = grid def <$> ass in Space $ line (grid def []) grids

instance Comonad Space where
  extract (Space l) = (extract . extract) l
  duplicate (Space l) = Space . fmap (fmap Space) $ (fmap distribute . duplicate . fmap duplicate) l

spaceOver :: (Int, Int, Int) -> Space a -> [[[a]]]
spaceOver (x, y, z) (Space l) = areaOver (x, y) <$> section z l

shiftSpaceZUp :: Int -> Space a -> Space a
shiftSpaceZUp n (Space line) = Space (iterate shiftBackward line !! n)

shiftSpaceYUp :: Int -> Space a -> Space a
shiftSpaceYUp n (Space line) = Space $ (\l -> iterate shiftUp l !! n) <$> line

shiftSpaceXUp :: Int -> Space a -> Space a
shiftSpaceXUp n (Space line) = Space $ (\l -> iterate shiftLeft l !! n) <$> line