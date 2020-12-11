module Grid where

import Control.Comonad
import Data.Distributive
-- import Data.List as L
-- import Data.Foldable as F
import Data.Maybe


-- data Line a = Line {
--     lineBackward :: [a],
--     lineCursor :: a,
--     lineForward :: [a]
-- }

data Line a = Line { lineCursor :: a, lineBackward :: [a], lineForward :: [a] } deriving Functor

-- with smart constructor we guarantee lineBackward and lineForward infinite
line :: a -> [a] -> Line a
line def as = Line (fromMaybe def (listToMaybe as)) (repeat def) (tail (as <> repeat def))

line' :: Int -> Line a -> [a]
line' n (Line c _ f) = take n (c:f)

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

instance Comonad Line where
  extract = lineCursor
  duplicate l = Line l (tail (iterate shiftBackward l)) (tail (iterate shiftForward l))

newtype Grid a = Grid (Line (Line a)) deriving Functor

grid :: a -> [[a]] -> Grid a
grid def ass = let lines = line def <$> ass in Grid $ line (line def []) lines

instance Comonad Grid where
  extract (Grid l) = (extract . extract) l
  duplicate (Grid l) = Grid . fmap (fmap Grid) $ (fmap distribute . duplicate . fmap duplicate) l

shiftUp :: Grid a -> Grid a
shiftUp (Grid l) = Grid (shiftBackward l)

shiftDown :: Grid a -> Grid a
shiftDown (Grid l) = Grid (shiftForward l)

shiftLeft :: Grid a -> Grid a
shiftLeft (Grid l) = Grid (shiftBackward <$> l)

shiftRight :: Grid a -> Grid a
shiftRight (Grid l) = Grid (shiftForward <$> l)

gridAt :: (Int, Int) -> Grid a -> a
gridAt (x, y)
  | x >= 0 && y >= 0 = extract . (!! x) . iterate shiftRight . (!! y) . iterate shiftDown
  | x >= 0 && y < 0 = extract . (!! x) . iterate shiftRight . (!! abs y) . iterate shiftUp
  | x < 0 && y < 0 = extract . (!! abs x) . iterate shiftLeft . (!! abs y) . iterate shiftUp
  | x < 0 && y >= 0 = extract . (!! abs x) . iterate shiftLeft . (!! y) . iterate shiftDown

gridAt' :: (Int, Int) -> Grid a -> [[a]]
gridAt' (x, y) (Grid l) = line' x <$> line' y l

viewAt :: (Int, Int) -> Grid a -> [a]
viewAt (x, y)
  | x >= 0 && y >= 0 = fmap extract . tail . iterate ((!! x) . iterate shiftRight . (!! y) . iterate shiftDown)
  | x >= 0 && y < 0 = fmap extract .  tail . iterate ((!! x) . iterate shiftRight . (!! abs y) . iterate shiftUp)
  | x < 0 && y < 0 = fmap extract .   tail . iterate ((!! abs x) . iterate shiftLeft . (!! abs y) . iterate shiftUp)
  | x < 0 && y >= 0 = fmap extract .  tail . iterate ((!! abs x) . iterate shiftLeft . (!! y) . iterate shiftDown)

-- deriving instance (Show a) => Show (Line a)
-- deriving instance (Eq a) => Eq (Line a)

-- instance Semigroup a => Semigroup (Line a) where
--     l1 <> l2 = Line (L.reverse (L.zipWith (<>) (L.reverse $ lineBackward l1) (L.reverse $ lineBackward l2))) (lineCursor l1 <> lineCursor l2) (L.zipWith (<>) (lineForward l1) (lineForward l2))

-- line :: [a] -> Maybe (Line a)
-- line as = Line <$> pure [] <*> LSF.head as <*> LSF.tail as

-- moveBack :: Line a -> Maybe (Line a)
-- moveBack (Line l c r) = Line <$> LSF.tail l <*> LSF.head l <*> pure (c:r)

-- iterM :: (a -> Maybe a) -> a -> [a]
-- iterM f a = case f a of
--     Nothing -> []
--     Just a' -> a':iterM f a'

-- repeatM :: (a -> Maybe a) -> Int -> a -> Maybe a
-- repeatM f 0 a = Just a
-- repeatM f n a = case f a of
--     Nothing -> Nothing
--     Just a' -> repeatM f (n - 1) a'

-- moveForward :: Line a -> Maybe (Line a)
-- moveForward (Line l c r) = Line <$> pure (c:l) <*> LSF.head r <*> LSF.tail r

-- instance Foldable Line where
--     foldr abb b (Line l c r)= Prelude.foldr abb b (L.reverse l <> [c] <> r)

-- instance Traversable Line where
--  sequenceA (Line l c r) = Line <$> sequenceA l <*> c <*> sequenceA r

-- instance Functor Line where
--     fmap f (Line l c r) = Line (f <$> l) (f c) (f <$> r)

-- instance Applicative Line where
--     pure a = Line (L.repeat a) a (L.repeat a)
--     l1 <*> l2 = Line (L.reverse (L.zipWith ($) (L.reverse $ lineBackward l1) (L.reverse $ lineBackward l2))) (lineCursor l1 $ lineCursor l2) (L.zipWith ($) (lineForward l1) (lineForward l2))

-- instance Comonad Line where
--     extract = lineCursor
--     duplicate l = Line (iterM moveBack l) l (iterM moveForward l)

-- data Grid a = Grid {
--     gridLines :: Line (Line a)
-- }

-- gridFromList :: [[a]] -> Maybe (Grid a)
-- gridFromList ass = let lines = catMaybes (line <$> ass) in Grid <$> (Line <$> pure [] <*> LSF.head lines <*> LSF.tail lines)

-- grid :: Int -> Int -> Maybe (Grid (Int, Int))
-- grid w h = gridFromList [[(x, y) | x <- [0..(w-1)]] | y <- [0..(h-1)]]

-- infinigrid :: Maybe (Grid (Int, Int))
-- infinigrid = gridFromList [[(x, y) | x <- [0..]] | y <- [0..]]

-- moveUp :: Grid a -> Maybe (Grid a)
-- moveUp (Grid l) = Grid <$> moveBack l

-- moveDown :: Grid a -> Maybe (Grid a)
-- moveDown (Grid l) = Grid <$> moveForward l

-- moveLeft :: Grid a -> Maybe (Grid a)
-- moveLeft (Grid l) = Grid <$> sequenceA (moveBack <$> l)

-- moveRight :: Grid a -> Maybe (Grid a)
-- moveRight (Grid l) = Grid <$> sequenceA (moveForward <$> l)

-- translate :: (Int, Int) -> (Int, Int) -> (Int, Int)
-- translate (dx, dy) (x, y) = (x-dx, y-dy)

-- instance Foldable Grid where
--     foldr abb b g = F.foldr (\l b -> F.foldr abb b l) b (gridLines g)

-- instance Functor Grid where
--     fmap f (Grid g) = Grid $ fmap f <$> g

-- instance Applicative Grid where
--     pure a = Grid $ Line (L.repeat (pure a)) (pure a) (L.repeat (pure a))
--     g1 <*> g2 = Grid $ (<*>) <$> gridLines g1 <*> gridLines g2

-- instance Comonad Grid where
--     extract = extract . extract . gridLines
--     -- duplicate g = Grid $ duplicate (Grid <$> duplicate (gridLines g))
--     -- duplicate g = Grid <$> Grid (duplicate $ duplicate <$> gridLines g)
--     duplicate g = Grid (iterM moveBack l) (l (iterM moveForward l)


-- instance Semigroup a => Semigroup (Grid a) where
--     g1 <> g2 = Grid $ gridLines g1 <> gridLines g2

-- deriving instance (Show a) => Show (Grid a)
-- deriving instance (Eq a) => Eq (Grid a)
