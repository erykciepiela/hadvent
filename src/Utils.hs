module Utils where

import Data.List as L

stripLeft :: Eq a => [a] -> [a] -> [a]
stripLeft toStrip stripped@(s:ss)
    | s `elem` toStrip = ss
    | otherwise = stripped
    
countMatching :: Eq a => [a] -> [(Int, a)]
countMatching as = (\a -> (length (filter (== a) as), a)) <$> nub as

countOccurences :: [Int] -> Int -> Int
countOccurences is i = L.length $ L.elemIndices i is

newtype SList a = SList { slist :: [a] } 

instance Semigroup a => Semigroup (SList a) where
    l1 <> l2 = SList $ L.foldl1 (<>) <$> L.transpose [slist l1, slist l2]
