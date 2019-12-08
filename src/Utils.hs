module Utils where

import Data.List as L
import Data.Text as T

stripLeft :: Eq a => [a] -> [a] -> [a]
stripLeft toStrip stripped@(s:ss)
    | s `elem` toStrip = ss
    | otherwise = stripped
    
countMatching :: Eq a => [a] -> [(Int, a)]
countMatching as = (\a -> (L.length (L.filter (== a) as), a)) <$> nub as

countOccurences :: [Int] -> Int -> Int
countOccurences is i = L.length $ L.elemIndices i is

strip :: String -> String
strip = T.unpack . T.strip . T.pack

newtype SList a = SList { slist :: [a] } 

instance Semigroup a => Semigroup (SList a) where
    l1 <> l2 = SList $ L.foldl1 (<>) <$> L.transpose [slist l1, slist l2]
