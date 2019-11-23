module Lists where

import Data.List

stripLeft :: Eq a => [a] -> [a] -> [a]
stripLeft toStrip stripped@(s:ss)
    | s `elem` toStrip = ss
    | otherwise = stripped
    
countMatching :: Eq a => [a] -> [(Int, a)]
countMatching as = (\a -> (length (filter (== a) as), a)) <$> nub as
    