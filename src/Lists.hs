module Lists where

stripLeft :: Eq a => [a] -> [a] -> [a]
stripLeft toStrip stripped@(s:ss)
    | s `elem` toStrip = ss
    | otherwise = stripped
    