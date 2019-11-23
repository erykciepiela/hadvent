module Main where

import Advent
import Data.List
import Lists

testCases = [
    ("aa\naaa", "1"),
    ("aa\naa", "0"),
    ("aaa\naaa", "0"),
    ("aa\naa\naaa", "2"),
    ("aa\naaa\naaa", "2"),
    ("a\na", "0"),
    ("a", "0"),
    ("a\na\naaaa", "0"),
    ("aaaa", "0"),
    ("aa\naa\naaa\naaa\naaa\n", "6"),
    ("aa\nbb\naaa\nbbb\naaa\n", "6"),
    ("aax\nbbx\naaax\nbbbx\n", "4"),
    ("aax\nbbx\naaax\nbbbx\n", "4"),
    ("yaax\nybbx\nyaaax\nybbbx\n", "4"),
    ("yaax\nybbx\nyaaax\nybbbx\n", "4")
    ]

solution :: String -> String
solution input = let
    ls = lines input -- lines
    x = length $ filter (elem 3) $ fmap fst . countMatching <$> ls
    y = length $ filter (elem 2) $ fmap fst . countMatching <$> ls
    in show $ x * y

testCases2 = [
    ("abciiiiiiiiiiiiiiiiiiiiiii\nabdiiiiiiiiiiiiiiiiiiiiiii\nxxxiiiiiiiiiiiiiiiiiiiiiii\nyyyiiiiiiiiiiiiiiiiiiiiiii\n", "abiiiiiiiiiiiiiiiiiiiiiii"),
    ("zabiiiiiiiiiiiiiiiiiiiiiii\ndabiiiiiiiiiiiiiiiiiiiiiii\nxxxiiiiiiiiiiiiiiiiiiiiiii\nyyyiiiiiiiiiiiiiiiiiiiiiii\n", "abiiiiiiiiiiiiiiiiiiiiiii"),
    ("abciiiiiiiiiiiiiiiiiiiiiii\nazciiiiiiiiiiiiiiiiiiiiiii\nxxxiiiiiiiiiiiiiiiiiiiiiii\nyyyiiiiiiiiiiiiiiiiiiiiiii\n", "aciiiiiiiiiiiiiiiiiiiiiii"),
    ("abciiiiiiiiiiiiiiiiiiiiiii\nabwiiiiiiiiiiiiiiiiiiiiiii\nxxxiiiiiiiiiiiiiiiiiiiiiii\nyyyiiiiiiiiiiiiiiiiiiiiiii\n", "abiiiiiiiiiiiiiiiiiiiiiii"),
    ("abciiiiiiiiiiiiiiiiiiiiiii\nabwiiiiiiiiiiiiiiiiiiiiiii\naxxiiiiiiiiiiiiiiiiiiiiiii\nayyiiiiiiiiiiiiiiiiiiiiiii\n", "abiiiiiiiiiiiiiiiiiiiiiii"),
    ("abcdiiiiiiiiiiiiiiiiiiiiii\nabcwiiiiiiiiiiiiiiiiiiiiii\naxxxiiiiiiiiiiiiiiiiiiiiii\nayyyiiiiiiiiiiiiiiiiiiiiii\n", "abciiiiiiiiiiiiiiiiiiiiii"),
    ("abcdiiiiiiiiiiiiiiiiiiiiii\nabcwiiiiiiiiiiiiiiiiiiiiii\nabzkiiiiiiiiiiiiiiiiiiiiii\nayyyiiiiiiiiiiiiiiiiiiiiii\n", "abciiiiiiiiiiiiiiiiiiiiii")
    ]

solution2 :: String -> String
solution2 input = let
    ls = lines input -- lines
    -- 26
    [c, _] = [common f s | f <- ls, s <- ls, length (common f s) == 25]
    in c

common :: Eq a => [a] -> [a] -> [a]
common as1 as2 = fst <$> filter (\(a1, a2) -> a1 == a2) (zip as1 as2)


main :: IO ()
main = runAdvent 2 solution2 testCases2 
