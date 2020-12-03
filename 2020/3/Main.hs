module Main where

import Advent

testInput :: String
testInput = "..##.......\n\
            \#...#...#..\n\
            \.#....#..#.\n\
            \..#.#...#.#\n\
            \.#...##..#.\n\
            \..#.##.....\n\
            \.#.#.#....#\n\
            \.#........#\n\
            \#.##...#...\n\
            \#...##....#\n\
            \.#..#...#.#"

solution1 :: String -> Int
solution1 = solution1' (3, 1)

solution1' :: (Int, Int) -> String -> Int
solution1' (dx, dy) input = let
  rows = cycle <$> lines input
  tree = '#'
  height = length rows
  path = takeWhile (\(_, y) -> y < height) $ iterate (\(x, y) -> (x + dx, y + dy)) (0, 0)
  in length $ filter id ((\(x, y) -> (rows !! y !! x) == tree) <$> path)

solution2 :: String -> Int
solution2 input = product $ (`solution1'` input) <$> [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

main :: IO ()
main = advent 2020 3 [solution2] $ do
    solution1 testInput `shouldBe` 7
    solution2 testInput `shouldBe` 336
    return ()

