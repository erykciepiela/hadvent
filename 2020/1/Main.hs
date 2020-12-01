module Main where

import Advent

solution1 :: String -> String
solution1 input = let
  (items :: [Int]) = read <$> lines input
  in head [show (a * b) | a <- items, b <-items, a + b == 2020]

solution2 :: String -> String
solution2 input = let
  (items :: [Int]) = read <$> lines input
  in head [show (a * b * c) | a <- items, b <-items, c <- items, a + b + c == 2020]

main :: IO ()
main = advent 2020 1 [solution2] $ do
    solution1 "1721\n979\n366\n299\n675\n1456" `shouldBe` "514579"
    solution2 "1721\n979\n366\n299\n675\n1456" `shouldBe` "241861950"
    return ()

