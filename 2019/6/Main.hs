module Main where

import Advent
import Data.Text
import Data.Text.Read as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative

solution1 :: String -> String
solution1 input = input

solution2 :: String -> String
solution2 input = input

main :: IO ()
main = do
    test solution1 "1" "1"
    runAdvent 2019 6 solution1 []
    test solution2 "2" "3"
    runAdvent 2019 6 solution2 []

