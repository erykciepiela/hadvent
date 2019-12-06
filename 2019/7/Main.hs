module Main where

import Advent
import Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Map as P
import Data.Maybe

solution1 :: String -> String
solution1 input = "abc"

solution2 :: String -> String
solution2 input = "abc"

main :: IO ()
main = advent 2019 7 solution1 $ do
    check id 1 1
    check id 2 2
    peek $ id 3 
    peek $ id 4 
