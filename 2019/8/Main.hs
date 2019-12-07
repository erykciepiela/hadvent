module Main where

import Advent
import Data.Text as T
import Data.Text.Read as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Map as P
import Data.Maybe
import Control.Exception
import Debug.Trace

solution1 :: String -> String
solution1 input = "?"

solution2 :: String -> String
solution2 input = "?"

main :: IO ()
main = advent 2019 8 [solution1, solution2] $ do
    solution1 "" `shouldBe` "?"
    solution2 "" `shouldBe` "?"
