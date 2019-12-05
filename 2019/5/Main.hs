module Main where

import Advent
import Data.Text
import Data.Text.Read as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative

set :: [Int] -> Int -> Int -> [Int]
set list pos val = L.take pos list <> [val] <> L.drop (pos + 1) list

interp1 :: Int -> Int -> [Int] -> [Int] -> ([Int], [Int])
interp1 _ _ [] os = ([], os)
interp1 i c l os = case l !! c of
    99 -> (l, os)
    -- 0 0 0 01
    1 ->   let nl = set l (l !! (c + 3)) (l !! (l !! (c +1)) + l !! (l !! (c+2))) in interp1 i (c + 4) nl os
    -- 0 0 1 01
    101 -> let nl = set l (l !! (c + 3)) (l !! ((c + 1)) + l !! (l !! (c+2))) in interp1 i (c + 4) nl os
    -- 0 1 0 01
    1001 -> let nl = set l (l !! (c + 3)) (l !! (l !! (c + 1)) + l !! ((c+2))) in interp1 i (c + 4) nl os
    -- 0 1 1 01
    1101 -> let nl = set l (l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp1 i (c + 4) nl os
    -- 0 0 0 02
    2 -> let nl = set l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! (l !! (c+2))) in interp1 i (c + 4) nl os
    -- 0 0 1 02
    102 -> let nl = set l (l !! (c + 3)) (l !! ((c +1)) * l !! (l !! (c+2))) in interp1 i (c + 4) nl os
    -- 0 1 0 02
    1002 -> let nl = set l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! ((c+2))) in interp1 i (c + 4) nl os
    -- 0 1 1 02
    1102 -> let nl = set l (l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp1 i (c + 4) nl os
    -- 0 0 0 03
    3 -> let nl = set l (l !! (c + 1)) i in interp1 i (c + 2) nl os
    -- 0 0 0 04 
    4 -> let o = (l !! (l !! (c + 1))) in interp1 i (c + 2) l (o:os)
    -- 0 0 1 04
    104 -> let o = (l !! (c + 1)) in interp1 i (c + 2) l (o:os)

solution1 :: Int -> String -> String
solution1 i input = let 
    prog  = fst . either error id . decimal <$> splitOn "," (pack input)
    in show $ snd $ interp1 i 0 prog []

interp2 :: Int -> Int -> [Int] -> [Int] -> ([Int], [Int])
interp2 _ _ [] os = ([], os)
interp2 i c l os = case l !! c of
    99 -> (l, os)
    -- 0 0 0 01
    1 ->   let nl = set l (l !! (c + 3)) (l !! (l !! (c +1)) + l !! (l !! (c+2))) in interp2 i (c + 4) nl os
    -- 0 0 1 01
    101 -> let nl = set l (l !! (c + 3)) (l !! ((c + 1)) + l !! (l !! (c+2))) in interp2 i (c + 4) nl os
    -- 0 1 0 01
    1001 -> let nl = set l (l !! (c + 3)) (l !! (l !! (c + 1)) + l !! ((c+2))) in interp2 i (c + 4) nl os
    -- 0 1 1 01
    1101 -> let nl = set l (l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp2 i (c + 4) nl os
    -- 0 0 0 02
    2 -> let nl = set l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! (l !! (c+2))) in interp2 i (c + 4) nl os
    -- 0 0 1 02
    102 -> let nl = set l (l !! (c + 3)) (l !! ((c +1)) * l !! (l !! (c+2))) in interp2 i (c + 4) nl os
    -- 0 1 0 02
    1002 -> let nl = set l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! ((c+2))) in interp2 i (c + 4) nl os
    -- 0 1 1 02
    1102 -> let nl = set l (l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp2 i (c + 4) nl os
    -- 0 0 0 03
    3 -> let nl = set l (l !! (c + 1)) i in interp2 i (c + 2) nl os
    -- 0 0 0 04 
    4 -> let o = (l !! (l !! (c + 1))) in interp2 i (c + 2) l (o:os)
    -- 0 0 1 04
    104 -> let o = (l !! (c + 1)) in interp2 i (c + 2) l (o:os)
    -- 0 0 0 05
    5 -> if l !! (l !! (c + 1)) /= 0 then interp2 i (l !! (l !! (c + 2))) l os else interp2 i (c + 3) l os
    -- 0 0 1 05
    105 -> if l !! ((c + 1)) /= 0 then interp2 i (l !! (l !! (c + 2))) l os else interp2 i (c + 3) l os
    -- 0 1 0 05
    1005 -> if l !! (l !! (c + 1)) /= 0 then interp2 i (l !! ((c + 2))) l os else interp2 i (c + 3) l os
    -- 1 1 0 05
    1105 -> if l !! ((c + 1)) /= 0 then interp2 i (l !! ((c + 2))) l os else interp2 i (c + 3) l os
    -- 0 0 0 06
    6 -> if l !! (l !! (c + 1)) == 0 then interp2 i (l !! (l !! (c + 2))) l os else interp2 i (c + 3) l os
    -- 0 0 1 06
    106 -> if l !! ((c + 1)) == 0 then interp2 i (l !! (l !! (c + 2))) l os else interp2 i (c + 3) l os
    -- 0 1 0 06
    1006 -> if l !! (l !! (c + 1)) == 0 then interp2 i (l !! ((c + 2))) l os else interp2 i (c + 3) l os
    -- 1 1 0 06
    1106 -> if l !! ((c + 1)) == 0 then interp2 i (l !! ((c + 2))) l os else interp2 i (c + 3) l os
    -- 0 0 0 07
    7 -> if l !! (l !! (c + 1)) < (l !! (l !! (c + 2))) then let nl = set l (l !! (c + 3)) 1 in interp2 i (c + 4) nl os else let nl = set l (l !! (c + 3)) 0 in interp2 i (c + 4) nl os
    -- 0 0 1 07
    107 -> if l !! ((c + 1)) < (l !! (l !! (c + 2))) then let nl = set l (l !! (c + 3)) 1 in interp2 i (c + 4) nl os else let nl = set l (l !! (c + 3)) 0 in interp2 i (c + 4) nl os
    -- 0 1 0 07
    1007 -> if l !! (l !! (c + 1)) < (l !! ((c + 2))) then let nl = set l (l !! (c + 3)) 1 in interp2 i (c + 4) nl os else let nl = set l (l !! (c + 3)) 0 in interp2 i (c + 4) nl os
    -- 0 1 1 07
    1107 -> if l !! ((c + 1)) < (l !! ((c + 2))) then let nl = set l (l !! (c + 3)) 1 in interp2 i (c + 4) nl os else let nl = set l (l !! (c + 3)) 0 in interp2 i (c + 4) nl os
    -- 0 0 0 08
    8 -> if l !! (l !! (c + 1)) == (l !! (l !! (c + 2))) then let nl = set l (l !! (c + 3)) 1 in interp2 i (c + 4) nl os else let nl = set l (l !! (c + 3)) 0 in interp2 i (c + 4) nl os
    -- 0 0 1 07
    108 -> if l !! ((c + 1)) == (l !! (l !! (c + 2))) then let nl = set l (l !! (c + 3)) 1 in interp2 i (c + 4) nl os else let nl = set l (l !! (c + 3)) 0 in interp2 i (c + 4) nl os
    -- 0 1 0 07
    1008 -> if l !! (l !! (c + 1)) == (l !! ((c + 2))) then let nl = set l (l !! (c + 3)) 1 in interp2 i (c + 4) nl os else let nl = set l (l !! (c + 3)) 0 in interp2 i (c + 4) nl os
    -- 0 1 1 07
    1108 -> if l !! ((c + 1)) == (l !!((c + 2))) then let nl = set l (l !! (c + 3)) 1 in interp2 i (c + 4) nl os else let nl = set l (l !! (c + 3)) 0 in interp2 i (c + 4) nl os
    -- 
    opcode -> error ("Opcode not found " <> show opcode)

solution2 :: Int -> String -> String
solution2 i input = let 
    prog  = fst . either error id . decimal <$> splitOn "," (pack input)
    in show $ snd $ interp2 i 0 prog []

main :: IO ()
-- main = runAdvent 2019 5 (solution1 1) []
main = runAdvent 2019 5 (solution2 5) []

