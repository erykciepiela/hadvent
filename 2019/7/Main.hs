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

setElem :: [Int] -> Int -> Int -> [Int]
setElem list pos val = L.take pos list <> [val] <> L.drop (pos + 1) list

interp :: [Int] -> Int -> [Int] -> ([Int], Int, Maybe Int)
interp _ c [] = ([], c, Nothing)
interp i c l = case l !! c of
    99 -> (l, 0, Nothing)
    -- 0 0 0 01
    1 ->   let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) + l !! (l !! (c+2))) in interp i (c + 4) nl
    -- 0 0 1 01
    101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! (l !! (c+2))) in interp i (c + 4) nl
    -- 0 1 0 01
    1001 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c + 1)) + l !! ((c+2))) in interp i (c + 4) nl
    -- 0 1 1 01
    1101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp i (c + 4) nl
    -- 0 0 0 02
    2 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! (l !! (c+2))) in interp i (c + 4) nl
    -- 0 0 1 02
    102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! (l !! (c+2))) in interp i (c + 4) nl
    -- 0 1 0 02
    1002 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! ((c+2))) in interp i (c + 4) nl
    -- 0 1 1 02
    1102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp i (c + 4) nl
    -- 0 0 0 03
    3 -> case i of
        [] -> (l, c, Nothing)
        (fi:ri) -> let nl = setElem l (l !! (c + 1)) fi in interp ri (c + 2) nl
    -- 0 0 0 04 
    4 -> let o = (l !! (l !! (c + 1))) in (l, (c + 2), Just o)
    -- 0 0 1 04
    104 -> let o = (l !! (c + 1)) in (l, (c + 2), Just o)
    -- 0 0 0 05
    5 -> if l !! (l !! (c + 1)) /= 0 then interp i (l !! (l !! (c + 2))) l else interp i (c + 3) l
    -- 0 0 1 05
    105 -> if l !! ((c + 1)) /= 0 then interp i (l !! (l !! (c + 2))) l else interp i (c + 3) l
    -- 0 1 0 05
    1005 -> if l !! (l !! (c + 1)) /= 0 then interp i (l !! ((c + 2))) l else interp i (c + 3) l
    -- 1 1 0 05
    1105 -> if l !! ((c + 1)) /= 0 then interp i (l !! ((c + 2))) l else interp i (c + 3) l
    -- 0 0 0 06
    6 -> if l !! (l !! (c + 1)) == 0 then interp i (l !! (l !! (c + 2))) l else interp i (c + 3) l
    -- 0 0 1 06
    106 -> if l !! ((c + 1)) == 0 then interp i (l !! (l !! (c + 2))) l else interp i (c + 3) l
    -- 0 1 0 06
    1006 -> if l !! (l !! (c + 1)) == 0 then interp i (l !! ((c + 2))) l else interp i (c + 3) l
    -- 1 1 0 06
    1106 -> if l !! ((c + 1)) == 0 then interp i (l !! ((c + 2))) l else interp i (c + 3) l
    -- 0 0 0 07
    7 -> if l !! (l !! (c + 1)) < (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) nl
    -- 0 0 1 07
    107 -> if l !! ((c + 1)) < (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) nl
    -- 0 1 0 07
    1007 -> if l !! (l !! (c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) nl
    -- 0 1 1 07
    1107 -> if l !! ((c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) nl
    -- 0 0 0 08
    8 -> if l !! (l !! (c + 1)) == (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) nl
    -- 0 0 1 07
    108 -> if l !! ((c + 1)) == (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) nl
    -- 0 1 0 07
    1008 -> if l !! (l !! (c + 1)) == (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) nl
    -- 0 1 1 07
    1108 -> if l !! ((c + 1)) == (l !!((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) nl
    -- 
    opcode -> error ("Opcode not found " <> show opcode)

runProg :: [Int] -> [Int] -> Int
runProg prog i = let 
    in (\(_, _, Just x) -> x) $ interp i 0 prog

runProg2 :: [Int] -> ([Int], Int) -> (([Int], Int), Maybe Int)
runProg2 i (prog, c) = let 
    in (\(l, c, o) -> ((l, c), o)) $ interp i c prog

xxx :: [Int] -> [Int] -> Int
xxx program [i1, i2, i3, i4, i5] = let
    r1 = runProg program [i1, 0]
    r2 = runProg program [i2, r1]
    r3 = runProg program [i3, r2]
    r4 = runProg program [i4, r3]
    r5 = runProg program [i5, r4]
    in r5

xxx2 :: Int -> [([Int], Int)] -> [Int] -> Int
xxx2 i [p1, p2, p3, p4, p5] [i1, i2, i3, i4, i5] = let
        (p1', _) =runProg2  [i1] p1
        (p2', _) = runProg2 [i2] p2 
        (p3', _) = runProg2 [i3] p3 
        (p4', _) = runProg2 [i4] p4 
        (p5', _) = runProg2 [i5] p5 
        in xxx2' 0 [p1',p2',p3', p4', p5'] [i1, i2, i3, i4, i5] 

xxx2' :: Int -> [([Int], Int)] -> [Int] -> Int
xxx2' i [p1, p2, p3, p4, p5] [i1, i2, i3, i4, i5] = case runProg2 [i]  p1 of
        (_, Nothing) -> i
        (p1', Just r1) -> let
            (p2', Just r2) = runProg2 [r1] p2 
            (p3', Just r3) = runProg2 [r2] p3 
            (p4', Just r4) = runProg2 [r3] p4 
            (p5', Just r5) = runProg2 [r4] p5 
            in xxx2' r5 [p1',p2',p3', p4', p5'] [i1, i2, i3, i4, i5] 
    
solution1 :: String -> String
solution1 input = let
    prog = read . unpack <$> splitOn "," (pack input)
    in show $ L.maximum $ xxx prog <$> L.permutations [0..4]

solution2 :: String -> String
solution2 input = let
    prog = read . unpack <$> splitOn "," (pack input)
    in show $ L.maximum $ xxx2 0 [(prog, 0), (prog, 0),(prog, 0),(prog, 0),(prog, 0)] <$> (L.permutations [5..9])

main :: IO ()
main = advent 2019 7 [solution1, solution2] $ do
    solution1 "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" `shouldBe` "65210"
    solution1 "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" `shouldBe` "54321"
    solution1 "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" `shouldBe` "43210"
    solution2 "3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5" `shouldBe` "139629729"
    solution2 "3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10" `shouldBe` "18216"
