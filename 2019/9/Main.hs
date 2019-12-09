module Main where

import Advent
import Utils
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
import Data.List.Split as LS

setElem :: [Int] -> Int -> Int -> [Int]
setElem list pos val = L.take pos list <> [val] <> L.drop (pos + 1) list

interp :: [Int] -> Int -> Int -> [Int] -> ([Int], Int, Int, Maybe Int)
interp _ c rb [] = ([], c, rb, Nothing)
interp i c rb l = case l !! c of
    99 -> (l, 0, rb, Nothing)
    -- 0 0 0 01
    1 ->   let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) + l !! (l !! (c+2))) in interp i (c + 4) rb nl
    -- 2 2 2 01
    22201 ->   let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) + l !! (rb + l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 0 1 01
    101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! (l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 1 0 01
    1001 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c + 1)) + l !! ((c+2))) in interp i (c + 4) rb nl
    -- 0 1 0 01
    1201 -> let nl = setElem l (l !! (c + 3)) (l !! (rb + l !! (c + 1)) + l !! ((c+2))) in interp i (c + 4) rb nl
    -- 2 1 0 01
    21201 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c + 1)) + l !! ((c+2))) in interp i (c + 4) rb nl
    -- 0 1 1 01
    1101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp i (c + 4) rb nl
    -- 2 1 1 01
    21101 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp i (c + 4) rb nl
    -- 2 1 1 01
    2101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! (rb + l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 0 0 02
    2 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! (l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 0 1 02
    102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! (l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 2 1 02
    2102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! (rb + l !! (c+2))) in interp i (c + 4) rb nl
    -- 0 1 0 02
    1002 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! ((c+2))) in interp i (c + 4) rb nl
    -- 0 1 2 02
    1202 -> let nl = setElem l (l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! ((c+2))) in interp i (c + 4) rb nl
    -- 2 1 2 02
    21202 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! ((c+2))) in interp i (c + 4) rb nl
    -- 0 1 1 02
    1102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp i (c + 4) rb nl
    -- 2 1 1 02
    21102 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp i (c + 4) rb nl
    -- 0 0 0 03
    3 -> case i of
        [] -> (l, c, rb, Nothing)
        (fi:ri) -> let nl = setElem l (l !! (c + 1)) fi in interp ri (c + 2) rb nl
    -- 0 0 2 03
    203 -> case i of
        [] -> (l, c, rb, Nothing)
        (fi:ri) -> let nl = setElem l (rb + (l !! (c + 1))) fi in interp ri (c + 2) rb nl
    -- 0 0 0 04 
    4 -> let o = (l !! (l !! (c + 1))) in (l, (c + 2), rb, Just o)
    -- 0 0 2 04
    204 -> let o = (l !! (rb + l !! (c + 1))) in (l, (c + 2), rb, Just o)
    -- 0 0 1 04
    104 -> let o = (l !! (c + 1)) in (l, (c + 2), rb, Just o)
    -- 0 0 0 05
    5 -> if l !! (l !! (c + 1)) /= 0 then interp i (l !! (l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 0 1 05
    105 -> if l !! ((c + 1)) /= 0 then interp i (l !! (l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 2 1 05
    2105 -> if l !! ((c + 1)) /= 0 then interp i (l !! (rb + l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 1 0 05
    1005 -> if l !! (l !! (c + 1)) /= 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 0 1 2 05
    1205 -> if l !! (rb + l !! (c + 1)) /= 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 1 1 0 05
    1105 -> if l !! ((c + 1)) /= 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 0 0 0 06
    6 -> if l !! (l !! (c + 1)) == 0 then interp i (l !! (l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 0 1 06
    106 -> if l !! ((c + 1)) == 0 then interp i (l !! (l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 2 1 06
    2106 -> if l !! ((c + 1)) == 0 then interp i (l !! (rb + l !! (c + 2))) rb l else interp i (c + 3) rb l
    -- 0 1 0 06
    1006 -> if l !! (l !! (c + 1)) == 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 0 1 2 06
    1206 -> if l !! (rb + l !! (c + 1)) == 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 1 1 0 06
    1106 -> if l !! ((c + 1)) == 0 then interp i (l !! ((c + 2))) rb l else interp i (c + 3) rb l
    -- 0 0 0 07
    7 -> if l !! (l !! (c + 1)) < (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 0 1 07
    107 -> if l !! ((c + 1)) < (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 2 1 07
    2107 -> if l !! ((c + 1)) < (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 0 07
    1007 -> if l !! (l !! (c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 2 07
    1207 -> if l !! (rb + l !! (c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 1 07
    1107 -> if l !! ((c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 2 1 1 07
    21107 -> if l !! ((c + 1)) < (l !! ((c + 2))) then let nl = setElem l (rb + l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (rb + l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 0 0 08
    8 -> if l !! (l !! (c + 1)) == (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 0 1 08
    108 -> if l !! ((c + 1)) == (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 2 1 08
    2108 -> if l !! ((c + 1)) == (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 0 08
    1008 -> if l !! (l !! (c + 1)) == (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 2 08
    1208 -> if l !! (rb + l !! (c + 1)) == (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 0 1 1 08
    1108 -> if l !! ((c + 1)) == (l !!((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (l !! (c + 3)) 0 in interp i (c + 4) rb nl
    -- 2 1 1 08
    21108 -> if l !! ((c + 1)) == (l !!((c + 2))) then let nl = setElem l (rb + l !! (c + 3)) 1 in interp i (c + 4) rb nl else let nl = setElem l (rb + l !! (c + 3)) 0 in interp i (c + 4) rb nl
    9 -> interp i (c + 2) (rb + (l !! (l !! (c+1)))) l
    109 -> interp i (c + 2) (rb + (l !! (c+1))) l
    209 -> interp i (c + 2) (rb + (l !! (rb + (l !! (c+1))))) l
    opcode -> error ("Opcode not found " <> show opcode)

interp' :: [Int] -> Int -> Int -> [Int] -> [Int] -> [Int]
interp' ins c rb prog os = let 
    (prog1, c1, rb1, mo) = interp ins c rb prog
    in case mo of
        Nothing -> os
        Just o -> interp' ins c1 rb1 prog1 (os<>[o])
    
par :: String -> [Int]
par input = read . T.unpack <$> T.splitOn "," (T.strip (T.pack input))

rap :: [Int] -> String
rap is = L.intercalate "," $ show <$> is

solution1 :: [Int] -> String -> String
solution1 is input = let
    prog = par input <> L.repeat 0
    in rap $ interp' is 0 0 prog []

solution2 :: String -> String
solution2 input = "?" -- interp [] 0 0 (par input)

main :: IO ()
main = advent 2019 9 [solution1 [1], solution1 [2]] $ do
    solution1 [] "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99" `shouldBe` "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    L.length (solution1 [] "1102,34915192,34915192,7,4,7,99,0") `shouldBe` 16
    solution1 [] "104,1125899906842624,99" `shouldBe` "1125899906842624"
    return ()