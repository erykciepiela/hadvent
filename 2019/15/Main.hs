module Main where

import Advent
import Utils
import Data.Text as T
import Data.Text.Read as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Map as M
import Data.Maybe
import Control.Exception
import Data.List.Split as LS
import Data.Angle
import Control.Comonad
import qualified Data.List.Safe as LSF
import Data.Semigroup
import Data.Foldable as F
import Debug.Trace
import Data.Functor
import Data.ByteString.Char8 as C
import Text.Parsec
import Text.Parsec.Char 
import Text.Parsec.Combinator as P

--

forceElements :: [a] -> ()
forceElements = F.foldr seq ()

setElem :: [Int] -> Int -> Int -> [Int]
setElem list pos val = seq forceElements $ L.take pos list <> [val] <> L.drop (pos + 1) list

data Intcode = Intcode {
    icCursor :: Int,
    icReadBase :: Int,
    icInstrs :: [Int]
} deriving Show

interp :: [Int] -> (Intcode, [Int]) -> (Maybe Intcode, [Int])
interp i (intcode, os) = let (Intcode c rb l) = intcode in case l !! c of
    99 -> (Nothing, os)
    -- 0 0 0 01
    1 ->   let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) + l !! (l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 0 0 01
    20001 ->   let nl = setElem l (rb + l !! (c + 3)) (l !! (l !! (c +1)) + l !! (l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 0 2 01 !!!
    201 ->  let nl = setElem l (l !! (c + 3)) (l !! (rb + l !! (c +1)) + l !! (l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 1 0 01 !!!
    21001 ->   let nl = setElem l (rb + l !! (c + 3)) (l !! (l !! (c +1)) + l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 2 2 01
    22201 ->   let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) + l !! (rb + l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 0 1 01
    101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! (l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 0 1 01 !!!
    20101 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c + 1)) + l !! (l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 2 1 01
    22101 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c + 1)) + l !! (rb + l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 0 01
    1001 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c + 1)) + l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 0 01
    1201 -> let nl = setElem l (l !! (c + 3)) (l !! (rb + l !! (c + 1)) + l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 1 0 01
    21201 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c + 1)) + l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 1 01
    1101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 1 1 01
    21101 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c + 1)) + l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 1 1 01
    2101 -> let nl = setElem l (l !! (c + 3)) (l !! ((c + 1)) + l !! (rb + l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 0 0 02
    2 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! (l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 1 0 02
    21002 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (l !! (c +1)) * l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 2 2 02
    22202 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! (rb + l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 0 1 02
    102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! (l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 0 1 02
    20102 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c +1)) * l !! (l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 2 1 02
    22102 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c +1)) * l !! (rb + l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 2 1 02
    2102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! (rb + l !! (c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 0 02
    1002 -> let nl = setElem l (l !! (c + 3)) (l !! (l !! (c +1)) * l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 2 02
    1202 -> let nl = setElem l (l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 1 2 02
    21202 -> let nl = setElem l (rb + l !! (c + 3)) (l !! (rb + l !! (c +1)) * l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 1 02
    1102 -> let nl = setElem l (l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 2 1 1 02
    21102 -> let nl = setElem l (rb + l !! (c + 3)) (l !! ((c +1)) * l !! ((c+2))) in interp i (Intcode (c + 4) rb nl, os)
    -- 0 0 0 03
    3 -> case i of
        [] -> (Just (Intcode (c) rb l), os)
        (fi:ri) -> let nl = setElem l (l !! (c + 1)) fi in interp ri (Intcode (c + 2) rb nl, os)
    -- 0 0 2 03
    203 -> case i of
        [] -> (Just (Intcode (c) rb l), os)
        (fi:ri) -> let nl = setElem l (rb + (l !! (c + 1))) fi in interp ri (Intcode (c + 2) rb nl, os)
    -- 0 0 0 04 
    4 -> let o = (l !! (l !! (c + 1))) in interp i (Intcode (c + 2) rb l, o:os)
    -- 0 0 2 04
    204 -> let o = (l !! (rb + l !! (c + 1))) in interp i  (Intcode (c + 2) rb l, o:os)
    -- 0 0 1 04
    104 -> let o = (l !! (c + 1)) in interp i  (Intcode (c + 2) rb l, o:os)
    -- 0 0 0 05
    5 -> if l !! (l !! (c + 1)) /= 0 then interp i (Intcode (l !! (l !! (c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 0 0 1 05
    105 -> if l !! ((c + 1)) /= 0 then interp i (Intcode (l !! (l !! (c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 0 2 1 05
    2105 -> if l !! ((c + 1)) /= 0 then interp i (Intcode (l !! (rb + l !! (c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 0 1 0 05
    1005 -> if l !! (l !! (c + 1)) /= 0 then interp i (Intcode (l !! ((c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 0 1 2 05
    1205 -> if l !! (rb + l !! (c + 1)) /= 0 then interp i (Intcode (l !! ((c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 1 1 0 05
    1105 -> if l !! ((c + 1)) /= 0 then interp i (Intcode (l !! ((c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 0 0 0 06
    6 -> if l !! (l !! (c + 1)) == 0 then interp i (Intcode (l !! (l !! (c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 0 0 1 06
    106 -> if l !! ((c + 1)) == 0 then interp i (Intcode (l !! (l !! (c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 0 2 1 06
    2106 -> if l !! ((c + 1)) == 0 then interp i (Intcode (l !! (rb + l !! (c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 0 1 0 06
    1006 -> if l !! (l !! (c + 1)) == 0 then interp i (Intcode (l !! ((c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 0 1 2 06
    1206 -> if l !! (rb + l !! (c + 1)) == 0 then interp i (Intcode (l !! ((c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 1 1 0 06
    1106 -> if l !! ((c + 1)) == 0 then interp i (Intcode (l !! ((c + 2))) rb l, os) else interp i (Intcode (c + 3) rb l, os)
    -- 0 0 0 07
    7 -> if l !! (l !! (c + 1)) < (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 2 2 07
    2207 -> if l !! (rb + l !! (c + 1)) < (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 0 1 07
    107 -> if l !! ((c + 1)) < (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 2 1 07
    2107 -> if l !! ((c + 1)) < (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 0 07
    1007 -> if l !! (l !! (c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 2 07
    1207 -> if l !! (rb + l !! (c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 1 07
    1107 -> if l !! ((c + 1)) < (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 2 1 1 07
    21107 -> if l !! ((c + 1)) < (l !! ((c + 2))) then let nl = setElem l (rb + l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (rb + l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 0 0 08
    8 -> if l !! (l !! (c + 1)) == (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 0 1 08
    108 -> if l !! ((c + 1)) == (l !! (l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 2 1 08
    2108 -> if l !! ((c + 1)) == (l !! (rb + l !! (c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 0 08
    1008 -> if l !! (l !! (c + 1)) == (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 2 08
    1208 -> if l !! (rb + l !! (c + 1)) == (l !! ((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 1 1 08
    1108 -> if l !! ((c + 1)) == (l !!((c + 2))) then let nl = setElem l (l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 2 1 1 08
    21108 -> if l !! ((c + 1)) == (l !!((c + 2))) then let nl = setElem l (rb + l !! (c + 3)) 1 in interp i (Intcode (c + 4) rb nl, os) else let nl = setElem l (rb + l !! (c + 3)) 0 in interp i (Intcode (c + 4) rb nl, os)
    -- 0 0 0 09
    9 -> interp i (Intcode (c + 2) (rb + (l !! (l !! (c+1)))) l, os)
    -- 0 0 1 09
    109 -> interp i (Intcode (c + 2) (rb + (l !! (c+1))) l, os)
    -- 0 0 2 09
    209 -> interp i (Intcode (c + 2) (rb + (l !! (rb + (l !! (c+1))))) l, os)
    opcode -> error ("Opcode not found " <> show opcode)

parseInput :: String -> [Int]
parseInput input = read . T.unpack <$> T.splitOn "," (T.strip (T.pack input))

tryMove :: Intcode -> Int -> (Intcode, Int)
tryMove ic d = (\(Just i, is) -> (i, L.head is)) $ interp [d] (ic, [])

walk :: [(Int, Int)] -> Intcode -> [(Int, (Int, Int), Intcode)]
walk p@((x, y):_) ic = let
    foo = (\(x', y', d) -> if (x',y') `L.elem` p then [] else 
        let (ic', q) = tryMove ic d 
        in case q of
            0 -> [] --wall
            1 -> (\(a, b, c) -> (a + 1, b, c)) <$> walk ((x', y'):p) ic'
            2 -> [(1, (x', y'), ic')]) <$> [(x,y-1,1), (x,y+1,2), (x-1,y,3), (x+1,y,4)] 
    in mconcat foo

walk' :: [(Int, Int)] -> Intcode -> [Int]
walk' p@((x, y):_) ic = let
    foo = (\(x', y', d) -> if (x',y') `L.elem` p then [] else 
        let (ic', q) = tryMove ic d 
        in case q of
            0 -> [0] --wall
            1 -> (\(a) -> a + 1) <$> walk' ((x', y'):p) ic'
            2 -> []) <$> [(x,y-1,1), (x,y+1,2), (x-1,y,3), (x+1,y,4)] 
    in mconcat foo

solution1 :: String -> String
solution1 input = let 
    prog = parseInput input
    ic = Intcode 0 0 prog
    in show $ F.maximum $ (\(f, _, _) -> f) <$> walk [(0,0)] ic

solution2 :: String -> String
solution2 input = let 
    prog = parseInput input
    ic = Intcode 0 0 prog
    ic' = (\(_, p, c) -> c) $ L.head $ walk [(0,0)] ic
    in show $ L.maximum $ walk' [(0,0)] ic'
    
main :: IO ()
main = advent 2019 15 [solution1, solution2] $ do
    return ()
