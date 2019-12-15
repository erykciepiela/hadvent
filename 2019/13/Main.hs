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

solution1 :: String -> String
solution1 input = let
    prog = parseInput input <> L.repeat 0
    in show $ L.length $ L.filter (==2) $ (!! 2) <$> (chunks3 $ snd $ foo (Intcode 0 0 prog) (L.repeat 0))

instr :: Intcode -> Int
instr (Intcode c _ is) = is !! c

foo :: Intcode -> [Int] -> (Maybe Intcode, [Int])
foo intcode is = case interp is (intcode, []) of
        (Just intcode', o) -> (Just intcode', o) -- output produced
        (Nothing, o) -> (Nothing, o) -- no inputs left

chunks3 :: [a] -> [[a]]
chunks3 [] = []
chunks3 (a1:a2:a3:as) = [a1, a2, a3]:chunks3 as

solution2 :: String -> String
solution2 input = let
    prog = setElem (parseInput input) 0 2 <> L.repeat 0
    (Just i1, o) = foo (Intcode 0 0 prog) [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1]
    chs = L.nubBy (\a b -> a !! 1 == b !! 1 && a !! 0 == b !! 0) $ L.reverse $ chunks3 $ o
    lines = [f . (!! 2) <$> (L.sortOn (!! 0) $ L.filter (\ch -> ch !! 1 == y) chs) | y <- [0..23]]
    in trace (L.unlines lines) $ show $ (!! 2) $ L.head $ L.filter (\[a1, a2, a3] -> a1 == -1 && a2 == 0) $ chs

solution2' :: IO Int
solution2' = do
    input <- C.unpack <$> C.readFile "2019/13/input.txt"
    let prog = setElem (parseInput input) 0 2 -- <> L.repeat 0
    let intcode = (Intcode 0 0 prog)
    return $ solution2'' 21 intcode []
    -- return ()

foresee :: [Int] -> Int
foresee o = let
    (Just [x , _, _]) = L.find (\ch -> ch !! 2 == 4) (L.reverse <$> chunks3 o)
    in x
-- foresee intcode = case interp [0] (intcode, []) of
--     (Just intcode', o) -> let
--         (Just [x , y, _]) = L.find (\ch -> ch !! 2 == 4) (L.reverse <$> chunks3 o)
--         in if y == 21 then Nothing else Just x
--     (Nothing, o) -> let
--         (Just [x , y, _]) = L.find (\ch -> ch !! 2 == 4) (L.reverse <$> chunks3 o)
--         in Just x
-- foresee' :: Intcode -> Maybe Int
-- foresee' intcode = case interp [0] (intcode, []) of
--     (Just intcode', o) -> let
--         (Just [x , y, _]) = L.find (\ch -> ch !! 2 == 4) (L.reverse <$> chunks3 o)
--         in if y == 21 then Just x else foresee' intcode' o
--     (Nothing, o) -> let
--         (Just [x , y, _]) = L.find (\ch -> ch !! 2 == 4) (L.reverse <$> chunks3 o)
--         in if y == 21 then Just x else error "!"
    -- let
    --     (Just [x , y, _]) = L.find (\ch -> ch !! 2 == 4) (L.reverse <$> chunks3 o)
    --     in error (show y)

solution2'' :: Int -> Intcode -> [Int] -> Int
solution2'' cx intcode is = case foo intcode is of
        (Nothing, o) -> let
            -- (trace (show (L.length chs))
            chs = chunks3 o
            (Just score) = (!! 0) <$> L.find (\[a3, a2, a1] -> a1 == -1 && a2 == 0) chs
            -- Prelude.putStrLn ("SCORE = " <> show score <> " " <> (show (L.length chs)))
            in score
        (Just intcode', o) -> let
            -- let oss = chunks3 $ (o <> os)
            -- let oss'' = mconcat oss'
            -- let chs = L.reverse <$> oss'
            -- let oss = L.nubBy (\a b -> a !! 1 == b !! 1 && a !! 2 == b !! 2) (chunks3 (o<>os))
            
            -- let oss = (chunks3 (o<>os))
            -- let chs = L.reverse <$> oss

            -- let (Just [currentx, _, _]) = L.find (\ch -> ch !! 2 == 3) chs
            
            -- let mscore = (!! 2) <$> L.find (\[a1, a2, a3] -> a1 == -1 && a2 == 0) chs
            -- Prelude.putStrLn ("s = " <> show mscore <> " " <> (show (L.length (L.findIndices (\[_, _, a3] -> a3 == 2) chs))))

            -- let lines = [f . (!! 2) <$> (L.sortOn (!! 0) (L.filter (\ch -> (ch !! 1) == y && ch !! 0 >= 0) chs)) | y <- [0..23]]
            -- Prelude.putStrLn (L.unlines lines)

            -- let score = L.head $ L.filter (\[a1, a2, a3] -> a1 == -1 && a2 == 0) chs
            -- Prelude.putStrLn ("will fall at x = " <> show targetx <> " " <> show currentx <> " " <> show currenty <> " score = " <> show score)
            -- case  of
            --     [] -> do
            --         let score = (!! 2) $ L.head $ L.filter (\[a1, a2, a3] -> a1 == -1 && a2 == 0) chs
            --         Prelude.putStrLn ("SCORE = " <> show score)
            --         return True
            --     indices -> do 
            --         Prelude.putStrLn ("SCORE = " <> show score)
            -- Prelude.putStrLn (("left = " :: String) <> show (L.length indices))
            d = g (foresee o) cx;
            -- in trace (show intcode') $ solution2'' (cx + d) intcode' [d]
            in solution2'' (cx + d) intcode' [d]
            -- b2 <- solution2'' intcode' (o<>os) [g (targetx+1) currentx]
            -- b3 <- solution2'' intcode' (o<>os) [g (targetx-1) currentx]
            -- return $ b1 `max` b2 `max` b3
            -- return $ b1 -- `max` b2 `max` b3

g :: Int -> Int -> Int
g target current
  | target > current = 1
  | target < current = -1
  | target == current = 0

f :: Int -> Char
f 0 = ' '
f 1 = '#'
f 2 = 'O'
f 3 = 'X'
f 4 = '*'
f i = error (show i)

main :: IO ()
main = solution2' >>= \x -> Prelude.putStrLn $ show "!!!" <> show x
-- main = advent 2019 13 [solution2] $ do
--     -- peek $ foo (Intcode 0 0 ([1,2,3,6,5,4] <> L.repeat)
--     return ()