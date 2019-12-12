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
-- import Data.Attoparsec.ByteString as A
-- import Data.ByteString.Char8 as C8
import Text.Parsec
import Text.Parsec.Char 
import Text.Parsec.Combinator as P

type Star = ((Int, Int, Int), (Int, Int, Int))

energy :: [Star] -> Int
energy ps = sum (energy' <$> ps)

energy' :: Star -> Int
energy' ((px, py, pz), (vx, vy, vz)) = (sum $ abs <$> [px, py, pz]) * (sum $ abs <$> [vx, vy, vz])

stepf :: [Star] -> [Star]
stepf = avelocity . agravity

agravity :: [Star] -> [Star]
agravity ss = [nx | 
    a <- ss,
    let bs = [b | b <- ss, a /= b],
    let nx = F.foldl ag a bs
    ]

ag :: Star -> Star -> Star
ag ((px, py, pz), (vx, vy, vz)) ((px', py', pz'), (vx', vy', vz')) = ((px, py, pz), 
    (vx + (f px px'), vy + (f py py'), vz + (f pz pz')))

avelocity :: [Star] -> [Star]
avelocity = fmap av

av :: Star -> Star
av ((px, py, pz), (vx, vy, vz)) = ((px+vx, py+vy, pz+vz), (vx, vy, vz))

stepf' :: [(Int, Int)] -> [(Int, Int)]
stepf' = avelocity' . agravity'

agravity' :: [(Int, Int)] -> [(Int, Int)]
agravity' ss = [nx | 
    a <- ss,
    let bs = [b | b <- ss, a /= b],
    let nx = F.foldl ag' a bs
    ]

ag' :: (Int, Int) -> (Int, Int) -> (Int, Int)
ag' (x, v) (x', _) = (x, v + f x x')

f :: Int -> Int -> Int
f a b 
    | a == b = 0
    | b > a = 1
    | otherwise  = -1

avelocity' :: [(Int, Int)] -> [(Int, Int)]
avelocity' = fmap av'

av' :: (Int, Int) -> (Int, Int)
av' (p, v) = (p + v, v)

parseInt :: Parsec String () Int
parseInt = do
    mminus <- P.option "" (string "-")
    digits <- many1 digit
    return (read (mminus <> digits))

parseInput :: Parsec String () [(Int, Int, Int)]
parseInput = endBy1 (do
    string "<x="
    x <- parseInt
    string ", y="
    y <- parseInt
    string ", z="
    z <- parseInt
    string ">"
    return (x, y, z)) (string "\n")

solution1 :: Int -> String -> String
solution1 steps input = let
    (Right i') = parse parseInput "" input
    i = (, (0,0,0)) <$> i'
    in show $ energy $ (L.iterate stepf i) !! steps
        
solution2 :: String -> String
solution2 input = let
    (Right i') = parse parseInput "" input
    i = (, (0,0,0)) <$> i'
    ix = (\((p, _, _), (v, _, _)) -> (p,v)) <$> i
    iy = (\((_, p, _), (_, v, _)) -> (p,v)) <$> i
    iz = (\((_, _, p), (_, _, v)) -> (p,v)) <$> i
    x' = L.elemIndices ix (L.iterate stepf' ix) !! 1
    y' = L.elemIndices iy (L.iterate stepf' iy) !! 1
    z' = L.elemIndices iz (L.iterate stepf' iz) !! 1
    in show $ x' `lcm` y' `lcm` z'

main :: IO ()
main = advent 2019 12 [solution1 1000, solution2] $ do
    let zs = (0, 0, 0)
    let in1 = [((-8, -10, 0),zs),((5,  5,  10),zs),((2,-7,3) ,zs),((9,  -8,  -3),zs)]
    peek $ (L.iterate stepf in1) !! 0
    peek $ (L.iterate stepf in1) !! 1
    peek $ (L.iterate stepf in1) !! 10
    peek $ (L.iterate stepf in1) !! 100
    peek $ show $ energy $ (L.iterate stepf in1) !! 100
    return ()