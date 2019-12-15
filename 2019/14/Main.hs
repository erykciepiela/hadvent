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
-- import Algebra.PartialOrd
-- import Data.Poset as PO


parseInt :: Parsec String () Int
parseInt = do
    digits <- many1 digit
    return (read digits)

parseReaction :: Parsec String () Reaction
parseReaction = do
    subs <- sepBy1 (do
        substractQty <- many1 digit
        string " "
        substract <- many1 alphaNum
        return (substract, read substractQty)) (string ", ")
    string " => "
    proQty <- many1 digit
    string " "
    pro <- many1 alphaNum
    -- string "\n"
    return $ Reaction pro (read proQty) subs

parseReactions :: Parsec String () [Reaction]
parseReactions = endBy1 parseReaction (string "\n")

type Substance = String
type Substract = (Substance, Integer)

data Reaction = Reaction {
    rproduct :: Substance,
    producedQty :: Integer,
    substracts :: [(Substance, Integer)]
} deriving (Show, Eq)

instance Ord Reaction where
    r1 `compare` r2 = rproduct r1 `compare` rproduct r2

-- instance PartialOrd Reaction where
--     r1 `leq` r2 = L.elem (rproduct r2) (fst <$> substracts r1)

data Needed = Needed {
    reaction :: Reaction,
    reactionTimes :: Integer
} deriving (Show)

-- instance PartialOrd Needed where
--     n1 `leq` n2 = reaction n1 `leq` reaction n2

instance Eq Needed where
    n1 == n2 = reaction n1 == reaction n2


depth :: [Reaction] -> Reaction -> Int
depth rs r = if fst (L.head (substracts r)) == "ORE" then 0 else (+1) $ L.maximum $ (\(str, _) -> let (Just r) = L.find (\r' -> rproduct r' == str) rs in depth rs r) <$> substracts r

-- sortNeeded :: [Needed] -> [Needed]
-- sortNeeded ns = let
--     leaves = L.filter (\n -> fst (L.head (substracts (reaction n))) == "ORE") ns
--     in (f ns leaves) <> leaves
--         where
--         f :: [Needed] -> [Needed] -> [Needed]
--         f ns allowed = L.filter (\n -> fst <$> (substracts (reaction n))) ns 

-- let 
--     l' = (PO.toList . PO.fromList) l
--     in l'
    -- l' = PO.empty
    -- F.foldr (\n ns -> PO.inse)
    -- in if check l' then l' else error "!"

-- check :: [Needed] -> Bool
-- check [n] = True
-- check (n1:n2:ns) = (n1 `leq` n2 || not (comparable n1 n2)) && check (n2:ns)

foo :: [Reaction] -> (Integer, [Needed]) -> (Integer, [Needed])
foo rs (i, []) = (i, [])
-- foo rs (i, ns) = trace ((show (i, ns)) <> "\n") $ let
foo rs (i, ns) = let
    Needed r qty = L.head ns
    -- nr = toInteger $ ceiling $ (fromIntegral qty) / fromIntegral (producedQty r)
    nr = qty `div` producedQty r + if qty `mod` producedQty r > 0 then 1 else 0
    strs = (\(str, strq) -> maybe (Right (nr * strq)) (\r' -> Left (Needed r' (nr * strq))) (L.find (\r -> rproduct r == str) rs)) <$> substracts r
    (i', ns') = F.foldr (\e (si, ns) -> case e of Right i -> (si + i, ns); Left n -> (si, n:ns)) (i, L.tail ns) strs
    ns'' = L.sortOn (negate . depth rs . reaction) $ (\(r, t) -> Needed r t) <$> (M.toList . M.fromListWith (+) $ (\(Needed r t) -> (r, t)) <$> ns')
    in foo rs (i', ns'')

solution1 :: String -> String
solution1 s = let 
    (Right rs) = parse parseReactions "" s 
    (Just fuelReaction) = L.find (\r -> rproduct r == "FUEL") rs
    in show $ fst $ foo rs (0, [Needed fuelReaction 1])

solution2 :: String -> String
solution2 s = let 
    (Right rs) = parse parseReactions "" s 
    (Just fuelReaction) = L.find (\r -> rproduct r == "FUEL") rs
    -- in show $ (fst $ foo rs (0, [Needed fuelReaction 3058799]))
    -- in show $ (fst $ foo rs (0, [Needed fuelReaction 4058799]))
    in show $ (fst $ foo rs (0, [Needed fuelReaction 3848998]))
    -- in show $ (fst $ foo rs (0, [Needed fuelReaction 3848999]))
    -- in show $ binSearch 3058799 4058799 1000000000000 (\x -> fst $ foo rs (0, [Needed fuelReaction x]))
    -- 3058799 4058799
main :: IO ()
main = advent 2019 14 [solution1, solution2] $ do
    solution1 "157 ORE => 5 NZVS\n165 ORE => 6 DCFZ\n44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n179 ORE => 7 PSHF\n177 ORE => 5 HKGWZ\n7 DCFZ, 7 PSHF => 2 XJWVT\n165 ORE => 2 GPVTF\n3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT\n" `shouldBe` "13312"
    solution1 "2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n17 NVRVD, 3 JNWZP => 8 VPVL\n53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n22 VJHF, 37 MNCFX => 5 FWMGM\n139 ORE => 4 NVRVD\n144 ORE => 7 JNWZP\n5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n145 ORE => 6 MNCFX\n1 NVRVD => 8 CXFTF\n1 VJHF, 6 MNCFX => 4 RFSQX\n176 ORE => 6 VJHF\n" `shouldBe` "180697"
    solution1 "171 ORE => 8 CNZTR\n7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n114 ORE => 4 BHXH\n14 VRPVC => 6 BMBT\n6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n5 BMBT => 4 WPTQ\n189 ORE => 9 KTJDG\n1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n12 VRPVC, 27 CNZTR => 2 XDBXC\n15 KTJDG, 12 BHXH => 5 XCVML\n3 BHXH, 2 VRPVC => 7 MZWV\n121 ORE => 7 VRPVC\n7 XCVML => 6 RJRHP\n5 BHXH, 4 VRPVC => 5 LTCX\n" `shouldBe` "2210736"
    peek $ 1000000000000 / 485720 
    return ()

binSearch :: Integer -> Integer -> Integer -> (Integer -> Integer) -> Integer
binSearch from to exp f 
  | from == to = from
  | otherwise = let q = f ((from + to) `div` 2) in if trace (show ((from + to) `div` 2)) q > exp 
    then binSearch from ((from + to) `div` 2) exp f
    else binSearch ((from + to) `div` 2) to exp f