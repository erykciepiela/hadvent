module Main where

import Advent

-- import Text.Parsec as P
import Text.Parsec.String
import Data.Functor
import Data.List as L
import Control.Monad
import Data.Set as S
import Data.Maybe
import Control.Comonad
import Data.Foldable as F
import Data.Monoid
import Text.Read
import Data.Bits
import Data.Map as M

import Text.Parsec as P
import Text.Parsec.String

import Debug.Trace

import Data.Either

grammarParser :: Parser (Parser ())
grammarParser = do
    snd . head <$> (grammarParser' (M.empty) `sepBy` newline)

grammarParser' :: M.Map Int (Parser ()) -> Parser (Int, Parser ())
grammarParser' otherParsers = do
  ruleNo <- read <$> many1 digit
  string ": "
  (parser :: Parser ()) <- try (void . char <$> quotedChar) <|> do
    (alts :: [[Int]]) <- try (num `sepBy` char ' ') `sepBy` string " | "
    return $ P.choice (alts <&> (\ints -> mconcat $ ints <&> lookupLax otherParsers))
  return (ruleNo, parser)

num :: Parser Int
num = read <$> many1 digit

quotedChar :: Parser Char
quotedChar = between (char '"') (char '"') anyChar

lookupLax :: Ord k => M.Map k v -> k -> v
lookupLax map k = let (Just v) = M.lookup k map in v

matches :: String -> String -> Bool
matches exp grammar = let
  expParser = either (error "wrong grammar parser") id $ parse grammarParser "" grammar
  in isRight $ parse (expParser >> eof) "" exp

testGrammar = "0: 4 1 5\n\
              \1: 2 3 | 3 2\n\
              \2: 4 4 | 5 5\n\
              \3: 4 5 | 5 4\n\
              \4: \"a\"\n\
              \5: \"b\""

solution1 :: String -> Int
solution1 input = let
  ls = lines input
  in 1

main :: IO ()
main = advent 2020 19 [solution1] $ do
  isRight (parse grammarParser "" "0: 4 1 5") `shouldBe` True
  isRight (parse grammarParser "" "1: \"a\"") `shouldBe` True
  isRight (parse grammarParser "" "1: 2 3 | 3 2") `shouldBe` True
  isRight (parse grammarParser "" testGrammar) `shouldBe` True
  -- ("aaaabb" `matches` testGrammar) `shouldBe` True
  -- ("aaabab" `matches` testGrammar) `shouldBe` True
  -- ("abbabb" `matches` testGrammar) `shouldBe` True
  -- ("abbbab" `matches` testGrammar) `shouldBe` True
  -- ("aabaab" `matches` testGrammar) `shouldBe` True
  -- ("aabbbbabaaab" `matches` testGrammar) `shouldBe` True
  return ()
