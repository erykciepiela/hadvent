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
import qualified Data.Map as M

import Text.Parsec as P
import Text.Parsec.String

import Debug.Trace

import Data.Either

inputParser :: Parser Int
inputParser = do
  ruleMap <- M.fromList <$> ruleParser `sepEndBy` newline
  newline
  expressions <- many1 (oneOf "ab") `sepEndBy` newline
  eof
  let expressionParser = grammarParser ruleMap 0
  return $ length $ L.filter isRight (parse (expressionParser >> eof) "" <$> expressions)
    where
      ruleParser :: Parser (Int, String)
      ruleParser = do
        ruleNo <- num
        string ": "
        str <- many1 (noneOf "\n")
        return (ruleNo, str)

grammarParser :: M.Map Int String -> Int -> Parser ()
grammarParser ruleMap 0 = do
  p42s <- many1 $ grammarParser ruleMap 42
  p31s <- many1 $ grammarParser ruleMap 31
  if length p42s > 1 && length p42s > length p31s then return () else fail "!"
grammarParser ruleMap ruleNo = let
  ruleStr = fromMaybe (error "rule not found") $ M.lookup ruleNo ruleMap
  in either (\e -> error $ "wrong parser for rule " <> show ruleNo <> " " <> show e) id $ parse grammarParser' "" ruleStr
    where
      grammarParser' :: Parser (Parser ())
      grammarParser' = do
        p <- try parseQuotedChar <|> parseAlts
        eof
        return p
        where
          parseQuotedChar :: Parser (Parser ())
          parseQuotedChar = void . char <$> between (char '"') (char '"') anyChar
          parseAlts :: Parser (Parser ())
          parseAlts = do
            (alts :: [[Int]]) <- (num `sepEndBy` char ' ') `sepBy` string "| "
            return $ do
              P.choice (alts <&> (\alt -> try $ mconcat $ alt <&> grammarParser ruleMap))

num :: Parser Int
num = read <$> many1 digit

solution1 :: String -> Int
solution1 input = let
  ls = either (error "wrong input parser") id $ parse inputParser "" input
  in ls

main :: IO ()
main = advent 2020 19 [solution1] $ do
  return ()
