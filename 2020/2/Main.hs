module Main where

import Advent
import Text.Parsec as P
import Text.Parsec.String

solution1 :: String -> String
solution1 input = let
  logs = (\l -> let (Right s) = l in s) . parsePasswordLog <$> lines input
  in show $ length $ filter wasPasswordCorrect logs

solution2 :: String -> String
solution2 input = let
  logs = (\l -> let (Right s) = l in s) . parsePasswordLog <$> lines input
  in show $ length $ filter wasPasswordCorrect2 logs

data PasswordLog = PasswordLog {
  letter :: Char,
  occurs :: (Int, Int),
  password :: String
} deriving Show

wasPasswordCorrect :: PasswordLog -> Bool
wasPasswordCorrect (PasswordLog letter (min, max) password) = let
  letterOccurs = length (filter (== letter) password)
  in letterOccurs >= min && letterOccurs <= max

wasPasswordCorrect2 :: PasswordLog -> Bool
wasPasswordCorrect2 (PasswordLog letter (min, max) password) = (password !! (min - 1) == letter) /= (password !! (max - 1) == letter)

parsePasswordLog :: String -> Either ParseError PasswordLog
parsePasswordLog s = parse foo "" s

foo :: Parser PasswordLog
foo = do
  min <- many1 digit
  char '-'
  max <- many1 digit
  char ' '
  letter <- P.letter
  char ':'
  char ' '
  password <- many1 anyChar
  return $ PasswordLog letter (read min, read max) password


main :: IO ()
main = advent 2020 2 [solution2] $ do
    solution1 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc" `shouldBe` "2"
    solution2 "1-3 a: abcde\n1-3 b: cdefg\n2-9 c: ccccccccc" `shouldBe` "1"
    -- solution2 "1721\n979\n366\n299\n675\n1456" `shouldBe` "241861950"
    -- peek parsePasswordLog
    return ()

