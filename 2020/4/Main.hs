module Main where

import Advent

import Text.Parsec as P
import Text.Parsec.String
import qualified Data.Map as M
import Data.Functor

testInput :: String
testInput = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
            \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
            \\n\
            \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
            \hcl:#cfa07d byr:1929\n\
            \\n\
            \hcl:#ae17e1 iyr:2013\n\
            \eyr:2024\n\
            \ecl:brn pid:760753108 byr:1931\n\
            \hgt:179cm\n\
            \\n\
            \hcl:#cfa07d eyr:2025 pid:166559648\n\
            \iyr:2011 ecl:brn hgt:59in\n\n"

invalidPassports :: String
invalidPassports =
  "eyr:1972 cid:100\n\
  \hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\
  \\n\
  \iyr:2019\n\
  \hcl:#602927 eyr:1967 hgt:170cm\n\
  \ecl:grn pid:012533040 byr:1946\n\
  \\n\
  \hcl:dab227 iyr:2012\n\
  \ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\
  \\n\
  \hgt:59cm ecl:zzz\n\
  \eyr:2038 hcl:74454a iyr:2023\n\
  \pid:3556412378 byr:2007\n"

validPassports :: String
validPassports =
  "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
  \hcl:#623a2f\n\
  \\n\
  \eyr:2029 ecl:blu cid:129 byr:1989\n\
  \iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\
  \\n\
  \hcl:#888785\n\
  \hgt:164cm byr:2001 iyr:2015 cid:88\n\
  \pid:545766238 ecl:hzl\n\
  \eyr:2022\n\
  \\n\
  \iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719\n"


fields = [
  "byr",
  "iyr",
  "eyr",
  "hgt",
  "hcl",
  "ecl",
  "pid",
  "cid"]

npfields = [
  "byr",
  "iyr",
  "eyr",
  "hgt",
  "hcl",
  "ecl",
  "pid"]

newtype Passport = Passport {
  passportFields :: M.Map String String
} deriving (Show, Eq)

passportsParser :: Parser [Passport]
passportsParser = passportParser `sepBy` char '\n'

passportParser :: Parser Passport
passportParser = do
  entries <- (do
    key <- many1 P.letter
    char ':'
    value <- many1 (P.letter <|> P.digit <|> char '#')
    return (key, value)) `sepEndBy` oneOf " \n"
  return $ Passport $ M.fromList entries

solution1 :: String -> String
solution1 input = let
  passports = either (error "invalid parser") id $ parse passportsParser "" input
  in show $ length $ filter (\p -> M.keys (passportFields p) `containsAll` fields || M.keys (passportFields p) `containsAll` npfields) passports
    where
      containsAll cs as = all (`elem` cs) as

solution2 :: String -> String
solution2 input = let
  passports = either (error "invalid parser") id $ parse passportsParser "" input
  in show $ length $ filter validPassport passports

validPassport :: Passport -> Bool
validPassport (Passport passportFields) = and $ fields <&> (\field -> case M.lookup field passportFields of
  Nothing -> field == "cid"
  (Just value) -> Right True == parse (fieldParser field) field value)
  where
    fieldParser :: String -> Parser Bool
    fieldParser "byr" = do
      digits <- read <$> count 4 digit
      eof
      return $ digits >= 1920 && digits <= 2002
    fieldParser "iyr" = do
      digits <- read <$> count 4 digit
      eof
      return $ digits >= 2010 && digits <= 2020
    fieldParser "eyr" = do
      digits <- read <$> count 4 digit
      eof
      return $ digits >= 2020 && digits <= 2030
    fieldParser "hgt" = do
      digits <- read <$> many1 digit
      choice
        [ do
          string "cm"
          eof
          return $ digits >= 150 && digits <= 193
        , do
          string "in"
          eof
          return $ digits >= 59 && digits <= 76
        ]
    fieldParser "hcl" = do
      char '#'
      count 6 (digit <|> oneOf "abcdef")
      eof
      return True
    fieldParser "ecl" = do
      choice $ try . string <$> ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
      eof
      return True
    fieldParser "pid" = do
      count 9 digit
      eof
      return True
    fieldParser "cid" = return True

main :: IO ()
main = advent 2020 4 [solution2] $ do
  let validPassports' = either (error "invalid parser") id $ parse passportsParser "" validPassports
  let invalidPassports' = either (error "invalid parser") id $ parse passportsParser "" invalidPassports
  all validPassport validPassports' `shouldBe` True
  not (any validPassport invalidPassports') `shouldBe` True
  return ()

