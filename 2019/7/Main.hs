module Main where

import Advent
import Data.Text as T
import qualified Data.List as L
import qualified Data.Set as S
import Control.Monad
import Control.Applicative
import Data.Map as P
import Data.Maybe
import Control.Monad.Cont
import Control.Exception

solution1 :: String -> String
solution1 input = input

solution2 :: String -> String
solution2 input = input

main :: IO ()
main = advent $ do
    check id 1 1
    check id 2 2
    peek id 3 
    solution 2019 6 (const "abc")
    solution 2019 6 (const "def")

peek :: Show o => (i -> o) -> i -> Advent a
peek f i = cont $ \k -> putStrLn $ "Peek:\n" <> show (f i)

check :: (Show i, Show o, Eq o) => (i -> o) -> i -> o -> Advent ()
check f i o = cont $ \k -> do
    mFail <- test' f i o
    case mFail of
        Nothing -> k ()
        Just fail -> putStrLn $ "Failure: \n" <> fail
        where
            test' :: (Show i, Show o, Eq o) => (i -> o) -> i -> o -> IO (Maybe String)
            test' solution i o = do
                (eactual :: Either SomeException o) <- try $ evaluate $ solution i
                return $ case eactual of
                    Right actual -> if actual /= o
                        then Just $ "for:\n" <> show i <> "\nexpected:\n" <> show o <> "\nactually:\n" <> show actual
                        else Nothing
                    Left e -> Just $ "for:\n" <> show i <> "\nexpected:\n" <> show o <> "\nactually:\n" <> show e

solution :: Int -> Int -> (String -> String) -> Advent a
solution year day s = cont $ \k -> do
    answer <- runAdvent' year day s
    putStrLn $ "Answer:\n" <> answer

type Advent a = Cont (IO ()) a

advent :: Advent (IO ()) -> IO ()
advent = flip runCont id