module Advent (
    runAdvent,
    runAdvent',
    test,
    peek,
    check,
    solution,
    advent,
    Advent
) where

import Data.ByteString as BS
import Data.ByteString.Char8 as C
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import Control.Monad
import System.Directory
import Control.Exception
import Control.Monad.Cont

httpsGet :: Manager -> BS.ByteString -> String -> IO String
httpsGet manager session url = do
    re <- setRequestHeader "cookie" ["session=" <> session] <$> parseRequest url
    let request = setRequestManager manager re
    response <- httpBS request
    return $ C.unpack $ getResponseBody response

downloadInput :: Int -> Int -> BS.ByteString -> IO String
downloadInput year day session = do
    manager <- newManager tlsManagerSettings
    httpsGet manager session $ url day
    where
        url :: Int -> String
        url day = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"

runAdvent :: Int -> Int -> (String -> String) -> [(String, String)] -> IO ()
runAdvent year day solution examples = do
    exists <- doesFileExist inputFile
    input <- if exists then C.unpack <$> C.readFile inputFile else do
        session <- BS.readFile ".session"
        i <- downloadInput year day session
        Prelude.writeFile inputFile i
        return i
    testResult <- testAdvent solution examples
    case testResult of
        Nothing -> do
            let answer = solution input
            Prelude.putStrLn $ "OK, your solution:\n" <> answer
        Just fail -> Prelude.putStrLn $ "Fail\n" <> fail
        where
            inputFile = show year <> "/" <> show day <> "/input.txt"

testAdvent :: (Show i, Show o, Eq o) => (i -> o) -> [(i, o)] -> IO (Maybe String)
testAdvent solution [] = return Nothing
testAdvent solution ((i, o):ios) = do
    (eactual :: Either SomeException o) <- try $ evaluate $ solution i
    case eactual of
        Right actual -> if actual /= o
            then return $ Just ("for:\n" <> show i <> "\nexpected:\n" <> show o <> "\nactually:\n" <> show actual) 
            else testAdvent solution ios
        Left e -> return $ Just ("for:\n" <> show i <> "\nexpected:\n" <> show o <> "\nactually:\n" <> show e) 

test :: (Show i, Show o, Eq o) => (i -> o) -> i -> o -> IO ()
test solution i o = do
    (eactual :: Either SomeException o) <- try $ evaluate $ solution i
    case eactual of
        Right actual -> if actual /= o
            then Prelude.putStrLn $ "for:\n" <> show i <> "\nexpected:\n" <> show o <> "\nactually:\n" <> show actual
            else return ()
        Left e -> Prelude.putStrLn $ "for:\n" <> show i <> "\nexpected:\n" <> show o <> "\nactually:\n" <> show e
        
runAdvent' :: Int -> Int -> (String -> String) -> IO String
runAdvent' year day solution = do
    exists <- doesFileExist inputFile
    input <- if exists then C.unpack <$> C.readFile inputFile else do
        session <- BS.readFile ".session"
        i <- downloadInput year day session
        Prelude.writeFile inputFile i
        return i
    return $ solution input
        where
            inputFile = show year <> "/" <> show day <> "/input.txt"
        
-- new API

peek :: Show o => o -> Advent ()
peek o = cont $ \k -> do
    Prelude.putStrLn $ "Peek:\n" <> show o
    k ()

check :: (Show i, Show o, Eq o) => (i -> o) -> i -> o -> Advent ()
check f i o = cont $ \k -> do
    mFail <- test' f i o
    case mFail of
        Nothing -> k ()
        Just fail -> Prelude.putStrLn $ "Failure: \n" <> fail
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
    Prelude.putStrLn $ "Answer:\n" <> answer

type Advent a = Cont (IO ()) a

advent :: Int -> Int -> (String -> String) -> Advent a -> IO ()
advent year day s a = flip runCont id $ do
    a
    solution year day s 