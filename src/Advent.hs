module Advent (
    runAdvent
) where

import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.ByteString.Lazy.Char8 as C
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple
import Control.Monad
import System.Directory
import Control.Exception

year :: Int
year = 2018 -- TODO: change to 2019

httpsGet :: Manager -> BS.ByteString -> String -> IO String
httpsGet manager session url = do
    re <- setRequestHeader "cookie" ["session=" <> session] <$> parseRequest url
    let request = setRequestManager manager re
    response <- httpLBS request
    return $ C.unpack $ getResponseBody response

downloadInput :: Int -> BS.ByteString -> IO String
downloadInput day session = do
    manager <- newManager tlsManagerSettings
    httpsGet manager session $ url day
    where
        url :: Int -> String
        url day = "https://adventofcode.com/" <> show year <> "/day/" <> show day <> "/input"

runAdvent :: Int -> (String -> String) -> [(String, String)] -> IO ()
runAdvent day solution examples = do
    exists <- doesFileExist inputFile
    input <- if exists then Prelude.readFile inputFile else do
        session <- BS.readFile ".session"
        i <- downloadInput day session
        Prelude.writeFile inputFile i
        return i
    testResult <- testAdvent solution examples
    case testResult of
        Nothing -> do
            let answer = solution input
            Prelude.putStrLn $ "OK, your solution:\n" <> answer
        Just fail -> Prelude.putStrLn $ "Fail\n" <> fail
        where
            inputFile = show day <> "/input.txt"

testAdvent :: (String -> String) -> [(String, String)] -> IO (Maybe String)
testAdvent solution [] = return Nothing
testAdvent solution ((i, o):ios) = do
    (eactual :: Either SomeException String)<- try $ evaluate $ solution i
    case eactual of
        Right actual -> if actual /= o
            then return $ Just ("for:\n" <> i <> "\nexpected:\n" <> o <> "\nactually:\n" <> actual) 
            else testAdvent solution ios
        Left e -> return $ Just ("for:\n" <> i <> "\nexpected:\n" <> o <> "\nactually:\n" <> show e) 

