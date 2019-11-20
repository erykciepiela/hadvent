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

year :: Int
year = 2018

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

runAdvent :: Int -> (String -> String) -> IO ()
runAdvent day advent = do
    exists <- doesFileExist inputFile
    input <- if exists then Prelude.readFile inputFile else do
        session <- BS.readFile ".session"
        i <- downloadInput day session
        Prelude.writeFile inputFile i
        return i
    Prelude.putStrLn $ advent input 
        where
            inputFile = show day <> "/input.txt"