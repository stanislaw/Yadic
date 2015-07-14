module Main where

import System.Environment(getArgs)
import Network.HTTP.Conduit
import Data.Text
import Data.Text.Encoding
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)

apiKey = "dict.1.1.20150714T192659Z.ececbd899bdd6716.e710db82e5a002c35ce71d6bf1e5d01a54e329eb"

performHttpRequest :: String -> IO ()
performHttpRequest request = do
  let url = "https://dictionary.yandex.net/api/v1/dicservice.json/lookup?key=APIkey&lang=en-ru&text=time"
  response <- simpleHttp "http://www.haskell.org/"
  liftIO $ L.putStr $ (responseBody response)
  return ()

wordHttpRequest :: String -> String
wordHttpRequest word = word

getTranslation :: String -> IO ()
getTranslation word = performHttpRequest (wordHttpRequest word)

main = do 
  args <- getArgs
  case args of
    [word] -> do
      getTranslation word
    
    _ -> do
      putStrLn "Enter a word"

