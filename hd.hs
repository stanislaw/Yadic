{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import System.Environment(getArgs)
import Network.HTTP.Conduit(withManager, parseUrl, responseBody, httpLbs)
import Data.Text (Text)
import Data.Aeson

import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)
import GHC.Generics
import Debug.Trace

apiKey = "dict.1.1.20150714T192659Z.ececbd899bdd6716.e710db82e5a002c35ce71d6bf1e5d01a54e329eb"

data YandexTranslation =
  YandexTranslation {
    text :: !String
  } deriving (Show, Generic)

data YandexDefinition = 
  YandexDefinition {
    pos :: !String,
    tr :: [YandexTranslation]
  } deriving (Show, Generic)

data YandexDictionaryResult =
  YandexDictionaryResult {
    head :: !Object,
    def :: [YandexDefinition]
  } deriving (Show, Generic)

instance FromJSON YandexTranslation
instance ToJSON YandexTranslation
instance FromJSON YandexDefinition
instance ToJSON YandexDefinition
instance FromJSON YandexDictionaryResult
instance ToJSON YandexDictionaryResult

getTranslation2 :: String -> String -> IO (Either String YandexDictionaryResult)
getTranslation2 lang word = do
  let url = "https://dictionary.yandex.net/api/v1/dicservice.json/lookup?lang=" ++ lang ++ "&key=" ++ apiKey ++ "&text=" ++ word

  request <- parseUrl url
  res <- withManager $ \manager -> httpLbs request manager

  -- liftIO $ L.putStrLn $ responseBody res

  return $ eitherDecode $ responseBody res

getTranslation :: String -> IO (Either String YandexDictionaryResult)
getTranslation word = getTranslation2 "en-en" word

yandexTranslation :: YandexTranslation -> String
yandexTranslation tr = text tr

yandexDefinition :: YandexDefinition -> String
yandexDefinition def = unwords (map yandexTranslation (tr def))

printYandexDictionaryResult :: YandexDictionaryResult -> IO ()
printYandexDictionaryResult result = do
  -- putStrLn "Meanings:"
  mapM_ putStrLn (map yandexDefinition (def result))
  return ()

main = do 
  args <- getArgs
  case args of
    [word] -> do
      translation <- getTranslation word
      case translation of
        Left err -> putStrLn err
        Right translation -> printYandexDictionaryResult translation
      return ()
    _ -> do
      putStrLn "Enter a word"

