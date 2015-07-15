{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import System.Environment(getArgs)
import System.Directory(doesFileExist, getHomeDirectory)
import System.FilePath.Posix(combine)
import System.Exit(exitFailure)
import Network.HTTP.Conduit(withManager, parseUrl, responseBody, httpLbs)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Aeson (eitherDecode, FromJSON, Object)
import Data.Yaml (decode)
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BS
import Control.Monad.IO.Class (liftIO)
import GHC.Generics

data YandexTranslation =
  YandexTranslation {
    text :: !String,
    pos :: !String
  } deriving (Show, Generic)

data YandexDefinition = 
  YandexDefinition {
    -- pos :: !String,
    tr :: [YandexTranslation]
  } deriving (Show, Generic)

data YandexDictionaryResult =
  YandexDictionaryResult {
    head :: !Object,
    def :: [YandexDefinition]
  } deriving (Show, Generic)

instance FromJSON YandexTranslation
instance FromJSON YandexDefinition
instance FromJSON YandexDictionaryResult

getTranslation :: String -> String -> String -> IO (Either String YandexDictionaryResult)
getTranslation apiKey lang word = do
  let url = "https://dictionary.yandex.net/api/v1/dicservice.json/lookup?lang=" ++ lang ++ "&key=" ++ apiKey ++ "&text=" ++ word

  request <- parseUrl url
  res <- withManager $ \manager -> httpLbs request manager

  -- liftIO $ L.putStrLn $ responseBody res

  return $ eitherDecode $ responseBody res

yandexTranslation :: YandexTranslation -> String
yandexTranslation tr = (text tr) ++ " (" ++ (pos tr) ++ ")"

yandexDefinition :: YandexDefinition -> String
yandexDefinition def = concat (intersperse " " (map yandexTranslation (tr def)))

printYandexDictionaryResult :: YandexDictionaryResult -> IO ()
printYandexDictionaryResult result = do
  mapM_ putStrLn (map yandexDefinition (def result))
  -- putStrLn "\nTranslation is provided by Yandex.Dictionary https://tech.yandex.com/dictionary/."
  return ()

data YadicConfiguration =
  YadicConfiguration {
    lang :: !String,
    apikey :: !String
  } deriving (Show, Generic)

instance FromJSON YadicConfiguration

main = do
  homePath <- getHomeDirectory 
  let configFile = combine homePath ".yadic"
  -- print $ configFile

  fileExists <- doesFileExist configFile
  if fileExists  
    then return()
    else do
      putStrLn "The configuration file doesn't exist!"
      exitFailure
  
  yamlConf <- BS.readFile configFile 

  let configuration = Data.Yaml.decode yamlConf :: Maybe YadicConfiguration
  -- print $ fromJust configuration

  let lng = lang $ fromJust configuration
  let apiKey = apikey $ fromJust configuration

  args <- getArgs
  case args of
    [word] -> do
      translation <- getTranslation apiKey lng word
      case translation of
        Left err -> putStrLn err
        Right translation -> printYandexDictionaryResult translation
      return ()
    [lang, word] -> do
      translation <- getTranslation apiKey lang word
      case translation of
        Left err -> putStrLn err
        Right translation -> printYandexDictionaryResult translation
      return ()
    _ -> do
      putStrLn "Yadic, version 0.1. Powered by Yandex.Dictionary https://tech.yandex.com/dictionary/.\n"
      putStrLn "Usage:"
      putStrLn "  yadic [word]"
      putStrLn "  yadic [lang] [word]"
      putStrLn "  Example: yadic de-en kamille\n"
      putStrLn "Configuration: create ~/.yadic configuration file with the following structure (YAML):"
      putStrLn "lang: en-en # Default pair of languages to translate (from-to)"
      putStrLn "apikey: dict.1.1.20150711T192659Z... # your Yandex Dictionary API key"

