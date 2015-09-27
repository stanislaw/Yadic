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
--import Control.Monad ((>=>), (<=<))

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

getTranslation :: YadicArguments -> IO (Either String YandexDictionaryResult)
getTranslation arguments = do
  let url = "https://dictionary.yandex.net/api/v1/dicservice.json/lookup?lang=" ++ (lng arguments) ++ "&key=" ++ (apiKey arguments) ++ "&text=" ++ (word arguments)
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

printResultOrError :: Either String YandexDictionaryResult -> IO ()
printResultOrError translation = do
      case translation of
        Left err -> putStrLn err
        Right translation -> printYandexDictionaryResult translation

data YadicConfiguration =
  YadicConfiguration {
    lang :: !String,
    apikey :: !String
  } deriving (Show, Generic)

instance FromJSON YadicConfiguration

data YadicArguments =
  YadicArguments {
    lng :: !String,
    word :: !String,
    apiKey:: !String
  } deriving (Show, Generic)


checkFileExist :: String -> IO (Either String String)
checkFileExist fileFullPath = do
  fileExists <- doesFileExist fileFullPath
  if fileExists
    then return $ Right fileFullPath
    else return $ Left "The configuration file doesn't exist!"

readYadicConfiguration :: Either String String -> IO (Either String YadicConfiguration)
readYadicConfiguration filePath =
    case filePath of
        Right path -> do
            yamlConf <- BS.readFile path
            return $ Right $ fromJust $ Data.Yaml.decode yamlConf
        Left err -> return $ Left err

getYadicArguments :: [String] -> Either String YadicConfiguration -> Either String YadicArguments
getYadicArguments args configuration = do
    conf <- configuration
    case args of
        [word] -> return $ YadicArguments (lang conf) word (apikey conf)
        [lng, word] -> return $ YadicArguments lng word (apikey conf)
        _ -> Left $ "Yadic, version 0.1. Powered by Yandex.Dictionary https://tech.yandex.com/dictionary/.\n" ++
                  "Usage:" ++
                  "  yadic [word]" ++
                  "  yadic [lang] [word]" ++
                  "  Example: yadic de-en kamille\n" ++
                  "Configuration: create ~/.yadic configuration file with the following structure (YAML):\n" ++
                  "lang: en-en # Default pair of languages to translate (from-to)\n" ++
                  "apikey: dict.1.1.20150711T192659Z... # your Yandex Dictionary API key"

main = do
  args <- getArgs
  configuration <- getHomeDirectory >>= (\path -> return $ combine path ".yadic")
                                    >>= checkFileExist
                                    >>= readYadicConfiguration

  let arguments = getYadicArguments args configuration

  case arguments of
      Right arg -> do
          translation <- getTranslation arg
          printResultOrError translation
      Left err -> putStrLn err
