module Main where

import System.Environment(getArgs)

performHttpRequest :: String -> IO ()
performHttpRequest request = do
  putStrLn request

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

