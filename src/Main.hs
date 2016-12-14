module Main where

import System.Environment

prompt :: String 
prompt = "Please enter your name: "

main :: IO ()
main = do
    putStrLn prompt
    name <- getLine
    putStrLn $ "Hello, " ++ name
