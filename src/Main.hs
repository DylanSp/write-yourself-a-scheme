module Main where

import System.Environment

addTwoStrings :: String -> String -> String
addTwoStrings a b = show $ read a + read b

main :: IO ()
main = do
     args <- getArgs
     let arg0 = args !! 0
     let arg1 = args !! 1
     putStrLn (arg0 ++ " + " ++ arg1 ++ " = " ++ addTwoStrings arg0 arg1)
     where 
        
