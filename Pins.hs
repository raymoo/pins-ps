module Main where

import Pins.Bot

generateConfig :: [String] -> Config
generateConfig ss = Config (arg 0) (arg 1) (arg 2) (read . arg $ 3) (arg 4) --VERY UGLY AND BAD FUNCTION, CHANGE TO USE YAML OR SOMETHING LATER
    where arg = (ss!!)

runWhat :: [String] -> IO ()
runWhat ss
    | length ss < 5 = putStrLn "Invalid Config File"
    | otherwise = runBot . generateConfig $ ss

main :: IO ()
main = 
    readFile "config" >>=
    runWhat . lines
