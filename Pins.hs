module Main where

import Pins.Bot
import Control.Monad
import System.Directory

generateConfig :: [String] -> Config
generateConfig ss = Config (arg 0) (arg 1) (arg 2) (read . arg $ 3) (arg 4) (lines $ arg 5) --VERY UGLY AND BAD FUNCTION, CHANGE TO USE YAML OR SOMETHING LATER
    where arg = (ss!!)

testConfig :: [String] -> IO Config
testConfig ss
    | length ss < 6 = putStrLn "Config File too short." >>
                      defaultConfig
    | otherwise = return . generateConfig $ ss

getConfig :: Bool -> IO Config
getConfig True  = readFile "config" >>= testConfig . lines
getConfig False = defaultConfig

defaultConfig :: IO Config
defaultConfig = putStrLn "No valid config file found, defaulting to Smogon server" >>
                liftM3 (\x y -> Config x y "sim.smogon.com" 8000 "/showdown/websocket")
                (prompt "Username?")
                (prompt "Password?")
                (promptList "Rooms?")

promptList :: String -> IO [String]
promptList s = putStrLn s >>
               getLine >>= \line ->
               case line of
                 "" -> return []
                 r  -> (r :) `liftM` promptList s

prompt :: String -> IO String
prompt s = putStrLn s >>
           getLine

main :: IO ()
main = 
    doesFileExist "config" >>=
    getConfig >>= runBot
