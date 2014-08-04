module Main where

import Pins.Bot
import Control.Monad
import System.Directory

generateConfig :: [String] -> Config
generateConfig ss = Config (arg 0) (arg 1) (arg 2) (read . arg $ 3) (arg 4) --VERY UGLY AND BAD FUNCTION, CHANGE TO USE YAML OR SOMETHING LATER
    where arg = (ss!!)

testConfig :: [String] -> IO Config
testConfig ss
    | length ss < 5 = putStrLn "Config File too short." >>
                      defaultConfig
    | otherwise = return . generateConfig $ ss

getConfig :: Bool -> IO Config
getConfig True  = readFile "config" >>= testConfig . lines
getConfig False = defaultConfig

defaultConfig :: IO Config
defaultConfig = putStrLn "No valid config file found, defaulting to Smogon server" >>
                liftM2 (\x y -> Config x y "sim.smogon.com" 8000 "/showdown/websocket")
                (prompt "Username?")
                (prompt "Password?")

prompt :: String -> IO String
prompt s = putStrLn s >>
           getLine

main :: IO ()
main = 
    doesFileExist "config" >>=
    getConfig >>= runBot
