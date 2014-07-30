module Pins.Bot (runBot, Config(..)) where

import           Pins.Handle
import           Pins.Bot.Login
import qualified Network.WebSockets as WS
import           Control.Monad
import           Control.Applicative ((<$>))
import qualified Data.Text          as T

data Config = Config { name   :: String
                     , pass   :: String
                     , server :: String
                     , port   :: Int
                     , path   :: String
                     } deriving (Show)

data Bot = Bot { bName       :: String
               , bPass       :: String 
               , bConn       :: WS.Connection
               }

actionsToIO :: Bot -> [Action] -> IO ()
actionsToIO b = mapM_ (actionToIO b)

actionToIO :: Bot -> Action -> IO ()
actionToIO b (Send s) = WS.sendTextData (bConn b) . T.pack $ (s ++ "\n")
actionToIO _ (Print s) = putStrLn s
actionToIO b (Login key chall) = do
  assertion <- getAssertion (bName b) (bPass b) key chall
  actionToIO b (Send ("|/trn " ++ bName b ++ ",0," ++ assertion))

bot :: Config -> WS.ClientApp ()
bot config conn = do
  putStrLn "Connected"
  forever $
    liftM T.unpack (WS.receiveData conn) >>=
    actionsToIO b . handle
  where b = Bot (name config) (pass config) conn

runBot :: Config -> IO ()
runBot c = WS.runClient (server c) (port c) (path c) (bot c)
