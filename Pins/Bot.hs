
module Pins.Bot (runBot, Config(..)) where

import           Pins.Handle.MonadAction.BotIO
import           Pins.Handle
import           Control.Monad.State.Lazy
import qualified Network.WebSockets as WS
import           Control.Monad
import qualified Data.Text          as T

data Config = Config { name   :: String
                     , pass   :: String
                     , server :: String
                     , port   :: Int
                     , path   :: String
                     } deriving (Show)

bot :: Config -> WS.ClientApp ()
bot config conn = do
  putStrLn "Connected"
  evalStateT (loop conn) b
  where b = Bot (name config) (pass config) conn blankVar

runBot :: Config -> IO ()
runBot c = WS.runClient (server c) (port c) (path c) (bot c)

loop :: WS.Connection -> StateT Bot IO ()
loop conn = forever $ lift (getData conn) >>= 
                      handle

getData :: WS.Connection -> IO String
getData = liftM T.unpack . WS.receiveData
