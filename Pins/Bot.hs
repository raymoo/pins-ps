
module Pins.Bot (runBot, Config(..)) where

import           Pins.Handle.MonadAction.BotIO
import           Pins.Handle
import           Control.Monad.State.Lazy
import qualified Network.WebSockets as WS
import           Control.Monad
import           Control.Exception.Base (catch)
import qualified Data.Text          as T

bot :: Config -> WS.ClientApp ()
bot config conn = do
  putStrLn "Connected"
  evalStateT loop b
  where b = Bot (name config) (pass config) conn blankVar config

runBot :: Config -> IO ()
runBot c = WS.runClient (server c) (port c) (path c) (bot c)

loop :: StateT Bot IO ()
loop = forever $ get >>= 
                 lift . getData >>= 
                 handle

getData :: Bot -> IO String
getData b = catch (liftM T.unpack . WS.receiveData . bConn $ b) ((\e -> startBotAgain b >>
                                                                       return "This will never return") :: WS.ConnectionException -> IO String)

startBotAgain :: Bot -> IO ()
startBotAgain b = WS.runClient (server c) (port c) (path c) (\x -> evalStateT loop b { bConn = x })
    where c = bConfig b
