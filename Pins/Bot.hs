
module Pins.Bot (runBot, Config(..)) where

import           Pins.Handle.MonadAction.BotIO
import           Pins.Handle
import           Control.Monad.State.Lazy
import qualified Network.WebSockets as WS
import           Control.Monad
import           Control.Exception (Exception, catch, SomeException)
import qualified Data.Text          as T

-- catches exceptions in the StateT s IO monad (not thread safe)
catchStateTIO    :: Exception e => StateT s IO a -> (e -> StateT s IO a) -> StateT s IO a
catchStateTIO x f = do 
                       initial <- get
                       (res, s) <- liftIO $ catch (runStateT x initial)
                                                  (flip runStateT initial . f)
                       put s
                       return res

bot :: Config -> WS.ClientApp ()
bot config conn = do
  putStrLn "Connected"
  evalStateT loop b
  where b = Bot (name config) (pass config) conn blankVar config

runBot :: Config -> IO ()
runBot c = WS.runClient (server c) (port c) (path c) (bot c)

loop :: StateT Bot IO ()
loop = catchStateTIO (forever $ get >>= 
                      lift . getData >>= 
                      handle)
                     ((\e -> get >>= liftIO . startBotAgain) :: SomeException -> StateT Bot IO ())

getData :: Bot -> IO String
getData = liftM T.unpack . WS.receiveData . bConn

startBotAgain :: Bot -> IO ()
startBotAgain b = WS.runClient (server c) (port c) (path c) (\x -> evalStateT loop b { bConn = x })
    where c = bConfig b
