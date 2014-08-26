
module Pins.Bot (runBot, Config(..)) where

import           Pins.Handle.MonadAction.BotIO
import           Pins.Handle
import           Control.Monad.State.Lazy
import qualified Network.WebSockets as WS
import           Control.Monad
import           Data.Acid
import           Control.Concurrent.Chan
import           Control.Concurrent
import           Control.Exception (Exception, catch, SomeException(..))
import qualified Data.Text          as T

-- catches exceptions in the StateT s IO monad (not thread safe)
catchStateTIO    :: Exception e => StateT s IO a -> (e -> StateT s IO a) -> StateT s IO a
catchStateTIO x f = do 
                       initial <- get
                       (res, s) <- liftIO $ catch (runStateT x initial)
                                                  (flip runStateT initial . f)
                       put s
                       return res

messageSender :: WS.Connection -> Chan T.Text -> ThreadId -> IO ()
messageSender c tc ti = catch messageLoop
                              (throwTo ti :: SomeException -> IO ()) 
    where messageLoop = forever $ readChan tc >>=
                                  WS.sendTextData c >>
                                  threadDelay 100000

bot :: Config -> WS.ClientApp ()
bot config conn = do
  putStrLn "Connected"
  as <- openLocalState initialPerma
  chan <- liftIO $ makeSender conn
  evalStateT loop (b as chan)
  where b = Bot (name config) (pass config) conn blankVar config

runBot :: Config -> IO ()
runBot c = WS.runClient (server c) (port c) (path c) (bot c)

loop :: StateT Bot IO ()
loop = forever $ catchStateTIO (get >>= 
                                lift . getData >>= 
                                handle)
                               ((\e -> (liftIO . print $ e) >> 
                                 get >>= 
                                 liftIO . startBotAgain) :: SomeException -> StateT Bot IO ())

makeSender :: WS.Connection -> IO (Chan T.Text)
makeSender con = newChan >>= \chan ->
                 myThreadId >>= \tid ->
                 forkIO (messageSender con chan tid) >>
                 return chan

altBot :: Bot -> WS.ClientApp ()
altBot b conn = do
  putStrLn "reconnected"
  chan <- liftIO $ makeSender conn
  let bot = b { bConn = conn, messChan = chan }
  evalStateT loop bot 

getData :: Bot -> IO String
getData = liftM T.unpack . WS.receiveData . bConn

startBotAgain :: Bot -> IO ()
startBotAgain b = putStrLn "Trying to reconnect/restart..." >>
                  newChan >>= \chan ->
                  myThreadId >>= \tid ->
                  catch (WS.runClient (server c) (port c) (path c) (altBot b))
                        ((\_ -> startBotAgain b) :: SomeException -> IO ())
    where c = bConfig b
