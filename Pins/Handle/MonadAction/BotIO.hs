{-# LANGUAGE FlexibleInstances #-}

module Pins.Handle.MonadAction.BotIO where

import           Pins.Bot.Login
import           Pins.Handle.MonadAction
import           Control.Monad.State.Lazy
import           Control.Applicative
import qualified Network.WebSockets       as WS
import qualified Data.Text                as T
import qualified Data.Map                 as M

data Bot = Bot { bName :: String
               , bPass :: String 
               , bConn :: WS.Connection
               , vars  :: M.Map String Var
               , bConfig :: Config
               }

data Config = Config { name   :: String
                     , pass   :: String
                     , server :: String
                     , port   :: Int
                     , path   :: String
                     } deriving (Show)


blankVar :: M.Map String Var
blankVar = M.empty

getName :: StateT Bot IO String
getName = bName <$> get

getPass :: StateT Bot IO String
getPass = bPass <$> get

getConn :: StateT Bot IO WS.Connection
getConn = bConn <$> get

getVars :: StateT Bot IO (M.Map String Var)
getVars = vars <$> get

instance MonadAction (StateT Bot IO) where
    send s = getConn >>= (\b ->
                          liftIO . WS.sendTextData b . T.pack $ s)
    printLn = liftIO . putStrLn
    login cKey chall = do
      name <- getName
      pass <- getPass
      assertion <- lift $ getAssertion name pass cKey chall
      command ("/trn " ++ name ++ ",0," ++ assertion)
    putVar k x = modify (\s -> s {vars = M.insert k x $ vars s})
    getVar k = M.lookup k <$> getVars
