{-# LANGUAGE FlexibleInstances, DeriveDataTypeable, TemplateHaskell, FlexibleContexts, TypeFamilies #-}

module Pins.Handle.MonadAction.BotIO where

import           Pins.Bot.Login
import           Pins.Handle.MonadAction
import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import           Control.Applicative
import           Data.Data
import           Data.Acid
import           Data.Acid.Advanced
import           Data.Acid.Local
import           Data.SafeCopy
import           Control.Concurrent.Chan
import qualified Network.WebSockets       as WS
import qualified Data.Text                as T
import qualified Data.Map                 as M

data Bot = Bot { bName :: String
               , bPass :: String 
               , bConn :: WS.Connection
               , vars  :: M.Map String Var
               , bConfig :: Config
               , acidState :: AcidState PermaStore
               , messChan :: Chan T.Text
               , timestamps :: M.Map String Integer
               }

defaultBot :: Bot
defaultBot = Bot ""
                 ""
                 (error "bot has no connection!!!")
                 M.empty
                 (error "bot has no config!!!")
                 (error "bot has no acid state!") 
                 (error "bot has no message queue!!!")
                 M.empty

data Config = Config { name   :: String
                     , pass   :: String
                     , server :: String
                     , port   :: Int
                     , path   :: String
                     , rooms  :: [String]
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

-- Acid state for Map
data PermaStore = PermaStore { permaStore :: M.Map String String }
                  deriving (Eq, Ord, Read, Show, Data, Typeable)

$(deriveSafeCopy 0 'base ''PermaStore)

initialPerma :: PermaStore
initialPerma = PermaStore M.empty

wrap :: (M.Map String String -> M.Map String String) ->
        PermaStore -> PermaStore
wrap f = PermaStore . f . permaStore

-- First string is the store name, second is the content
writeStore :: String -> String -> PermaStore -> PermaStore
writeStore = ((wrap .) .) M.insert

appendStore :: String -> String -> PermaStore -> PermaStore
appendStore k a = wrap $ M.insertWith appendWithNl k a
    where appendWithNl x y = x ++ y ++ "/n"

getStore :: String -> PermaStore -> String
getStore k = M.findWithDefault [] k . permaStore

acidWrite :: String -> String -> Update PermaStore ()
acidWrite k = modify . writeStore k

acidAppend :: String -> String -> Update PermaStore ()
acidAppend k = modify . appendStore k

acidGet :: String -> Query PermaStore String
acidGet k = getStore k `liftM` ask

$(makeAcidic ''PermaStore ['acidWrite, 'acidAppend, 'acidGet]) 

instance MonadAction (StateT Bot IO) where
    send s = messChan <$> get >>= \chan ->
             liftIO $ writeList2Chan chan (T.lines . T.pack $ s)
    printLn = liftIO . putStrLn
    login cKey chall = do
      name <- getName
      pass <- getPass
      assertion <- lift $ getAssertion name pass cKey chall
      command ("/trn " ++ name ++ ",0," ++ assertion)
    putVar k x = modify (\s -> s {vars = M.insert k x $ vars s})
    getVar k = M.lookup k <$> getVars
    duraGet k = acidState <$> get >>= \as ->
                liftIO $ query' as (AcidGet k)
    duraStore k a = acidState <$> get >>= \as ->
                    liftIO $ update' as (AcidWrite k a)
    duraAppend k a = acidState <$> get >>= \as ->
                     liftIO $ update' as (AcidAppend k a)
    getRooms = (rooms . bConfig) `liftM` get
    setJoinTime s t = get >>= \b ->
                      put . setTimestamps b . M.insert s t $ timestamps b
        where setTimestamps b ts = b { timestamps = ts }
    getJoinTime s = M.findWithDefault 0 s . timestamps <$> get
