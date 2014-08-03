{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Pins.Handle.MonadAction where

import Data.Maybe
import Control.Monad

type Room = String
type User = String
type Pass = String

data Var = VarString String
         | VarArray [Var]
         | VarAlist [(String, Var)]

class Variable a where
    pack   :: a -> Var
    unpack :: Var -> Maybe a

mapSnds :: (a -> b) -> [(c,a)] -> [(c,b)]
mapSnds f = map (mapSnd f)
    where mapSnd f (x,y) = (x,f y)

removeNothings :: [(a,Maybe b)] -> [(a,b)]
removeNothings (x:xs) = case snd x of
                          Just a  -> (fst x, a) : removeNothings xs
                          Nothing -> removeNothings xs
removeNothings []     = []

instance Variable a => Variable [(String, a)] where
    pack   = VarAlist . mapSnds pack
    unpack (VarAlist as) = Just . removeNothings . mapSnds unpack $ as

instance Variable String where
    pack = VarString
    unpack (VarString s) = Just s
    unpack _             = Nothing

instance Variable a => Variable [a] where
    pack = VarArray . map pack
    unpack (VarArray xs) = Just . catMaybes $ map unpack xs
    unpack _             = Nothing

class Monad m => MonadAction m where
    send :: String -> m ()
    login :: Int -> String -> m ()
    printLn :: String -> m () 
    putVar :: String -> Var -> m ()
    getVar :: String -> m (Maybe Var)

constant :: MonadAction m => a -> m a
constant = return

sendChat :: MonadAction m => Room -> String -> m ()
sendChat r m = send chatMessage
    where chatMessage = r ++ "|" ++ m

sendPm :: MonadAction m => User -> String -> m ()
sendPm u m = command ("/pm " ++ u ++ ',' : m)

command :: MonadAction m => String -> m ()
command = send . ('|' :)

varGet :: (Variable a, MonadAction m) => String -> m (Maybe a)
varGet = liftM unpackCheck . getVar
    where unpackCheck (Just x) = unpack x
          unpackCheck _        = Nothing

varPut :: (MonadAction m, Variable a) => String -> a -> m ()
varPut k = putVar k . pack

varMod :: (MonadAction m, Variable a) => String -> (a -> a) -> m ()
varMod k f = varGet k >>= testVar
    where testVar (Just x) = varPut k . f $ x
          testVar Nothing = return ()
