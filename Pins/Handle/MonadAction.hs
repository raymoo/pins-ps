{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}

module Pins.Handle.MonadAction where

import Data.Maybe
import Control.Monad

type Room = String
type User = String
type Pass = String

data Var = VarString String
         | VarArray [Var]

class Variable a where
    pack   :: a -> Var
    unpack :: Var -> Maybe a

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

getString :: MonadAction m => String -> m (Maybe String)
getString = liftM unpackCheck . getVar
    where unpackCheck (Just x) = unpack x
          unpackCheck _      = Nothing

getStringList :: MonadAction m => String -> m (Maybe [String])
getStringList = liftM unpackCheck . getVar
    where unpackCheck (Just x) = unpack x
          unpackCheck _        = Nothing

varPut :: (MonadAction m, Variable a) => String -> a -> m ()
varPut k = putVar k . pack
