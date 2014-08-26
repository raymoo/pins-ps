{-# LANGUAGE FlexibleInstances #-}

module Pins.Handle.MonadAction where

import Data.Maybe
import Control.Monad

type Room = String
type User = String
type Pass = String

data Var = VarChar Char
         | VarArray [Var]
         | VarPair (String, Var)

class Variable a where
    pack   :: a -> Var
    unpack :: Var -> Maybe a

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (x,y) = (x,f y)

instance Variable a => Variable (String, a) where
    pack = VarPair . mapSnd pack
    unpack (VarPair x) = (unpack . snd $ x) >>= Just . (,) (fst x)
    unpack _           = Nothing

instance Variable Char where
    pack = VarChar
    unpack (VarChar s) = Just s
    unpack _           = Nothing

instance Variable a => Variable [a] where
    pack = VarArray . map pack
    unpack (VarArray xs) = Just . catMaybes $ map unpack xs
    unpack _             = Nothing

class Monad m => MonadAction m where
    send :: String -> m ()                 -- send a message to the server
    login :: Int -> String -> m ()         -- login with a challenge key id and a challenge
    printLn :: String -> m ()              -- print a line to standard output
    putVar :: String -> Var -> m ()        -- put a variable
    getVar :: String -> m (Maybe Var)      -- possibly get a variable
    duraGet :: String -> m String          -- get a string from durable storage (similar to file I/O)
    duraStore :: String -> String -> m ()  -- put a string in durable storage
    duraAppend :: String -> String -> m () -- append a string to a string in durable storage, adding a newline

constant :: MonadAction m => a -> m a
constant = return

sendChat :: MonadAction m => Room -> String -> m ()
sendChat r m = send chatMessage
    where chatMessage = unlines . map ((r ++ "|")++) $ lines m

sendPm :: MonadAction m => User -> String -> m ()
sendPm u m = send pmMessage
    where pmMessage = unlines . map (("|/pm " ++ u ++ ",") ++) . lines $ m

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
