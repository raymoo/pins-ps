module Pins.Handle.MonadAction where

type Room = String
type User = String
type Pass = String

class Monad m => MonadAction m where
    send :: String -> m ()
    login :: Int -> String -> m ()
    printLn :: String -> m () 

constant :: MonadAction m => a -> m a
constant = return

sendChat :: MonadAction m => Room -> String -> m ()
sendChat r m = send chatMessage
    where chatMessage = r ++ "|" ++ m

sendPm :: MonadAction m => User -> String -> m ()
sendPm u m = command ("/pm " ++ u ++ ',' : m)

command :: MonadAction m => String -> m ()
command = send . ('|' :)

put :: MonadAction m => String -> m ()
put = printLn
