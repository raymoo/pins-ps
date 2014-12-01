module Pins.Handle ( handle ) where

import Pins.Handle.Parse
import Pins.Handle.MonadAction
import Pins.Handle.Triggers
import Control.Monad (liftM)
import Control.Applicative ((<$>))
import Data.Maybe

passTriggers :: MonadAction m => MessageInfo -> [Trigger] -> m ()
passTriggers mi = mapM_ (doTrigger mi) . filter (checkTrigger mi)

checkTrigger :: MessageInfo -> Trigger -> Bool
checkTrigger mi t = test t mi

doTrigger :: MonadAction m => MessageInfo -> Trigger -> m ()
doTrigger mi t = act t mi

defaultMInfo :: MessageInfo
defaultMInfo = MessageInfo "idleThoughts"
                           ""
                           ""
                           ' '
                           ""
                           printLn

handle :: MonadAction m => String -> m ()
handle = makeActions . parseMessage

makeActions :: MonadAction m => [Message] -> m ()
makeActions = mapM_ makeAction

makeAction :: MonadAction m => Message -> m ()
makeAction (ChallStr ckey chall) = printLn "Received Challenge" >>
                                   login ckey chall >>
                                   printLn "Sending response" >>
                                   getRooms >>= mapM_ join
makeAction (Time r t)            = setJoinTime r t
makeAction m@(Chat r t _ _)      = (t >) `liftM` getJoinTime r >>= \new ->
                                   if new
                                   then handleAny m
                                   else return ()
makeAction m                     = handleAny m

handleAny :: MonadAction m => Message -> m ()
handleAny m = maybe (printLn $ "Unhandled Message: " ++ show m)
                    (`passTriggers` triggerList)
                    (makeMInfo m)

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing

takeRank = fromMaybe ' ' . safeHead

makeMInfo :: Message -> Maybe MessageInfo
makeMInfo (Chat r _ u w) = Just defaultMInfo { mType = "c"
                                             , what = w
                                             , who  = drop 1 u
                                             , rank = takeRank u
                                             , room = r
                                             , respond = sendChat r
                                             }
makeMInfo (Pm u w)       = Just defaultMInfo { mType = "pm"
                                             , what = w
                                             , who = drop 1 u
                                             , rank = takeRank u
                                             , respond = sendPm u
                                             }
makeMInfo (Raw r s)      = Just defaultMInfo { mType = "raw"
                                             , what  = s
                                             , room  = r
                                             , respond = command
                                             } 
makeMInfo _              = Nothing

