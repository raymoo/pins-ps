module Pins.Handle ( handle ) where

import Pins.Handle.Parse
import Pins.Handle.MonadAction
import Pins.Handle.Triggers
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
handle = makeAction . parseMessage

makeAction :: MonadAction m => Message -> m ()
makeAction (ChallStr ckey chall) = printLn "Received Challenge" >>
                                   login ckey chall >>
                                   printLn "Sending response" >>
                                   command "/join yuyukofanclub"
makeAction m = maybe (printLn $ "Unhandled Message: " ++ show m) (`passTriggers` triggerList) (makeMInfo m)

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing

takeRank = fromMaybe ' ' . safeHead

makeMInfo :: Message -> Maybe MessageInfo
makeMInfo (Chat r u w) = Just defaultMInfo { mType = "c"
                                           , what = w
                                           , who  = drop 1 u
                                           , rank = takeRank u
                                           , room = r
                                           , respond = sendChat r
                                           }
makeMInfo (Pm u w)     = Just defaultMInfo { mType = "pm"
                                           , what = w
                                           , who = drop 1 u
                                           , rank = takeRank u
                                           , respond = sendPm u
                                           }
makeMInfo _            = Nothing

