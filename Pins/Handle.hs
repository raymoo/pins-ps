module Pins.Handle ( Action(..), handle ) where

import Pins.Handle.Parse
import Pins.Handle.Actions.Base
import Pins.Handle.Actions
import Pins.Handle.Triggers
import Pins.Bot.Inputs

import Data.Maybe

passTriggers :: MessageInfo -> [Trigger] -> [Action]
passTriggers mi = concatMap (doTrigger mi) . filter (checkTrigger mi)

checkTrigger :: MessageInfo -> Trigger -> Bool
checkTrigger mi t = test t mi

doTrigger :: MessageInfo -> Trigger -> [Action]
doTrigger mi t = act t (mi { inputs = getInputResults (tInputs t) })

defaultMInfo :: MessageInfo
defaultMInfo = MessageInfo "idleThoughts"
                           ""
                           ""
                           ' '
                           ""
                           Print



                           []

handle :: String -> [Action]
handle = makeAction . parseMessage

makeAction :: Message -> [Action]
makeAction (ChallStr ckey chall) = [ Print "Received Challenge"
                                   , Login ckey chall
                                   , Print "Sending response"
                                   , Send "|/join yuyukofanclub"
                                   ]
makeAction m = maybe [Print ("Unhandled Message: " ++ show m)] (`passTriggers` triggerList) (makeMInfo m)

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

getInputResults :: [(String, Input)] -> [(String, InputResult)]
getInputResults xs = zip (map fst xs) (map (getInputResult . snd) xs)

getInputResult :: Input -> InputResult --Placeholder Function
getInputResult (ConstantString s) = InputString s
