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
checkTrigger mi t = (test t) mi

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
                                   , Send "|/join techcode"
                                   ]
makeAction c@(Chat _ _ _) = passTriggers (makeMInfo c) triggerList
makeAction m = [Print ("Unhandled Message: " ++ show m)]

safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead []     = Nothing

makeMInfo :: Message -> MessageInfo
makeMInfo (Chat r u w) = defaultMInfo { mType = "c"
                                 , what = w
                                 , who  = drop 1 u
                                 , rank = fromMaybe ' ' (safeHead u)
                                 , room = r
                                 , respond = sendChat r
                                 }

getInputResults :: [(String, Input)] -> [(String, InputResult)]
getInputResults xs = zip (map fst xs) (map (getInputResult . snd) xs)

getInputResult :: Input -> InputResult --Placeholder Function
getInputResult (ConstantString s) = InputString s
