module Pins.Handle.Actions where

import Pins.Handle.Actions.Base

type Room = String
type User = String

sendChat :: Room -> String -> Action
sendChat r m = Send chatMessage
    where chatMessage = r ++ "|" ++ m

sendPm :: User -> String -> Action
sendPm u m = command ("/pm " ++ u ++ ',' : m)

command :: String -> Action
command c = Send ('|' : c)

put :: String -> Action
put = Print

