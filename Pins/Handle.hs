module Pins.Handle ( Action(..), handle ) where

import Pins.Handle.Parse
import Pins.Handle.Actions.Base

handle :: String -> [Action]
handle = makeAction . parseMessage

makeAction :: Message -> [Action]
makeAction (ChallStr ckey chall) = [ Print "Received Challenge"
                                   , Login ckey chall
                                   , Print "Sending response"
                                   , Send "|/join techcode"
                                   ]
makeAction m = [Print "Unhandled Message"]
