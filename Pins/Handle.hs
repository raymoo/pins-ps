module Pins.Handle ( Action(..), handle ) where

import Pins.Handle.Parse

data Action = Send String
            | Login Int String
              deriving Show

handle :: String -> [Action]
handle = makeAction . parseMessage

makeAction :: Message -> [Action]
makeAction (ChallStr ckey chall) = [Login ckey chall]
makeAction m = []
