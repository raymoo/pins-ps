module Pins.Handle.Triggers where

import Pins.Handle.Triggers.Imports

data MessageInfo = MessageInfo { mType  :: String        -- What was it - chat, pm, join message, etc?
                               , what   :: String        -- The content
                               , who    :: String        -- The user
                               , rank   :: Char          -- The user's rank
                               , room   :: String        -- The room the message was in
                               , inputs :: [InputResult] -- Result of any inputs
                               }

data Trigger = Trigger [Input]                   -- Any inputs the trigger needs
                       (MessageInfo -> Bool)     -- The checking function
                       (MessageInfo -> [Action]) -- The acting function


