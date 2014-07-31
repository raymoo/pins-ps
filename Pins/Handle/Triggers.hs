module Pins.Handle.Triggers where

import Pins.Handle.Triggers.Imports

data MessageInfo = MessageInfo { mType  :: String                   -- What was it - chat, pm, join message, etc?
                               , what    :: String                  -- The content
                               , who     :: String                  -- The user
                               , rank    :: Char                    -- The user's rank
                               , room    :: String                  -- The room the message was in
                               , respond :: String -> Action        -- Shortcut for sending message to room
                               , inputs  :: [(String, InputResult)] -- Result of any inputs
                               }

type Test = (MessageInfo -> Bool)
type Act = (MessageInfo -> [Action])
type Key = String

data Trigger = Trigger [(String, Input)] -- Any inputs the trigger needs, with names
                       Test              -- The checking function
                       Act               -- The acting function

-- Utility Functions: Common Tests
contentIs :: String -> Test
contentIs s = (s==) . what

startsWith :: String -> Test
startsWith s = and . zipWith (==) s . what

-- Utility Functions: Common Actions
say :: String -> MessageInfo -> Action
say s = ($ s) . respond

sayOnly :: String -> Act
sayOnly s = single . say s

-- Utility Functions: Etc
maybeString :: Key -> MessageInfo -> Maybe String
maybeString k mi = case couldBeString of
                     Just (InputString s) -> Just s
                     _                    -> Nothing
    where couldBeString = lookup k (inputs mi)

single :: a -> [a]
single x = [x]

-- Test trigger: Tests current basic functionality
testCheck :: Trigger
testCheck = Trigger [("testString", constant "basic input system works")]
                    (contentIs "!test")
                    (\mi -> sayOnly
                           (maybe "not this" ("The test string is: "++) . maybeString "testString" $ mi) 
                           mi)
