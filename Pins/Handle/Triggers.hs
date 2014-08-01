module Pins.Handle.Triggers where

import Pins.Handle.Triggers.Imports
import Data.Char
import qualified Data.List.Split as LS

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

data Trigger = Trigger { tInputs :: [(String, Input)] -- Any inputs the trigger needs, with names
                       , test ::Test                 -- The checking function
                       , act :: Act                  -- The acting function
                       }

triggerList :: [Trigger]
triggerList = [testCheck]

-- Utility Functions: Common Tests
contentIs :: String -> Test
contentIs s = (s==) . what

typeIs :: String -> Test
typeIs s = (s==) . mType

startsWith :: String -> Test
startsWith s = and . zipWith (==) s . what

combine :: [Test] -> Test
combine ts = and . flip map ts . flip ($)

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

condenseNick :: String -> String
condenseNick = map toLower . filter isAlphaNum

dropLeadSpaces :: String -> String
dropLeadSpaces = dropWhile (' '==)

getArgs :: String -> [String]
getArgs = map dropLeadSpaces . LS.splitOn ","

-- Test trigger: Tests current basic functionality
testCheck :: Trigger
testCheck = Trigger [("testString", constant "basic input system works")]
                    (contentIs "!test")
                    (\mi -> sayOnly
                           (maybe "not this" ("The test string is: "++) . maybeString "testString" $ mi) 
                           mi)

-- Anonymous message trigger: use !mess destination, message to send a message
anonMessage :: Trigger
anonMessage = Trigger []
                      (combine [startsWith "!mess", typeIs "pm"])
                      sendAnonMessage

sendAnonMessage :: Act
sendAnonMessage mi = anonMessMake . getArgs . drop 6 $ what mi
    where anonMessMake ss
              | length ss >= 2 = case ss !! 0 of
                                   ('#':xs) -> [sendChat (ss !! 0) ("AnonymousMessage: " ++ ss !! 1)]
                                   _        -> [sendPm (ss !! 0) ("AnonymousMessage: " ++ ss !! 1)]
              | otherwise      = anonErrorMessage
          anonErrorMessage = [ respond mi "Usage is: !mess [#]destination, message"
                             , respond mi "The # is only necessary for sending to rooms"
                             ]               
