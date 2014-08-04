{-# LANGUAGE Rank2Types #-}

module Pins.Handle.Triggers where

import           Pins.Handle.Triggers.Imports
import           Control.Monad
import           Data.Char
import           Data.Maybe
import qualified Data.List.Split as LS

data MessageInfo = MessageInfo { mType   :: String   -- What was it - chat, pm, join message, etc?
                               , what    :: String   -- The content
                               , who     :: String   -- The user
                               , rank    :: Char     -- The user's rank
                               , room    :: String   -- The room the message was in
                               , respond :: Response -- Shortcut for sending message to room
                               }

type Response = MonadAction m => String -> m ()
type Test = MessageInfo -> Bool
type Act = MonadAction m => MessageInfo -> m ()
type Key = String

data Trigger = Trigger { test ::Test               -- The checking function
                       , act :: Act                -- The acting function
                       }

-- Add any triggers you want used here
triggerList :: [Trigger]
triggerList = [ testCheck
              , anonMessage
              , about
              , sokuHost
              , sokuHosting
              , sokuUnhost
              ]

-- Utility Functions: Common Tests
contentIs :: String -> Test
contentIs s = (s==) . what

typeIs :: String -> Test
typeIs s = (s==) . mType

startsWith :: String -> Test
startsWith s = and . zipWith (==) s . what

combine :: [Test] -> Test
combine ts = and . flip map ts . flip ($)

anyTest :: [Test] -> Test
anyTest ts = or . flip map ts . flip ($)

(<&&>) :: Test -> Test -> Test
t1 <&&> t2 = combine [t1,t2]

(<||>) :: Test -> Test -> Test
t1 <||> t2 = anyTest [t1,t2]

-- Utility Functions: Common Actions
say :: String -> Act
say s = ($ s) . respond

-- Utility Functions: Etc
single :: a -> [a]
single x = [x]

condenseNick :: String -> String
condenseNick = map toLower . filter isAlphaNum

dropLeadSpaces :: String -> String
dropLeadSpaces = dropWhile (' '==)

getArgs :: String -> [String]
getArgs = map dropLeadSpaces . LS.splitOn ","

aListSet :: Eq k => k -> a -> [(k,a)] -> [(k,a)] -- Very inefficient (O(n)) modification for associated lists.
aListSet k x = ((k, x) :) . aListDel k

aListDel :: Eq k => k -> [(k,a)] -> [(k,a)] -- O(n) deletion function
aListDel k = filter ((k/=) . fst)

-- Test trigger: Tests current basic functionality
testCheck :: Trigger
testCheck = Trigger (contentIs "!test")
                    (\x -> constant "Working" >>= flip say x)

-- Anonymous message trigger: use !mess destination, message to send a message
anonMessage :: Trigger
anonMessage = Trigger (combine [startsWith "!mess", typeIs "pm"])
                      sendAnonMessage

sendAnonMessage :: Act
sendAnonMessage mi = anonMessMake . getArgs . drop 6 $ what mi
    where anonMessMake ss
              | length ss >= 2 = case ss !! 0 of
                                   ('#':xs) -> sendChat (drop 1 (ss !! 0)) ("Anonymous Message: " ++ ss !! 1)
                                   _        -> sendPm (ss !! 0) ("Anonymous Message: " ++ ss !! 1) >>
                                               printLn ("Sending pm to " ++ ss !! 0)
              | otherwise      = anonErrorMessage
          anonErrorMessage = respond mi "Usage is: !mess [#]destination, message" >>
                             respond mi "The # is only necessary for sending to rooms"

-- About trigger: Displays bot info
about :: Trigger
about = Trigger (contentIs "!about")
                (say "I am a bot written by Reimu in the functional language Haskell (http://www.haskell.org). Repo: https://github.com/raymoo/pins-ps")

-- Host trigger: Record hosting info
sokuHost :: Trigger
sokuHost = Trigger (combine [ startsWith "!host "
                            , typeIs "c"
                            ]
                   )
                   recHost

recHost :: Act
recHost mi = case drop 6 . what $ mi of
               [] -> respond mi "You need to specify hosting info"
               s  -> putBack `liftM` varGet "soku" >>=
                     varPut "soku" >>
                     respond mi (who mi ++ " is hosting at " ++ s)
                   where putBack = aListSet (condenseNick . who $ mi) s . fromMaybe []

-- Hosting Trigger: Get hosting info
sokuHosting :: Trigger
sokuHosting = Trigger (contentIs "!hosting" <&&> (typeIs "c" <||> typeIs "pm"))
                      getHosting

getHosting :: Act
getHosting mi = hostList >>= sendPm (who mi)
    where hostList = (generateList . fromMaybe []) `liftM` varGet "soku"

generateList :: [(String, String)] -> String
generateList [] = "Nobody is hosting."
generateList xs =  unlines $ map (\(x,y) -> x ++ " is hosting at " ++ y) xs

-- Unhost Trigger: Stop hosting
sokuUnhost :: Trigger
sokuUnhost = Trigger (contentIs "!unhost" <&&> typeIs "c")
                     stopHosting

stopHosting :: Act
stopHosting mi = theVar >>= removeHost
    where theVar :: (MonadAction m) => m [(String, String)]
          theVar = fromMaybe [] `liftM` varGet "soku"
          removeHost :: (MonadAction m) => [(String, String)] -> m ()
          removeHost xs = case lookup (condenseNick . who $ mi) xs of
                            Just x  -> varMod "soku" (aListDel (condenseNick . who $ mi) :: [(String,String)] -> [(String,String)]) >>
                                       respond mi (who mi ++ " is no longer hosting.")
                            Nothing -> respond mi "You are not hosting!"
