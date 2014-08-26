{-# LANGUAGE Rank2Types #-}

module Pins.Handle.Triggers where

import           Pins.Handle.Triggers.Imports
import           Control.Monad
import           Data.Char
import           Data.Maybe
import qualified Data.Foldable   as F
import qualified Data.List       as L
import qualified Data.List.Split as LS

data MessageInfo = MessageInfo { mType   :: String   -- What was it - chat, pm, join message, etc?
                               , what    :: String   -- The content
                               , who     :: String   -- The user
                               , rank    :: Char     -- The user's rank
                               , room    :: String   -- The room the message was in
                               , respond :: Response -- Shortcut for sending a response to the message
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
              , kickHost
              , topic
              , testDur
              , testChan
              ]

-- Utility Functions: Common Tests
contentIs :: String -> Test
contentIs s = (s==) . what

rankIn :: String -> Test
rankIn s = flip elem s . rank

typeIs :: String -> Test
typeIs s = (s==) . mType

startsWith :: String -> Test
startsWith s =  (s==) . take l . what
    where l = length s

combine :: [Test] -> Test
combine ts = and . flip map ts . flip ($)

anyTest :: [Test] -> Test
anyTest ts = or . flip map ts . flip ($)

(<&&>) :: Test -> Test -> Test
(<&&>) t1 t2 mi = t1 mi && t2 mi

(<||>) :: Test -> Test -> Test
(<||>) t1 t2 mi = t1 mi || t2 mi

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

getSomeArgs ::Int ->  String -> ([String], String)
getSomeArgs n s = let ss = LS.splitOn "," s
                  in (map dropLeadSpaces $ take n ss, L.intercalate "," . drop n $ ss)

aListSet :: Eq k => k -> a -> [(k,a)] -> [(k,a)] -- Very inefficient (O(n)) modification for associated lists.
aListSet k x = ((k, x) :) . aListDel k

aListSetVar :: (Variable a, MonadAction m) => Key -> Key -> a -> m ()
aListSetVar k k' x = varGet k >>=
                     varPut k . aListSet k' x . fromMaybe []

aListDel :: Eq k => k -> [(k,a)] -> [(k,a)] -- O(n) deletion function
aListDel k = filter ((k/=) . fst)

aListDelVarString :: MonadAction m => Key -> Key -> m ()
aListDelVarString k k' = (varGet k :: MonadAction m => m (Maybe [(String, String)])) >>=
                         (`F.forM_` (varPut k . aListDel k'))

-- Test trigger: Tests current basic functionality
testCheck :: Trigger
testCheck = Trigger (contentIs "!test")
                    (\x -> constant "Working" >>= flip say x)

-- Anonymous message trigger: use !mess destination, message to send a message
anonMessage :: Trigger
anonMessage = Trigger (startsWith "!mess" <&&> typeIs "pm")
                      sendAnonMessage

sendAnonMessage :: Act
sendAnonMessage mi = anonMessMake . args $ mi
    where anonMessMake (ss, s)
              | not . null $ ss = case ss !! 0 of
                                   ('#':xs) -> sendChat (drop 1 (ss !! 0)) ("Anonymous Message: " ++ s)
                                   _        -> sendPm (ss !! 0) ("Anonymous Message: " ++ s) >>
                                               printLn ("Sending pm to " ++ ss !! 0)
              | otherwise      = anonErrorMessage
          anonErrorMessage = respond mi "Usage is: !mess [#]destination, message" >>
                             respond mi "The # is only necessary for sending to rooms"
          args = getSomeArgs 1 . drop 6 . what

-- About trigger: Displays bot info
about :: Trigger
about = Trigger (contentIs "!about")
                (say "I am a bot written by Reimu in the functional language Haskell (http://www.haskell.org). Repo: https://github.com/raymoo/pins-ps")

-- Host trigger: Record hosting info
sokuHost :: Trigger
sokuHost = Trigger (startsWith "!host " <&&> typeIs "c")
                   recHost

recHost :: Act
recHost mi = case drop 6 . what $ mi of
               [] -> recHostOnly mi
               s  -> aListSetVar "soku" (condenseNick . who $ mi) s >>
                     respond mi (who mi ++ " is hosting at " ++ s) >>
                     duraStore ("soku_" ++ condenseNick (who mi)) s

recHostOnly :: Act
recHostOnly mi = duraGet ("soku_" ++ condenseNick (who mi)) >>= \s ->
                 case s of
                   [] -> respond mi "You must have explicitly given an address:port at least once for me to remember it."
                   s -> aListSetVar "soku" (condenseNick . who $ mi) s >>
                        respond mi (who mi ++ " is hosting at " ++ s)

-- Hosting Trigger: Get hosting info
sokuHosting :: Trigger
sokuHosting = Trigger (contentIs "!hosting" <&&> (typeIs "c" <||> typeIs "pm"))
                      getHosting

getHosting :: Act
getHosting mi = sendPm (who mi) "Hosts:" >>
                hostList >>= sendPm (who mi)
    where hostList = (generateList . fromMaybe []) `liftM` varGet "soku"

generateList :: [(String, String)] -> String
generateList [] = "Nobody is hosting."
generateList xs =  unlines $ map (\(x,y) -> x ++ " is hosting at " ++ y) xs

-- Unhost Trigger: Stop hosting
sokuUnhost :: Trigger
sokuUnhost = Trigger (contentIs "!unhost" <&&> typeIs "c")
                     stopHosting

stopHosting :: Act
stopHosting mi = (removeHost . who $ mi) >>= \result ->
                 case result of
                   True  -> respond mi (who mi ++ " is no longer hosting")
                   False -> respond mi "You are not hosting"

removeHost :: (MonadAction m) => String -> m Bool
removeHost s = sokuVar >>= \var ->
               case lookup (condenseNick s) var of
                 Just x  -> aListDelVarString "soku" (condenseNick s) >>
                            return True
                 Nothing -> return False
    where sokuVar = fromMaybe [] `liftM` varGet "soku" :: (MonadAction m) => m [(String, String)]

-- Kick Host: Kick a host
kickHost :: Trigger
kickHost = Trigger (startsWith "!kickhost " <&&> typeIs "c" <&&> rankIn "%@#&~")
                   kickAHost

kickAHost :: Act
kickAHost mi = case drop 10 . what $ mi of
                 []   -> respond mi "Specify a user to kick"
                 user -> removeHost user >>= \res ->
                         case res of
                           True  -> respond mi "User removed."
                           False -> respond mi $ "User " ++ user ++ " is not hosting."
                                   

-- Topic Trigger: Can set or get the topic
topic :: Trigger
topic = Trigger (startsWith "!topic" <&&> typeIs "c" <&&> rankIn "%@#&~")
                doTopic

doTopic :: Act
doTopic mi = let rem = (drop 7 . what $ mi)
             in case rem of
                  [] -> varGet k >>=    -- They want the topic
                        sendChat (room mi) . ("/wall Topic: "++) . fromMaybe "Nothing"
                  s  -> varPut k rem >> --They are setting the topic
                        sendChat (room mi) ("/wall Topic: " ++ rem)
                 where k = "topic_" ++ room mi

-- Test Durable storage
testDur :: Trigger
testDur = Trigger (startsWith "!testDur")
                  testTheDur

testTheDur :: Act
testTheDur mi = respond mi "Storing your test" >>
                duraStore "test" (what mi) >>
                duraGet "test" >>= \ts ->
                respond mi ("Retrieved: " ++ ts)

-- Test Channel: Tests if the queue works
testChan :: Trigger
testChan = Trigger (contentIs "!testChan" <&&> typeIs "pm")
                   (\mi -> respond mi "This is a message\nthat should be\nfiltered")
