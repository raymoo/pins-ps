module Pins.Handle.Parse ( Message(..), parseMessage ) where

import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Data.Maybe

type User = String
type Room = String
type What = String

data Message = Unknown
             | ChallStr Int String
             | Chat Room User What
             | Pm User What
             | Base String
               deriving Show

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- Basic Parsers
mLex :: Parser String
mLex = char '|' *> inMessage

dLex :: Parser Int
dLex = maybeRead <$> mLex >>= check
    where check Nothing = unexpected "non-number"
          check (Just a) = return a

contLex :: Parser String
contLex = char '|' *> many (noneOf "\n")

room :: Parser String
room = char '>' *> many (noneOf "\n")

inMessage :: Parser String
inMessage = many (noneOf "|\n")

getArgs :: Parser [String]
getArgs = many mLex

-- Main message parser
parseMessage :: String -> [Message]
parseMessage s = case parse messages "message parsing" s of
                 Right a -> a
                 Left _ -> [Unknown]

messages :: Parser [Message]
messages =  room >>= \r ->
            many (char '\n' *> message r) <|>
            liftA2 (:) (message "") (many (char '\n' *> message ""))

message :: String -> Parser Message
message r = try (chooseParse r) <|> try (chooseParse r) <|> baseStr

-- Deciders
chooseParse :: String -> Parser Message
chooseParse r = mLex >>= chooseMore
    where chooseMore "c:"       = try (mLex *> chat r) <|> return Unknown
          chooseMore "challstr" = try challStr <|> return Unknown
          chooseMore "pm"       = try pm <|> return Unknown
          chooseMore _          = return Unknown

-- Message type specific parsers
pm :: Parser Message
pm = Pm <$> mLex <*> (mLex *> contLex)

chat :: String -> Parser Message
chat r = Chat r <$> mLex <*> contLex

challStr :: Parser Message
challStr = ChallStr <$>  dLex <*> mLex

baseStr :: Parser Message
baseStr = Base <$> many anyChar
