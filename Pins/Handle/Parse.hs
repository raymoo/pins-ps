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
             | Base String
               deriving Show

inMessage :: Parser String
inMessage = many (noneOf "|")

message :: Parser Message
message = try (room >>= roomParse) <|> try (mLex >>= chooseBlankParse) <|> baseStr

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

roomParse :: String -> Parser Message
roomParse r = (mLex >>= chooseRoomParse r)

chooseRoomParse :: String -> String -> Parser Message
chooseRoomParse r "c" = liftM2 (Chat r . drop 1) mLex mLex
chooseRoomParse _ _   = return Unknown

chooseBlankParse :: String -> Parser Message
chooseBlankParse "challstr" = try challStr <|> return Unknown
chooseBlankParse x = return Unknown

getArgs :: Parser [String]
getArgs = many mLex

mLex :: Parser String
mLex = char '|' >> inMessage

dLex :: Parser Int
dLex = liftM maybeRead mLex >>= check
    where check Nothing = unexpected "non-number"
          check (Just a) = return a

room :: Parser String
room = char '>' *> many (noneOf "\n") <* char '\n'

challStr :: Parser Message
challStr = liftM2 ChallStr dLex mLex

baseStr :: Parser Message
baseStr = liftM Base (many anyChar)

parseMessage :: String -> Message
parseMessage s = case parse message "message parsing" s of
                 Right a -> a
                 Left _ -> Unknown
