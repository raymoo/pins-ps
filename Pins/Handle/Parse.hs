module Pins.Handle.Parse ( Message(..), parseMessage ) where

import Control.Monad
import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Data.Maybe

data Message = Unknown
             | ChallStr {cKeyID :: Int, challenge :: String}
             | Base String
               deriving Show

message :: Parser Message
message = try (mLex >>= chooseParse) <|> baseStr

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

chooseParse :: String -> Parser Message
chooseParse "challstr" = try challStr <|> return Unknown
chooseParse x = return Unknown

getArgs :: Parser [String]
getArgs = many mLex

mLex :: Parser String
mLex = char '|' >> many (noneOf "|")

dLex :: Parser Int
dLex = liftM maybeRead mLex >>= check
    where check Nothing = unexpected "non-number"
          check (Just a) = return a

challStr :: Parser Message
challStr = liftM2 ChallStr dLex mLex

baseStr :: Parser Message
baseStr = liftM Base (many anyChar)

parseMessage :: String -> Message
parseMessage s = case parse message "message parsing" s of
                 Right a -> a
                 Left _ -> Unknown
