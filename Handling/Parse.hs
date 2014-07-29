module Parse where

import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec

data Message = Unknown
             | ChallStr {cKeyID :: String, challenge :: String}
             | Base String
               deriving Show

message :: Parser Message
message = (mLex >>= chooseParse) <|> baseStr

chooseParse :: String -> Parser Message
chooseParse "challstr" = challStr
chooseParse x = return Unknown

getArgs :: Parser [String]
getArgs = many mLex

mLex :: Parser String
mLex = char '|' >> many (noneOf "|")

challStr :: Parser Message
challStr = do
  cKey <- mLex
  chall <- mLex
  return (ChallStr cKey chall)

baseStr :: Parser Message
baseStr = many anyChar >>= return . Base

parseMessage = parse message "message parsing"
