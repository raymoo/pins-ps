module Paskell.Handle.Parse ( Message(..), parseMessage ) where

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
dLex = maybeRead <$> mLex >>= check
    where check Nothing = unexpected "non-number"
          check (Just a) = return a

challStr :: Parser Message
challStr = do
  cKey <- dLex
  chall <- mLex
  return (ChallStr (fromIntegral cKey) chall)

baseStr :: Parser Message
baseStr = many anyChar >>= return . Base

parseMessage :: String -> Message
parseMessage s = case parse message "message parsing" s of
                 Right a -> a
                 Left _ -> Unknown
