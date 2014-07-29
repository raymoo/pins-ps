module Pins.Bot.Login (getAssertion) where

import Text.JSON hiding (Result)
import Data.Maybe
import Data.List
import Network.HTTP
import Network.Stream
import Control.Applicative ((<$>))

rServer = "http://play.pokemonshowdown.com/action.php"

getAssertion :: String -> String -> Int -> String -> IO String
getAssertion name pass cKey chall = do
  response <- simpleHTTP $ postRequestWithBody rServer "application/x-www-form-urlencoded" requestString
  unpacked <- getResponseBody response
  return $ aFromResponse (drop 1 unpacked)
      where requestString = getRequestString [ ("act", "login")
                                             , ("name", name)
                                             , ("pass", pass)
                                             , ("challengekeyid", show cKey)
                                             , ("challenge", chall) ]

aFromResponse :: String -> String
aFromResponse = getA . decode
    where getA (Ok (JSObject o)) = fromMaybe "" . fmap valueToString . lookup "assertion"  . fromJSObject $ o
          getA  (Error _) = ""

valueToString :: JSValue -> String
valueToString js = case result of
                  Ok s -> s
                  Error _ -> ""
    where result = readJSON js

getRequestString :: [(String, String)] -> String
getRequestString = intercalate "&" . map (\(x,y) -> x ++ "=" ++ y)
