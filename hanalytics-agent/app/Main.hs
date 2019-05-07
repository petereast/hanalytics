{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Database.Redis hiding (decode)
import Lib (AnalyticsEvent, pushEventToStream)
import Snap.Core (Snap, ifTop, readRequestBody, route, writeBS)
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = do
  redisConnection <- checkedConnect redisConnectionInfo
  quickHttpServe $ webServer redisConnection
  where
    redisConnectionInfo :: ConnectInfo
    redisConnectionInfo = defaultConnectInfo {connectHost = "redis"}

-- TODO: Investigate use of websockets
webServer :: Connection -> Snap ()
webServer redisConnection =
  ifTop (writeBS "System is healthy!") <|>
  route [("capture", analyticsEndpoint redisConnection)]

analyticsEndpoint :: Connection -> Snap ()
analyticsEndpoint redisConnection = do
  req <- processBody <$> readRequestBody 2048
  generateResponse req
  where
    generateResponse Nothing = writeBS "NOT OK"
    generateResponse (Just event)
      -- TODO: Validate that the strings don't contain colons or other stuff that
      -- could fucc
     = do
      liftIO $ pushEventToStream redisConnection event
      writeBS "OK"

processBody :: L.ByteString -> Maybe AnalyticsEvent
processBody = decode
