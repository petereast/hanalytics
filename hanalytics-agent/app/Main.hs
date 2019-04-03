{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Database.Redis (Connection, checkedConnect, defaultConnectInfo)
import Lib (AnalyticsEvent, pushEventToStream)
import Snap.Core (Snap, ifTop, readRequestBody, route, writeBS)
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = do
  redisConnection <- checkedConnect defaultConnectInfo
  quickHttpServe $ webServer redisConnection

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
    generateResponse (Just event) = do
      liftIO $ pushEventToStream redisConnection event
      writeBS "OK"

processBody :: L.ByteString -> Maybe AnalyticsEvent
processBody = decode
