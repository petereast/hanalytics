{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Database.Redis (Connection, checkedConnect, defaultConnectInfo)
import Lib
import Snap.Core (Snap, ifTop, route, writeBS)
import Snap.Http.Server (quickHttpServe)

main :: IO ()
main = do
  redisConnection <- checkedConnect defaultConnectInfo
  quickHttpServe $ webServer redisConnection

-- TODO: Define the API for ingesting analytics events here - think of this file as linking
-- http with the streams based backend
webServer :: Connection -> Snap ()
webServer redisConnection =
  ifTop (writeBS "System is healthy!") <|>
  route [("capture", analyticsEndpoint redisConnection)]

analyticsEndpoint :: Connection -> Snap ()
analyticsEndpoint redisConnection = do
  writeBS "{\"ok\":true}"
