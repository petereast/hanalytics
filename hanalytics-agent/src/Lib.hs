{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
  ( AnalyticsEvent
  , pushEventToStream
  ) where

import Conduit
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Database.Redis
import GHC.Generics

-- Define functions for building and storing analytics events
data AnalyticsEvent = AnalyticsEvent
  { action :: String
  , category :: String
  , label :: String
  , resource :: String
  , session_id :: String
  } deriving (Generic, Show)

instance ToJSON AnalyticsEvent

instance FromJSON AnalyticsEvent

eventToStreamPairs :: AnalyticsEvent -> [(C.ByteString, C.ByteString)]
eventToStreamPairs event =
  [ ("action", C.pack $ action event)
  , ("category", C.pack $ category event)
  , ("label", C.pack $ label event)
  , ("resource", C.pack $ resource event)
  , ("session_id", C.pack $ session_id event)
  ]

-- TODO: For optimisation, all of these should be done
-- in one RunRedis context (per thread?). This is a naieve solution
-- TODO: Error handling?
pushEventToStream :: Connection -> AnalyticsEvent -> IO ()
pushEventToStream redisConnection event = do
  runRedis redisConnection $ xadd "events" "*" $ eventToStreamPairs event
  return ()
