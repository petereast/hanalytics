{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
  ( someFunc
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

eventToStreamPairs :: AnalyticsEvent -> [(C.ByteString, C.ByteString)]
eventToStreamPairs event =
  [ ("action", C.pack $ action event)
  , ("category", C.pack $ category event)
  , ("label", C.pack $ label event)
  , ("resource", C.pack $ resource event)
  , ("session_id", C.pack $ session_id event)
  ]

someFunc :: IO ()
someFunc = do
  redisConnection <- checkedConnect defaultConnectInfo
  pushEventToStream
    redisConnection
    AnalyticsEvent
      { action = "Some action"
      , category = "some category"
      , resource = "some resource"
      , label = "some label"
      , session_id = "session_id"
      }
  pushEventToStream
    redisConnection
    AnalyticsEvent
      { action = "Some other action"
      , category = "some other category"
      , resource = "some other resource"
      , label = "some other label"
      , session_id = "another_session_id"
      }

-- TODO: For optimisation, all of these should be done
-- in one RunRedis context (per thread?). This is a naieve solution
pushEventToStream :: Connection -> AnalyticsEvent -> IO ()
pushEventToStream redisConnection event = do
  print $ encode event
  runRedis redisConnection $ xadd "events" "*" $ eventToStreamPairs event
  return ()
