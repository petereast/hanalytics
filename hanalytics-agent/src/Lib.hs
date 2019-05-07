{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
  ( AnalyticsEvent
  , pushEventToStream
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Time
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

-- NOTE: I thought the return type of this should be IO
addTime :: [(C.ByteString, C.ByteString)] -> IO [(C.ByteString, C.ByteString)]
addTime withoutTime = do
  time <- show <$> getCurrentTime
  return $ withoutTime ++ [(C.pack "time", C.pack time)]

-- TODO: For optimisation, all of these should be done
-- in one RunRedis context (per thread?). This is a naieve solution
-- TODO: Error handling?
pushEventToStream :: Connection -> AnalyticsEvent -> IO ()
pushEventToStream redisConnection event = do
  outgoing_event <- (addTime . eventToStreamPairs) event
  runRedis redisConnection $ do
    xadd "events" "*" outgoing_event
    incr "events_published"
  return ()
