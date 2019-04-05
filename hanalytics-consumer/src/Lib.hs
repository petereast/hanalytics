{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
  ( someFunc
  , createConsumerGroup
  , AnalyticsEvent
  , payloadFromRedis
  , time
  , action
  , category
  , label
  , resource
  , session_id
  ) where

import Data.Aeson
import qualified Data.ByteString.Char8 as C
import Data.Map.Lazy (Map, (!), fromList, lookup)
import Data.Maybe (fromJust)
import Database.Redis
  ( Connection
  , StreamsRecord
  , XReadResponse
  , incr
  , keyValues
  , lpush
  , recordId
  , records
  , runRedis
  , xack
  , xadd
  , xgroupCreate
  , xreadGroup
  )
import GHC.Generics
import Prelude hiding (lookup)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Function to read a set of events from Redis
data AnalyticsEvent = AnalyticsEvent
  { action :: String
  , category :: String
  , label :: String
  , resource :: String
  , time :: String
  , session_id :: String
  } deriving (Generic, Show)

instance ToJSON AnalyticsEvent

payloadToMap :: [(C.ByteString, C.ByteString)] -> Map C.ByteString C.ByteString
payloadToMap = fromList

buildEvent :: Map C.ByteString C.ByteString -> AnalyticsEvent
buildEvent lookupTable =
  AnalyticsEvent
    { action = orEmpty lookupTable "action"
    , category = orEmpty lookupTable "category"
    , label = orEmpty lookupTable "label"
    , resource = orEmpty lookupTable "resource"
    , time = orEmpty lookupTable "time"
    , session_id = orEmpty lookupTable "session_id"
    }
  where
    orEmpty :: Map C.ByteString C.ByteString -> String -> String
    orEmpty map key = maybe "" C.unpack $ lookup (C.pack key) map

payloadFromRedis ::
     Connection -> String -> String -> IO ([AnalyticsEvent], () -> IO ())
payloadFromRedis redisConnection consumerGroupId thisConsumer =
  runRedis redisConnection $ do
    streamReadResult <-
      xreadGroup
        (C.pack consumerGroupId)
        (C.pack thisConsumer)
        [("events", ">")] -- TODO: Add a count field to this call
    incr "event_consumes_total"
    return $ handlePayload streamReadResult
  where
    ackEvents :: [String] -> IO ()
    ackEvents [] = pure ()
    ackEvents messageIds = do
      let messageId = head messageIds
      runRedis redisConnection $ do
        xack "events" (C.pack consumerGroupId) (C.pack messageId)
        incr "event_acked_total"
        lpush "debug_acked_events" [C.pack messageId]
      putStrLn $ "Debug: ACKing event: " ++ messageId
      ackEvents $ tail messageIds
    handlePayload ::
         Either f (Maybe [XReadResponse]) -> ([AnalyticsEvent], () -> IO ())
    handlePayload (Left _) = ([], const $ pure ())
    handlePayload (Right Nothing) = ([], const $ pure ())
    handlePayload (Right (Just payload)) = do
      let (ids, events) =
            foldr foldRecordPairs ([], []) $
            fmap mapRecord $ records $ head payload
      (events, \_ -> ackEvents ids)
      where
        mapRecord :: StreamsRecord -> (String, AnalyticsEvent)
        mapRecord p =
          (C.unpack $ recordId p, (buildEvent . payloadToMap) $ keyValues p)
        foldRecordPairs ::
             (String, AnalyticsEvent)
          -> ([String], [AnalyticsEvent])
          -> ([String], [AnalyticsEvent])
        foldRecordPairs (id, event) (ids, events) =
          (ids ++ [id], events ++ [event])

-- BUG: If we try to create a consumer group without a stream being there, this will error/hang
-- Add logic to conditionally create a stream
createConsumerGroup :: Connection -> IO String
createConsumerGroup redisConnection = do
  let consumerGroupId = "consumers_1"
  -- Start listening to events from the beginning of time
  runRedis redisConnection $ do
    xadd "events" "*" [(C.pack "type", C.pack "starting_consumer_group")]
    xgroupCreate "events" consumerGroupId "0"
    incr "event_consumers"
  return $ C.unpack consumerGroupId
