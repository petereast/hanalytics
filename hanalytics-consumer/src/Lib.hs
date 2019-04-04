{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Lib
  ( someFunc
  , createConsumerGroup
  , payloadFromRedis
  ) where

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

payloadToMap -- oof that wasn't easy
 ::
     [(C.ByteString, C.ByteString)] -> Map C.ByteString C.ByteString
payloadToMap = fromList

buildEvent :: Map C.ByteString C.ByteString -> AnalyticsEvent
buildEvent lookupTable =
  AnalyticsEvent
    { action = C.unpack $ lookupTable ! C.pack "action"
    , category = C.unpack $ lookupTable ! C.pack "category"
    , label = C.unpack $ lookupTable ! C.pack "label"
    , resource = C.unpack $ lookupTable ! C.pack "resource"
    , time = C.unpack $ lookupTable ! C.pack "time"
    , session_id = C.unpack $ lookupTable ! C.pack "session_id"
    }

payloadFromRedis ::
     Connection -> String -> String -> IO ([AnalyticsEvent], () -> IO ())
payloadFromRedis redisConnection consumerGroupId thisConsumer =
  runRedis redisConnection $ do
    streamReadResult <-
      xreadGroup
        (C.pack consumerGroupId)
        (C.pack thisConsumer)
        [("events", ">")]
    incr "event_consumes"
    return $ handlePayload streamReadResult
  where
    ackEvent :: String -> IO ()
    ackEvent messageId = do
      runRedis redisConnection $ do
        xack "events" (C.pack consumerGroupId) (C.pack messageId)
        lpush "debug:acked_events" [C.pack messageId]
      return ()
    ackEvents :: [String] -> IO ()
    ackEvents messageIds = do
      let _ = fmap ackEvent messageIds
      pure ()
    handlePayload ::
         Either f (Maybe [XReadResponse]) -> ([AnalyticsEvent], () -> IO ())
    handlePayload (Left _) = ([], const $ pure ())
    handlePayload (Right Nothing) = ([], const $ pure ())
    handlePayload (Right (Just payload)) = do
      let (ids, events) =
            foldr foldRecordPairs ([], []) $
            fmap mapRecord $ records $ payload !! 1
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

createConsumerGroup :: Connection -> IO String
createConsumerGroup redisConnection = do
  let consumerGroupId = "some_string"
  -- Start listening to events from the beginning of time
  runRedis redisConnection $ do
    xgroupCreate "events" consumerGroupId "0"
    incr "event_consumers"
  return $ C.unpack consumerGroupId
