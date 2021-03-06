{-# LANGUAGE OverloadedStrings #-}

module StreamUtils where

import Aggregator
import Control.Concurrent
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Database.Redis (Connection, Redis, expire, incr, lpush, runRedis)
import Lib

readStream :: Connection -> String -> IO ()
readStream redisConnection consumerGroupId = do
  output <- payloadFromRedis redisConnection consumerGroupId "consumer_1"
  afterwards output
  threadDelay $ 5000 * 1000
  readStream redisConnection consumerGroupId
  where
    afterwards :: ([AnalyticsEvent], () -> IO ()) -> IO ()
    afterwards (events, callback) = do
      handleEvents events
      callback ()
    handleEvents :: [AnalyticsEvent] -> IO ()
    handleEvents [] = pure ()
    handleEvents events' = do
      placeEventInBucket redisConnection $ head events'
      handleEvents $ tail events'

placeEventInBucket :: Connection -> AnalyticsEvent -> IO ()
placeEventInBucket redisConnection event =
  placeIntoBuckets (timeBucketId event) event
  where
    placeIntoBuckets :: Maybe String -> AnalyticsEvent -> IO ()
    placeIntoBuckets Nothing event = pure ()
    placeIntoBuckets (Just bucketId) event = do
      runRedis redisConnection $ do
        place label "label" event
        place category "category" event
        place action "action" event
        place resource "resource" event
        place session_id "session_id" event
        incr "processed_events"
      return ()
      where
        place ::
             (AnalyticsEvent -> String) -> String -> AnalyticsEvent -> Redis ()
        place selector label event = do
          let bucketName =
                C.pack $ label ++ ":" ++ selector event ++ ":" ++ bucketId
          lpush bucketName [L.toStrict $ encode event]
          expire bucketName $ 60 * 10
          return ()
-- TODO: Somehow cumulatively aggregate these into one bucket
-- Incoming events are placed in buckets aggregated by
-- - time
-- - attribtue-value
-- This means it'll be really easy to say "give me all the events with this value from this time"
-- We can think about how we store this data later
