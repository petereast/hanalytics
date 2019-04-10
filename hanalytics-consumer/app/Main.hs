{-# LANGUAGE OverloadedStrings #-}

module Main where

import Aggregator
import Control.Concurrent
import Control.Monad.IO.Class
import Data.Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Maybe
import Database.Redis
import Lib
import StreamUtils
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  putStrLn "[info] Startup - master thread"
  streamConsumer
  -- put an api here
  return ()

streamConsumer :: IO ()
streamConsumer = do
  putStrLn "[info] Startup - stream consumer"
  redisConnection <- checkedConnect redisConnectionInfo
  consumerGroupId <- createConsumerGroup redisConnection
  readStream redisConnection consumerGroupId
  where
    redisConnectionInfo :: ConnectInfo
    redisConnectionInfo = do
      let hostName =
            fromMaybe "localhost" $ unsafePerformIO $ lookupEnv "REDIS_URL"
            -- I shouldn't do unsafePerformIO but it's only for an envVar so it should be ok
      defaultConnectInfo {connectHost = hostName}
