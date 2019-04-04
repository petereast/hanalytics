module Main where

import Database.Redis (checkedConnect, defaultConnectInfo)
import Lib

main :: IO ()
main = do
  putStrLn "Starting"
  redisConnection <- checkedConnect defaultConnectInfo
  consumerGroupId <- createConsumerGroup redisConnection
  (evs, fn) <- payloadFromRedis redisConnection consumerGroupId "consumer_1"
  fn ()
  putStrLn consumerGroupId
  return ()
