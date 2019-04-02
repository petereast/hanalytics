module Main where

import Lib
import Snap.Core
import Snap.Http.Server

main :: IO ()
main = do
  putStrLn "Init"
  someFunc
-- TODO: Define the API for ingesting analytics events here - think of this file as linking
-- http with the streams based backend

WebServer :: Connection -> Snap ()
WebServer redisConnection =
  ifTop (writeBS "System is healthy!")
