module Aggregator where

import Data.Time
import Data.Time.Format
import Lib

someOtherFunc :: IO ()
someOtherFunc = putStrLn "ok!"

type Seconds = Int

eventGroupingFrequency :: Seconds
eventGroupingFrequency = 10 -- seconds

-- I want this to group 
-- Take the events from the stream, aggregate every 10second bucket into a single entity with stats and stuff
--
timeBucketId :: AnalyticsEvent -> Maybe String -- If an event doesn't have a valid time, we can safely drop it
timeBucketId event = do
  let time' = time event
  let utcTime =
        parseTimeM True defaultTimeLocale "%F %H:%M:%S%Q UTC" time' :: Maybe UTCTime
  formatTime defaultTimeLocale "%F%H%M" <$> utcTime -- this will result in events being aggregated per minute
