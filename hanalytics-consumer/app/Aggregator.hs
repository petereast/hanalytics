module Aggregator where

import qualified Data.Map.Lazy as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Time
import Data.Time.Format
import Lib (AnalyticsEvent, action, resource, session_id, time)

pageViewAction :: String
pageViewAction = "PAGE_VIEW"

-- I want this to group 
-- Take the events from the stream, aggregate every 10second bucket into a single entity with stats and stuff
--
timeBucketId :: AnalyticsEvent -> Maybe String -- If an event doesn't have a valid time, we can safely drop it
timeBucketId event = do
  let time' = time event
  let utcTime =
        parseTimeM True defaultTimeLocale "%F %H:%M:%S%Q UTC" time' :: Maybe UTCTime
  formatTime defaultTimeLocale "%F%H%M" <$> utcTime -- this will result in events being aggregated per minute

data ResourceUsage = ResourceUsage
  { usageResourceId :: String
  , usageCount :: Int
  }

-- This will look the same as what is sent back by the API
data AggregatedAnalytics = AggregatedAnalytics
  { rawEvents :: [AnalyticsEvent]
  , resourceUsage :: [ResourceUsage]
  , pageViews :: Int
  , eventCount :: Int
  , uniqueSessions :: Int
  , eventsPerSession :: Int -- Mean events per session
  }

calculatePageViews :: [AnalyticsEvent] -> Int
calculatePageViews events =
  length $ filter (\ev -> action ev == pageViewAction) events

calculateEventCount :: [AnalyticsEvent] -> Int
calculateEventCount = length

calculateUniqueSessions :: [AnalyticsEvent] -> Int
calculateUniqueSessions events = Set.size $ Set.fromList $ session_id <$> events

-- TODO: Refactor the following two functions to be more DRY
calculateResourceUsage :: [AnalyticsEvent] -> [ResourceUsage]
calculateResourceUsage events =
  fmap toRecord $ Map.toList $ foldr perEvent Map.empty events
  where
    perEvent :: AnalyticsEvent -> Map.Map String Int -> Map.Map String Int
    perEvent ev acc = update $ Map.lookup (resource ev) acc
      where
        update Nothing = Map.insert (resource ev) 1 acc
        update (Just id) = Map.adjust succ (resource ev) acc
    toRecord :: (String, Int) -> ResourceUsage
    toRecord (resource, count) =
      ResourceUsage {usageResourceId = resource, usageCount = count}

calculateEventsPerSession :: [AnalyticsEvent] -> Int
calculateEventsPerSession events = do
  let eventCounts = Map.elems $ foldr perEvent Map.empty events
  let eventsTotal = sum eventCounts
  let eventsCount = length eventCounts
  div eventsTotal eventsCount
  where
    perEvent :: AnalyticsEvent -> Map.Map String Int -> Map.Map String Int
    perEvent ev acc = update $ Map.lookup (session_id ev) acc
      where
        update Nothing = Map.insert (session_id ev) 1 acc
        update (Just id) = Map.adjust succ (session_id ev) acc

-- Maybe this code should go into another consumer? 
createAggregatedData :: [AnalyticsEvent] -> AggregatedAnalytics
createAggregatedData events =
  AggregatedAnalytics
    { rawEvents = events
    , eventsPerSession = calculateEventsPerSession events
    , resourceUsage = calculateResourceUsage events
    , pageViews = calculatePageViews events
    , eventCount = calculateEventCount events
    , uniqueSessions = calculateUniqueSessions events
    }
