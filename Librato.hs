{-# LANGUAGE OverloadedStrings #-}
module Librato (incr) where

import Control.Monad.State
import Control.Applicative
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS
import System.Environment
import Control.Concurrent

type Metric = String
type Count = Integer
type Time = Integer

incr :: Metric -> Integer -> StateT Integer IO Integer
incr metricName frequency = do
  entries <- get
  let newCount = entries + 1
  if newCount > frequency then flushAndReset newCount else put newCount
  return entries
  where flushAndReset c = (lift . forkIO $ postMetric metricName c) >> put 0

postMetric :: Metric -> Count -> IO ()
postMetric metricName count = do
       t <- roundedTime
       request <- libratoRequest t count metricName
       print =<< withManager (httpLbs request)
       where roundedTime = round <$> getPOSIXTime

libratoRequest :: Time -> Count -> Metric -> IO Request
libratoRequest t c m = do uname <- getEnv "LIBRATO_USERNAME"
                          pword <- getEnv "LIBRATO_PASSWORD"
                          urlEncodedBody params <$> authed uname pword <$> parsedUrl
    where parsedUrl = parseUrl "http://metrics-api.librato.com/v1/metrics"
          authed u p =  applyBasicAuth (BS.pack u) (BS.pack p)
          params = [("counters[0][value]", BS.pack $ show c), ("measure_time", BS.pack $ show t), ("counters[0][name]", BS.pack m)]

