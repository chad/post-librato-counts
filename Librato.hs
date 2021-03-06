{-# LANGUAGE OverloadedStrings #-}
module Librato (incr) where

import Control.Monad.State
import Control.Applicative
import Data.Time.Clock.POSIX
import Network.HTTP.Conduit
import qualified Data.ByteString.Char8 as BS

type Metric = String
type Count = Integer

incr :: Metric -> Integer -> StateT Integer IO Integer
incr metricName frequency = do
  entries <- get
  let newCount = entries + 1
  if newCount > frequency
    then flushAndReset newCount
    else put newCount
  return entries
  where flushAndReset c = do { lift $ postMetric metricName c; put 0}

postMetric :: Metric -> Count -> IO ()
postMetric metricName count = do
       t <- roundedTime
       request <- libratoRequest t count metricName
       print =<< withManager (httpLbs request)
       where roundedTime = round <$> getPOSIXTime

libratoRequest :: Integer -> Count -> Metric -> IO Request
libratoRequest time count metricName = urlEncodedBody  headers <$> authed <$> parsedUrl
    where parsedUrl = parseUrl "http://metrics-api.librato.com/v1/metrics"
          authed =  applyBasicAuth (BS.pack "LUSER") (BS.pack "PASS")
          headers = [("counters[0][value]", BS.pack $ show count), ("measure_time", BS.pack $ show time), ("counters[0][name]", BS.pack metricName)]

