module MovingAverage 
    ( simpleMovingAvg
    , expMovingAvg
    ) where

import Client
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Maybe
import Data.IORef
import JSONTypes

-- get price every 5 minutes
-- every 8 prices - sum the prices and divide by 8 - this is the 8 period moving avg for 5 mins
-- every 12 prices - take the close price - this is the price of the last hour bar
-- every 8 hour bar prices - sum the prices and divide by 8 - this is 8 period moving avg for the hour
-- every 34 hour bar prices - sum the prices and divide by 34 - this is the 34 period mvg avg for the hour

-- need to keep track of the 5 minute prices in case the condition is met when we move to smaller window

-- need 408 5 minute prices to start
-- every 12th 5 min prices = 1 hour close
-- every 12 5 min prices - can drop the head * 12 prices (i.e. the first hour as we only need to look back 34 hours to get the average)

-- have 1 thread that gets prices every 5 minutes and puts into a time series/list
 
fiveMinutes = 300000000 -- microseconds
twoSecs = 2000000 -- microseconds

-- simple moving average
simpleMovingAvg :: [Double] -> Double 
simpleMovingAvg prices = sum prices / fromIntegral (length prices)

-- exponential moving average -- SMA used as previous period's first EMA
expMovingAvg :: [Double] -> Double -> Double
expMovingAvg prices prevEMA = (close - prevEMA) * multiplier + prevEMA
    where
        multiplier = 2.0 / (fromIntegral (length prices) + 1.0)
        close = last prices

-- exponential moving average
-- exponential moving average calc 
-- http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_averages#exponential_moving_average_calculation

    -- Initial SMA: 10-period sum / 10 

    -- Multiplier: (2 / (Time periods + 1) ) = (2 / (10 + 1) ) = 0.1818 (18.18%)

    -- EMA: {Close - EMA(previous day)} x multiplier + EMA(previous day). 
