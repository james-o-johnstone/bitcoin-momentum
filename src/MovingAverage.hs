module MovingAverage 
    ( simpleMovingAvg
    , expMovingAvg
    , GlobalState (..)
    , readPrices
    , getPriceEveryFiveMinutes
    ) where

import Client
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad
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

type GlobalState = MVar [Double]

data PriceState = PriceState {
    latestPrice :: Double
    , slowMovingAvg :: Double
    , fastMovingAvg :: Double
} deriving (Eq, Show)

type PriceData = State PriceState

readPrices :: GlobalState -> Double -> Double -> Double -> IO Double
readPrices state latestPrice slowMovingAvg fastMovingAvg = do
        currentPrices <- readMVar state
        if pricesChanged currentPrices then priceUpdate currentPrices else readPrices state latestPrice slowMovingAvg fastMovingAvg

    where
        pricesChanged currentPrices = last currentPrices /= latestPrice

        priceUpdate prices = do
            print $ "New price " ++ (show (last prices))
            if length prices > 34 
                then calculateMovingAverages prices slowMovingAvg fastMovingAvg
                else readPrices state (last prices) (calcSlowMovingAvg prices) (calcFastMovingAvg prices)
            
        calcSlowMovingAvg prices = simpleMovingAvg (take 34 (reverse prices))
        calcFastMovingAvg prices = simpleMovingAvg (take 8 (reverse prices))

        calculateMovingAverages :: [Double] -> Double -> Double -> IO Double
        calculateMovingAverages prices prevSlowExpMvgAvg prevFastExpMvgAvg = do
            let fastMovingAvg = expMovingAvg (take 8 (reverse prices)) prevFastExpMvgAvg
            print $ "Fast avg " ++ (show fastMovingAvg)
            let slowMovingAvg = expMovingAvg (take 34 (reverse prices)) prevSlowExpMvgAvg
            print $ "Slow avg " ++ (show slowMovingAvg)
            if fastMovingAvg > slowMovingAvg then print "Upwards trend" else print "Downwards trend"
            readPrices state (last prices) slowMovingAvg fastMovingAvg

getPriceEveryFiveMinutes :: GlobalState -> IO ()
getPriceEveryFiveMinutes state = do
    currentPrices <- takeMVar state
    price <- getPrice
    print ("Got Price " ++ show price)
    putMVar state (currentPrices ++ [price])
    threadDelay twoSecs
    getPriceEveryFiveMinutes state

getPrice :: IO Double
getPrice = do
    quote <- getQuote "BTCGBP"
    print $ timestamp $ fromJust quote
    print $ price $ fromJust quote
    case quote of
        Just q -> return $ price q
        Nothing -> return 0.0

-- 8 period moving average - add up prices of last 8 periods and divide by 8
-- on a 1 hour chart this is prices of last 8 hours / 8
-- on a 5 min char this is prices of last (5*8 mins) 40 mins / 8

-- 34 period moving average

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
