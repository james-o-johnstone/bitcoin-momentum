module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad
import Control.Monad.State
import Data.Maybe
import Data.IORef
import Debug.Trace

import Client
import JSONTypes

import MovingAverage
import Prices

-- while market is open 
    -- append prices into a list
-- when market is closed
    -- find max of the list = today's high

maximum' :: Ord a => [a] -> a
maximum' [x]       = x
maximum' (x:x':xs) = maximum' ((if x >= x' then x else x'):xs)

-- repeat nQuotes times: 
-- call getQuotes to get a quote
    -- put quote into a list

data Range = Range {
    high :: Double
    , low :: Double
    , close :: Double
}

-- price state:
-- five minute prices
-- 1 hour prices
-- slow moving avg (1 hour)
-- fast moving avg (1 hour/5 minute)
data PriceData = PriceData {
    fiveMinPrices :: [Double]
    , oneHourPrices :: [Double]
    , hourSlowMovingAvg :: Double
    , hourFastMovingAvg :: Double
    , fiveMinFastMovingAvg :: Double 
} deriving (Eq, Show)

type PriceState = State PriceData

initialPriceData :: PriceData
initialPriceData = PriceData [] [] 0 0 0 -- update this to contain prev data to not have to wait 34 hours at start!

main :: IO ()
main = runStateT trendFollower initialPriceData

trendFollower :: StateT PriceState IO ()
trendFollower = forever $ do
    newPrice <- liftIO getPrice
    updatedPriceData <- updatePriceData newPrice
    liftIO $ print updatedPriceData
    -- check trends and whether to notify listeners buy/sell
    threadDelay twoSecs --fiveMinutes

updateTrends :: PriceData -> PriceData
updateTrends = updateOneHourTrends . updateFiveMinTrends

updateFiveMinTrends :: PriceState ()
updateFiveMinTrends priceData = modify (\priceData -> priceData {
    fiveMinFastMovingAvg = calculateFastMovingAverage (fiveMinPrices priceData) (fiveMinFastMovingAvg priceData)
})

updateOneHourTrends :: PriceState ()
updateOneHourTrends = modify (\priceData -> priceData {
    hourSlowMovingAvg = calculateSlowMovingAverage (oneHourPrices priceData) (hourSlowMovingAvg priceData)
    , hourFastMovingAvg = calculateFastMovingAverage (oneHourPrices priceData) (hourFastMovingAvg priceData)
})

calculateSlowMovingAverage :: [Double] -> Double -> Double
calculateSlowMovingAverage prices prevMovingAvg
    | length prices < 34 = simpleMovingAvg prices
    | otherwise = expMovingAvg (take 34 (reverse prices)) prevMovingAvg

calculateFastMovingAverage :: [Double] -> Double -> Double
calculateFastMovingAverage prices prevMovingAvg 
    | length prices < 8 = simpleMovingAvg prices
    | otherwise = expMovingAvg (take 8 (reverse prices)) prevMovingAvg

updatePriceData :: Double -> PriceState ()
updatePriceData newPrice = do
    updateFiveMinPrices newPrice
    updateOneHourPrices
    updateFiveMinTrends
    updateOneHourTrends

updateFiveMinPrices :: Double -> PriceState()
updateFiveMinPrices newPrice = modify (\priceData -> priceData {
    fiveMinPrices = fiveMinPrices ++ [newPrice]
})

-- updateOneHourPrices checks whether the new five minute price occcurs at the end of an hour
-- if it does then update the last hour price
-- also check whether there are more than 34 hour prices - if so then can remove last item from the list
-- as max lookback is only 34 hours
updateOneHourPrices :: PriceState -> [Double]
updateOneHourPrices priceData newPrice
    | length (fiveMinPrices priceData) `mod` 12 == 0 = removeStalePrices (oneHourPrices priceData) 34 ++ [newPrice] -- removeStalePrices then addNewPrice, e.g. addNewPrice . removeStalePrices prices
    | otherwise = oneHourPrices priceData

removeStalePrices :: [Double] -> Int -> [Double]
removeStalePrices prices maxNPrices = take maxNPrices prices
    -- get price every 5 minutes
    -- on every 12th price - close = new 1 hour price
    -- if there < 34 1 hour prices then calculate simple moving avg using 1 hour prices
    -- if > 34 1 hour prices then calculate exponential moving avgs using 1 hour prices
    -- if FMA > SMA then calculate mvg avgs with 5 minute prices
    -- if price > FMA then BUY signal (Tweet/etc.)


    -- every hour - check whether the hourFMA > hourSMA
    -- if hourFMA > hourSMA
            -- buy when price > fiveMinFMA

    -- every hour check whether the hourFMA < hourSMA
    -- if hourFMA < hourSMA
        -- every five minutes check whether the price < fiveMinSMA
            -- sell when price < fiveMinSMA

fiveMinutes = 300000000 -- microseconds
twoSecs = 2000000 -- microseconds

-- https://www.dailyfx.com/forex/education/trading_tips/daily_trading_lesson/2012/03/26/Short_Term_Momentum_Scalping_in_Forex.html



-- calculate 8 period exponential moving average (fast moving average)

-- calculate 34 period exponential moving average (slow moving average)

-- if FMA > SMA - wait until price > (SMA AND FMA) in a bull market ( 1 hour chart )

    -- buy when price > 8 EMA on the 5 minute chart

-- if FMA < SMA - wait until price < (SMA AND FMA) in a bear market

    -- sell when price < 8 EMA on the 5 minute chart

    
-- find maximum and minimum in the list
-- print max and min
-- getMultipleQuotes :: Int -> [Maybe [Quotes]]

-- Trading strategy: 
-- http://www.quantifiedstrategies.com/buy-when-sp-500-makes-new-intraday-high/
-- http://jonathankinlay.com/2018/06/simple-momentum-strategy/

-- if todays high > previous 5 day high
    -- if IBS < 0.15
        -- buy
    -- if today's close > yesterday's high 
        -- sell
    -- both entry and exiton the close

-- Internal Bar Strength (IBS) = (Close - Low) / (High - Low)

-- make request to API every ~5 minutes
-- track current high, current low
-- also need to store highest over the last 5 days (do this at the end of the day)
-- also need to store the closing price of the previous day
