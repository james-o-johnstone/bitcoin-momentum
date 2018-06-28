module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Async
import Control.Monad
import Data.Maybe
import Data.IORef
import Debug.Trace

import Client
import JSONTypes

import MovingAverage

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
 
main :: IO ()
main = do
    -- three background threads:
    globalState <- newMVar []
    print "Hello"
    withAsync (getPriceEveryFiveMinutes globalState) $ \writerThread ->
        withAsync (readPrices globalState 0 0 0) $ \readerThread -> do
            print "main thread"
            threadDelay 100000000000000
            return ()
    -- fiveMinFMA <- periodMvgAvg fiveMinutes 8
    -- print fiveMinFMA
    -- hourFMA <- periodMvgAvg oneHour 8
    -- hourSMA <- periodMvgAvg oneHour 34 -- need to seed with last 34 hours closing prices to start this

    -- every hour - check whether the hourFMA > hourSMA
    -- if hourFMA > hourSMA
            -- buy when price > fiveMinFMA

    -- every hour check whether the hourFMA < hourSMA
    -- if hourFMA < hourSMA
        -- every five minutes check whether the price < fiveMinSMA
            -- sell when price < fiveMinSMA

    -- print mvgAvg
    -- multipleDays
    -- prices <- getPricesWhileMarketOpen
    -- seq <- sequence prices
    -- print $ "Today's low:" ++ show (minimum seq)

multipleDays :: IO ()
multipleDays = do
    dayRange <- oneDay
    multipleDays

oneDay :: IO Range
oneDay = do
    range <- getTodaysRange
    print $ "Today's high = " ++ show (high range)
    print $ "Today's low = " ++ show (low range)
    print $ "Today's close = " ++ show (close range) 
    return range

getTodaysRange :: IO Range
getTodaysRange = do
    prices <- getPricesWhileMarketOpen
    -- seq <- sequence prices
    return $ Range (maximum prices) (minimum prices) (last prices)

getPricesWhileMarketOpen :: IO [Double]
getPricesWhileMarketOpen = go [] 5
    where
        go l nPrices = do
--            isOpen <- isMarketOpen
            price <- getPrice
            print nPrices
            if nPrices > 0 then go (l ++ [price]) (nPrices-1) else return (l ++ [price])
            -- if isOpen then go (l ++ [price]) (nPrices-1) else return l

fiveMinutes = 300000000 -- microseconds
twoSecs = 2000000 -- microseconds

getPrice :: IO Double
getPrice = do
    quote <- getQuote "BTCGBP"
    print $ timestamp $ fromJust quote
    print $ price $ fromJust quote
    threadDelay twoSecs
    case quote of
        Just q -> return $ price q
        Nothing -> return 0.0

isMarketOpen :: IO Bool
isMarketOpen = do
    marketStatus <- marketIsOpen
    case marketStatus of
        Just m -> return $ market_is_open m
        Nothing -> return False


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
