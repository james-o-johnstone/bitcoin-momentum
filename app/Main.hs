module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.State

import MovingAverage
import PriceState
import Prices

type PriceStateT = StateT PriceData IO

initialPriceData :: PriceData
initialPriceData = PriceData 0 [] [] 0 0 0 -- update this to contain prev data to not have to wait 34 hours at start!

main :: IO ((), PriceData)
main = runStateT trendFollower initialPriceData

trendFollower :: PriceStateT ()
trendFollower = forever $ do
    newPrice <- liftIO getPrice
    priceData <- get
    let updatedPriceData = runPriceDataUpdater newPrice priceData
    liftIO $ print updatedPriceData
    put updatedPriceData
    -- check trends and whether to notify listeners buy/sell
    liftIO $ threadDelay twoSecs --fiveMinutes

fiveMinutes :: Int
fiveMinutes = 300000000 -- microseconds

twoSecs :: Int
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
