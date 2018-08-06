module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.State

import Const (fastLookback, fiveMinutes, slowLookback)
import MovingAverage (fastMovingAvg, simpleMovingAvg, slowMovingAvg)
import PriceState (PriceData (..), runPriceDataUpdater)
import Prices (getPrice, Price, takeNLatestPrices)

type PriceStateT = StateT PriceData IO

initialPriceData :: PriceData
initialPriceData = PriceData
    { ticks = 0
    , fiveMinPrices = []
    , oneHourPrices = []
    , hourSlowMovingAvg = slowMovingAvg initHourPrices (simpleMovingAvg (takeNLatestPrices slowLookback initHourPrices))
    , hourFastMovingAvg = fastMovingAvg initHourPrices (simpleMovingAvg (takeNLatestPrices fastLookback initHourPrices))
    , fiveMinFastMovingAvg = 0
    }
    where
        initHourPrices :: [Price]
        initHourPrices = []

main :: IO ()
main = trendFollower initialPriceData

trendFollower :: PriceData -> IO ()
trendFollower priceData = do
    newPrice <- getPrice
    let updatedPriceData = runPriceDataUpdater newPrice priceData
    print updatedPriceData
    print $ checkTrends updatedPriceData
    -- TODO notify listeners buy/sell
    threadDelay fiveMinutes
    trendFollower updatedPriceData

checkTrends :: PriceData -> String
checkTrends priceData
    | strongBuy priceData = "Buy"
    | weakBuy priceData = "Weak Buy"
    | strongSell priceData = "Sell"
    | weakSell priceData = "Weak Sell"
    | otherwise = "Not sure"
    where
        strongBuy :: PriceData -> Bool
        strongBuy priceData = hourFastMovingAvg priceData > hourSlowMovingAvg priceData
            && last (fiveMinPrices priceData) > hourFastMovingAvg priceData
            && last (fiveMinPrices priceData) > fiveMinFastMovingAvg priceData
        
        weakBuy :: PriceData -> Bool
        weakBuy priceData = hourFastMovingAvg priceData > hourSlowMovingAvg priceData

        strongSell :: PriceData -> Bool
        strongSell priceData = hourFastMovingAvg priceData < hourSlowMovingAvg priceData 
            && last (fiveMinPrices priceData) < hourFastMovingAvg priceData
            && last (fiveMinPrices priceData) < fiveMinFastMovingAvg priceData
        
        weakSell :: PriceData -> Bool
        weakSell priceData = hourFastMovingAvg priceData < hourSlowMovingAvg priceData