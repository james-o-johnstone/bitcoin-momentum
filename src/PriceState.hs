module PriceState (
    PriceData (..)
    , updatePrices
)
where

import Control.Monad.State

import MovingAverage

data PriceData = PriceData {
    latestPrice :: Double
    , fiveMinPrices :: [Double]
    , oneHourPrices :: [Double]
    , hourSlowMovingAvg :: Double
    , hourFastMovingAvg :: Double
    , fiveMinFastMovingAvg :: Double 
} deriving (Eq, Show)

type PriceState = State PriceData

updatePrices = runState updatePriceData

updatePriceData :: PriceState ()
updatePriceData = do
    updateFiveMinPrices
    updateOneHourPrices
    removeStalePrices 
    updateFiveMinTrends
    updateOneHourTrends

updateFiveMinTrends :: PriceState ()
updateFiveMinTrends = do
    modify (\priceData -> priceData {
        fiveMinFastMovingAvg = calculateFastMovingAverage (fiveMinPrices priceData) (fiveMinFastMovingAvg priceData)
    })
    return ()

updateOneHourTrends :: PriceState ()
updateOneHourTrends = do
    modify (\priceData -> priceData {
        hourSlowMovingAvg = calculateSlowMovingAverage (oneHourPrices priceData) (hourSlowMovingAvg priceData)
        , hourFastMovingAvg = calculateFastMovingAverage (oneHourPrices priceData) (hourFastMovingAvg priceData)
    })
    return ()

updateFiveMinPrices :: PriceState ()
updateFiveMinPrices = do 
    modify (\priceData -> priceData {
        fiveMinPrices = fiveMinPrices priceData ++ [latestPrice priceData]
    })
    return ()

updateOneHourPrices :: PriceState ()
updateOneHourPrices = do
    priceData <- get
    when (length (fiveMinPrices priceData) `mod` 12 == 0) $
        modify (\pd -> pd {oneHourPrices = oneHourPrices priceData ++ [latestPrice priceData]})
    return ()
    
removeStalePrices :: PriceState ()
removeStalePrices = do 
    modify (\priceData -> priceData {
        fiveMinPrices = take 8 (fiveMinPrices priceData)
        , oneHourPrices = take 34 (oneHourPrices priceData)
    })
    return ()