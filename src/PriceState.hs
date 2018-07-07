module PriceState (
    PriceData (..)
    , runPriceDataUpdater
)
where

import Control.Monad.State

import MovingAverage

data PriceData = PriceData {
    ticks :: Integer
    , fiveMinPrices :: [Double]
    , oneHourPrices :: [Double]
    , hourSlowMovingAvg :: Double
    , hourFastMovingAvg :: Double
    , fiveMinFastMovingAvg :: Double 
} deriving (Eq, Show)

type PriceState = State PriceData

runPriceDataUpdater :: Double -> PriceData -> PriceData
runPriceDataUpdater newPrice = execState $ updatePriceData newPrice

updatePriceData :: Double -> PriceState ()
updatePriceData newPrice = do
    tick
    updateFiveMinPrices newPrice
    updateOneHourPrices
    removeStalePrices 
    updateFiveMinTrends
    updateOneHourTrends

tick :: PriceState ()
tick = modify (\priceData -> priceData {
    ticks = ticks priceData + 1
})

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

updateFiveMinPrices :: Double -> PriceState ()
updateFiveMinPrices newPrice = do 
    modify (\priceData -> priceData {
        fiveMinPrices = fiveMinPrices priceData ++ [newPrice]
    })
    return ()

-- also check whether there are more than 34 hour prices - if so then can remove last item from the list
-- as max lookback is only 34 hours
updateOneHourPrices :: PriceState ()
updateOneHourPrices = do
    priceData <- get
    when (ticks priceData `mod` 12 == 0) $
        modify (\pd -> pd {oneHourPrices = oneHourPrices priceData ++ [last $ fiveMinPrices priceData]})
    return ()
    
removeStalePrices :: PriceState ()
removeStalePrices = do 
    modify (\priceData -> priceData {
        fiveMinPrices = take 8 (fiveMinPrices priceData)
        , oneHourPrices = take 34 (oneHourPrices priceData)
    })
    return ()