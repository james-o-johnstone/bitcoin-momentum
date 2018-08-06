module PriceState 
    ( PriceData (..)
    , runPriceDataUpdater
    ) where

import Control.Monad.State

import Const (fastLookback, slowLookback)
import MovingAverage (Average, fastMovingAvg, slowMovingAvg)
import Prices (Price, takeNLatestPrices)

data PriceData = PriceData 
    { ticks :: !Int
    , fiveMinPrices :: ![Price]
    , oneHourPrices :: ![Price]
    , hourSlowMovingAvg :: !Average
    , hourFastMovingAvg :: !Average
    , fiveMinFastMovingAvg :: !Average 
    } deriving (Eq, Show)

type PriceState = State PriceData

runPriceDataUpdater :: Price -> PriceData -> PriceData
runPriceDataUpdater newPrice = execState $ updatePriceData newPrice

updatePriceData :: Price -> PriceState ()
updatePriceData newPrice = do
    tick
    updateFiveMinPrices newPrice
    updateOneHourPrices
    removeStalePrices 
    updateFiveMinTrends
    updateOneHourTrends

tick :: PriceState ()
tick = modify (\pd -> pd {
    ticks = ticks pd + 1
})

updateFiveMinTrends :: PriceState ()
updateFiveMinTrends = do
    modify (\pd -> pd {
        fiveMinFastMovingAvg = fastMovingAvg (fiveMinPrices pd) (fiveMinFastMovingAvg pd)
    })
    return ()

updateOneHourTrends :: PriceState ()
updateOneHourTrends = do
    modify (\pd -> pd {
        hourSlowMovingAvg = slowMovingAvg (oneHourPrices pd) (hourSlowMovingAvg pd)
        , hourFastMovingAvg = fastMovingAvg (oneHourPrices pd) (hourFastMovingAvg pd)
    })
    return ()

updateFiveMinPrices :: Double -> PriceState ()
updateFiveMinPrices newPrice = do 
    modify (\priceData -> priceData {
        fiveMinPrices = fiveMinPrices priceData ++ [newPrice]
    })
    return ()

updateOneHourPrices :: PriceState ()
updateOneHourPrices = do
    priceData <- get
    when (oneHourPassed $ ticks priceData) $
        modify (\pd -> pd {oneHourPrices = oneHourPrices priceData ++ [latestPrice priceData]})
    return ()
    where 
        oneHourPassed t = t `mod` 12 == 0
        latestPrice priceData = last $ fiveMinPrices priceData

removeStalePrices :: PriceState ()
removeStalePrices = do 
    modify (\pd -> pd {
        fiveMinPrices = takeNLatestPrices fastLookback (fiveMinPrices pd)
        , oneHourPrices = takeNLatestPrices slowLookback (oneHourPrices pd)
    })
    return ()