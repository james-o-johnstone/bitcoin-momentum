module MovingAverage 
    ( Average
    , fastMovingAvg
    , slowMovingAvg
    , simpleMovingAvg
    ) where

import Const (fastLookback, slowLookback)
import Prices (Price, takeNLatestPrices)

type Average = Double

simpleMovingAvg :: [Price] -> Average 
simpleMovingAvg prices = sum prices / fromIntegral (length prices)

-- exponential moving average -- SMA used as previous period's first EMA
-- http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_averages#exponential_moving_average_calculation
expMovingAvg :: [Price] -> Average -> Average
expMovingAvg prices prevEMA = (close - prevEMA) * multiplier + prevEMA
    where
        multiplier = 2.0 / (fromIntegral (length prices) + 1.0)
        close = last prices

slowMovingAvg :: [Price] -> Average -> Average
slowMovingAvg prices prevMovingAvg
    | length prices < slowLookback = simpleMovingAvg prices
    | otherwise = expMovingAvg latestPrices prevMovingAvg
    where
        latestPrices = takeNLatestPrices slowLookback prices

fastMovingAvg :: [Price] -> Average -> Average
fastMovingAvg prices prevMovingAvg 
    | length prices < fastLookback = simpleMovingAvg prices
    | otherwise = expMovingAvg latestPrices prevMovingAvg
    where
        latestPrices = takeNLatestPrices fastLookback prices