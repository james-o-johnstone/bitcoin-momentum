module MovingAverage 
    ( calculateFastMovingAverage
    , calculateSlowMovingAverage
    ) where

-- simple moving average
simpleMovingAvg :: [Double] -> Double 
simpleMovingAvg prices = sum prices / fromIntegral (length prices)

-- exponential moving average -- SMA used as previous period's first EMA
expMovingAvg :: [Double] -> Double -> Double
expMovingAvg prices prevEMA = (close - prevEMA) * multiplier + prevEMA
    where
        multiplier = 2.0 / (fromIntegral (length prices) + 1.0)
        close = last prices

calculateSlowMovingAverage :: [Double] -> Double -> Double
calculateSlowMovingAverage prices prevMovingAvg
    | length prices < 34 = simpleMovingAvg prices
    | otherwise = expMovingAvg (take 34 (reverse prices)) prevMovingAvg

calculateFastMovingAverage :: [Double] -> Double -> Double
calculateFastMovingAverage prices prevMovingAvg 
    | length prices < 8 = simpleMovingAvg prices
    | otherwise = expMovingAvg (take 8 (reverse prices)) prevMovingAvg

-- exponential moving average
-- exponential moving average calc 
-- http://stockcharts.com/school/doku.php?id=chart_school:technical_indicators:moving_averages#exponential_moving_average_calculation

    -- Initial SMA: 10-period sum / 10 

    -- Multiplier: (2 / (Time periods + 1) ) = (2 / (10 + 1) ) = 0.1818 (18.18%)

    -- EMA: {Close - EMA(previous day)} x multiplier + EMA(previous day). 
