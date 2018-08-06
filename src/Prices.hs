module Prices 
    ( getPrice
    , Price
    , takeNLatestPrices
    ) where

import Data.Maybe (fromJust)

import Client (getQuote)
import JSONTypes (Quote (..))

type Price = Double

getPrice :: IO Price
getPrice = do
    quote <- getQuote "BTCGBP"
    print $ "Timestamp: " ++ show (timestamp (fromJust quote)) ++ ", New price: " ++ show (price (fromJust quote))
    case quote of
        Just q -> return $ price q
        Nothing -> return 0.0

takeNLatestPrices :: Int -> [Price] -> [Price]
takeNLatestPrices n = reverse . take n . reverse