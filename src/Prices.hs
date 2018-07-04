module Prices (
    getPrice
    ) where

import Data.Maybe

import Client
import JSONTypes

getPrice :: IO Double
getPrice = do
    quote <- getQuote "BTCGBP"
    print $ "Timestamp: " ++ show (timestamp (fromJust quote)) ++ ", New price: " ++ show (price (fromJust quote))
    case quote of
        Just q -> return $ price q
        Nothing -> return 0.0