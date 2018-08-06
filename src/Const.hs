module Const (
    fastLookback
    , fiveMinutes
    , slowLookback
    , twoSecs
    ) where

fastLookback :: Int
fastLookback = 8

slowLookback :: Int
slowLookback = 34

fiveMinutes :: Int
fiveMinutes = 300000000 -- microseconds

twoSecs :: Int
twoSecs = 2000000 -- microseconds