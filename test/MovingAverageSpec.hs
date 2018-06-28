module MovingAverageSpec where

import MovingAverage

import SpecHelper

spec :: Spec
spec =
    describe "lib tests" $ do
        describe "simpleMovingAvg" $ do
            context "with [1,2,3,4,5]" $
                it "should be 3" $
                    simpleMovingAvg [1.0, 2.0, 3.0, 4.0, 5.0] `shouldBe` 3.0

        describe "expMovingAvg" $ do
            context "with [1,2,3,4,5] 3.67" $ 
                it "should be 5.8" $
                    expMovingAvg [1.0, 2.0, 3.0, 4.0, 5.0] 3.0 `shouldBe` 3.6666666666666665 


main :: IO()
main = hspec spec