module AoC.Day.Day6T ( day6T ) where

import AoC.Day.Day6
import Test.Hspec

day6T :: IO ()
day6T = hspec $ do
  describe "the optimizer" $ do
    it "should correctly output the winning amount of races" $ do
      optimize 7 9 `shouldBe` 4
      optimize 15 40 `shouldBe` 8
      optimize 30 200 `shouldBe` 9
