module AoC.Day.Day9T ( day9T ) where

import AoC.Day.Day9
import qualified Data.Sequence as S
import Test.Hspec

day9T :: IO ()
day9T = hspec $ do
  describe "the sequence extender" $ do
    it "should use the nth difference method to extend polynomial sequences" $ do
      -- Rear extension.
      slast (nthDifferenceExt (S.fromList [0, 3, 6, 9, 12, 15])) `shouldBe` 18
      slast (nthDifferenceExt (S.fromList [1, 3, 6, 10, 15, 21])) `shouldBe` 28
      slast (nthDifferenceExt (S.fromList [10, 13, 16, 21, 30, 45])) `shouldBe` 68
      
      -- Front extension.
      shead (nthDifferenceExt (S.fromList [10, 13, 16, 21, 30, 45])) `shouldBe` 5
