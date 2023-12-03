{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module AoC.Day.Day3T ( day3T ) where

import AoC.Day.Day3
import qualified Data.Map as M
import Test.Hspec

testCase :: String
testCase =
  "467..114..\n\
  \...*......\n\
  \..35..633.\n\
  \......#...\n\
  \617*......\n\
  \.....+.58.\n\
  \..592.....\n\
  \......755.\n\
  \...$.*....\n\
  \.664.598.."

day3T :: IO ()
day3T = do
  let poss@(syms, nums) = findPossible testCase
      adj               = fmap number (findParts poss)
  print adj

  hspec $ do
    describe "the part finder" $ do

      it "should find all numbers and symbols in the engine mapping" $ do
        syms M.!? (2, 4) `shouldBe` Just '*'
        (35 `elem` fmap number nums) `shouldBe` True

      it "should emit numbers that can find adjacencies" $ do
        let ~(n467 : _) = filter ((== 467) . number) nums
        (isAdjacent n467 (1, 1)) `shouldBe` True
        (isAdjacent n467 (2, 4)) `shouldBe` True
        (isAdjacent n467 (3, 3)) `shouldBe` False

        let ~(n35 : _) = filter ((== 35) . number) nums
        (isAdjacent n35 (1, 1)) `shouldBe` False
        (isAdjacent n35 (2, 3)) `shouldBe` True
        (isAdjacent n35 (2, 1)) `shouldBe` False
        (isAdjacent n35 (2, 2)) `shouldBe` True

      it "should find every number adjacent to a symbol" $ do
        sum adj `shouldBe` 4361

    describe "the gear ratio finder" $ do
      it "should emit the proper gear ratio sum" $
        sum (fmap gearRatio (gears poss)) `shouldBe` 467835
