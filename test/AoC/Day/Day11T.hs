module AoC.Day.Day11T ( day11T ) where

import AoC.Day.Day11
import AoC.Util
import qualified Data.Set as S
import Test.Hspec

testCase :: String
testCase =
  "...#......\n\
  \.......#..\n\
  \#.........\n\
  \..........\n\
  \......#...\n\
  \.#........\n\
  \.........#\n\
  \..........\n\
  \.......#..\n\
  \#...#....."

day11T :: IO ()
day11T = hspec $ do
  let galaxies   = expandedMap 1 testCase
      galaxiesS  = S.fromList galaxies
      galaxies10 = expandedMap 9 testCase

  describe "the galaxy map reader and expander" $ do
    it "should read the map, and expand empty rows and columns" $ do
      Galaxy (5, 1) `shouldSatisfy` (`S.member` galaxiesS)
      Galaxy (10, 2) `shouldSatisfy` (`S.member` galaxiesS)
      Galaxy (1, 3) `shouldSatisfy` (`S.member` galaxiesS)
      Galaxy (9, 6) `shouldSatisfy` (`S.member` galaxiesS)
      Galaxy (2, 7) `shouldSatisfy` (`S.member` galaxiesS)
      Galaxy (13, 8) `shouldSatisfy` (`S.member` galaxiesS)
      Galaxy (10, 11) `shouldSatisfy` (`S.member` galaxiesS)
      Galaxy (1, 12) `shouldSatisfy` (`S.member` galaxiesS)
      Galaxy (6, 12) `shouldSatisfy` (`S.member` galaxiesS)

  describe "the path finder" $ do
    it "should find the shortest path pairwise between galaxies" $ do
      sum (uncurry pathLength <$> triangle galaxies) `shouldBe` 374
      sum (uncurry pathLength <$> triangle galaxies10) `shouldBe` 1030

