module AoC.Day.Day5T ( day5T ) where

import AoC.Day.Day5
import qualified Data.Set as S
import Test.Hspec

testCase :: String
testCase =
  "seeds: 79 14 55 13\n\
  \\n\
  \seed-to-soil map:\n\
  \50 98 2\n\
  \52 50 48\n\
  \\n\
  \soil-to-fertilizer map:\n\
  \0 15 37\n\
  \37 52 2\n\
  \39 0 15\n\
  \\n\
  \fertilizer-to-water map:\n\
  \49 53 8\n\
  \0 11 42\n\
  \42 0 7\n\
  \57 7 4\n\
  \\n\
  \water-to-light map:\n\
  \88 18 7\n\
  \18 25 70\n\
  \\n\
  \light-to-temperature map:\n\
  \45 77 23\n\
  \81 45 19\n\
  \68 64 13\n\
  \\n\
  \temperature-to-humidity map:\n\
  \0 69 1\n\
  \1 0 69\n\
  \\n\
  \humidity-to-location map:\n\
  \60 56 37\n\
  \56 93 4"

day5T :: IO ()
day5T = hspec $ do
  let (_, pl) = read testCase :: (Seeds, SeedPipeline (Int -> Int))

  describe "the seed pipeline" $ do
    it "should remap seeds to their appropriate locations" $ do
      pipe pl 79 `shouldBe` 82
      pipe pl 14 `shouldBe` 43
      pipe pl 55 `shouldBe` 86
      pipe pl 13 `shouldBe` 35

  describe "the range splitter" $ do
    it "should find suitable ranges for all cases" $ do
      let uri = S.singleton (RangeInfo 6 1 5)
      splitRange uri (0, 6) `shouldBe` S.fromList [(0, 0), (6, 10), (6, 6)]
      splitRange uri (4, 6) `shouldBe` S.fromList [(9, 10), (6, 6)]
      splitRange uri (0, 2) `shouldBe` S.fromList [(0, 0), (6, 7)]
      splitRange uri (2, 4) `shouldBe` S.fromList [(7, 9)]
      let u2  = S.fromList [RangeInfo 11 1 2, RangeInfo 14 4 2]
      splitRange u2 (0, 6) `shouldBe` S.fromList [(0, 0), (11, 12), (3, 3), (14, 15), (6, 6)]

