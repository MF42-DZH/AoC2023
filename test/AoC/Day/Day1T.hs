module AoC.Day.Day1T ( day1T ) where

import AoC.Day.Day1
import Control.Monad ( forM_ )
import Test.Hspec

testCases1 :: [(String, Int)]
testCases1 =
  [ ("1abc2", 12)
  , ("pqr3stu8vwx", 38)
  , ("a1b2c3d4e5f", 15)
  , ("treb7uchet", 77)
  ]

testCases2 :: [(String, Int)]
testCases2 =
  [ ("two1nine", 29)
  , ("eightwothree", 83)
  , ("abcone2threexyz", 13)
  , ("xtwone3four", 24)
  , ("4nineeightseven2", 42)
  , ("zoneight234", 14)
  , ("7pqrstsixteen", 76)
  , ("oneight", 18)
  , ("eightwo", 82)
  , ("eighthree", 83)
  ]

day1T :: IO ()
day1T = hspec $ do
  describe "the digit finder" $ do
    it "can find the first and last digits and combine them" $ do
      forM_ testCases1 $ \ (text, expec) -> do
        process1 False [text] `shouldBe` expec

    it "do the same with worded digits" $ do
      forM_ testCases2 $ \ (text, expec) -> do
        process1 True [text] `shouldBe` expec
