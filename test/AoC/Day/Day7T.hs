module AoC.Day.Day7T ( day7T ) where

import AoC.Day.Day7
import Test.Hspec

day7T :: IO ()
day7T = hspec $ do
  describe "the hand parser" $ do
    it "should parse hands and bets properly" $ do
      (read "32T3K 0" :: (Hand, Int)) `shouldSatisfy` handPred Pair
      (read "T55J5 0" :: (Hand, Int)) `shouldSatisfy` handPred ThreeK
      (read "KK677 0" :: (Hand, Int)) `shouldSatisfy` handPred TwoPair
      (read "KTJJT 0" :: (Hand, Int)) `shouldSatisfy` handPred TwoPair
      (read "QQQJA 0" :: (Hand, Int)) `shouldSatisfy` handPred ThreeK
      (read "AAAAA 0" :: (Hand, Int)) `shouldSatisfy` handPred FiveK
      (read "QQAAA 0" :: (Hand, Int)) `shouldSatisfy` handPred FullH
      (read "QAAAA 0" :: (Hand, Int)) `shouldSatisfy` handPred FourK

    -- Via /u/LxsterGames on /r/AdventOfCode.
    it "should handle jokers fine" $ do
      (read "2345A  1" :: HandWithJokers) `shouldSatisfy` handPred' High
      (read "J345A  2" :: HandWithJokers) `shouldSatisfy` handPred' Pair
      (read "2345J  3" :: HandWithJokers) `shouldSatisfy` handPred' Pair
      (read "32T3K  5" :: HandWithJokers) `shouldSatisfy` handPred' Pair
      (read "KK677  7" :: HandWithJokers) `shouldSatisfy` handPred' TwoPair
      (read "T3Q33 11" :: HandWithJokers) `shouldSatisfy` handPred' ThreeK
      (read "Q2KJJ 13" :: HandWithJokers) `shouldSatisfy` handPred' ThreeK
      (read "T3T3J 17" :: HandWithJokers) `shouldSatisfy` handPred' FullH
      (read "Q2Q2Q 19" :: HandWithJokers) `shouldSatisfy` handPred' FullH
      (read "2AAAA 23" :: HandWithJokers) `shouldSatisfy` handPred' FourK
      (read "T55J5 29" :: HandWithJokers) `shouldSatisfy` handPred' FourK
      (read "QQQJA 31" :: HandWithJokers) `shouldSatisfy` handPred' FourK
      (read "KTJJT 34" :: HandWithJokers) `shouldSatisfy` handPred' FourK
      (read "JJJJJ 37" :: HandWithJokers) `shouldSatisfy` handPred' FiveK
      (read "JJJJ2 41" :: HandWithJokers) `shouldSatisfy` handPred' FiveK
      (read "JAAAA 43" :: HandWithJokers) `shouldSatisfy` handPred' FiveK
      (read "2JJJJ 53" :: HandWithJokers) `shouldSatisfy` handPred' FiveK
      (read "AAAAJ 59" :: HandWithJokers) `shouldSatisfy` handPred' FiveK
      (read "AAAAA 61" :: HandWithJokers) `shouldSatisfy` handPred' FiveK
  where
    handPred ctor hb = isHand ctor (fst hb)
    handPred' ctor hb = isHand ctor (fst (hbj hb))
    isHand ctor hand = ctor (cardsInHand hand) == hand
