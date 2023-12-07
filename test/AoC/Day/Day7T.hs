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
  where
    handPred ctor hb = isHand ctor (fst hb)
    isHand ctor hand = ctor (cardsInHand hand) == hand
