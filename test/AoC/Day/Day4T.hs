module AoC.Day.Day4T ( day4T ) where

import AoC.Day.Day4
import Control.Monad
import Data.Bifunctor
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Hspec

testCases1 :: [(String, Int)]
testCases1 =
  [ ("Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53", 8)
  , ("Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19", 2)
  , ("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1", 2)
  , ("Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83", 1)
  , ("Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36", 0)
  , ("Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11", 0)
  ]

day4T :: IO ()
day4T = hspec $ do
  let parsedCards = fmap (first read) testCases1

  describe "the scratch card parser" $ do
    it "should parse cards correctly" $ do
      let (ScratchCard i w p) = fst (head parsedCards)
      i `shouldBe` 1
      S.member 83 w `shouldBe` True
      S.member 17 p `shouldBe` True
      S.member 69 w `shouldBe` False
      S.member 42 p `shouldBe` False

  describe "the points calculator" $ do
    it "should calculate all scores as a geometric sequence" $ do
      forM_ parsedCards $ \ (card, expPoints) ->
        points card `shouldBe` expPoints
  
  describe "the explosive growth of cards should be tied to winnings" $ do
    it "should only add cards when winning" $ do
      let cards = fmap fst parsedCards
          cmap  = toCopies cards
          cmap' = foldl (flip addCopiesFor) cmap cards
      sum (M.elems cmap') `shouldBe` 30
