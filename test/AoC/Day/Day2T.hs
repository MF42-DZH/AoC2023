module AoC.Day.Day2T ( day2T ) where

import AoC.Day.Day2
import Control.Monad
import Test.Hspec
import Text.Read

testCases :: [(String, Bool, Int)]
testCases =
  [ ("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green", True, 48)
  , ("Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue", True, 12)
  , ("Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red", True, 1560)
  , ("Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red", True, 630)
  , ("Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green", True, 36)
  ]

day2T :: IO ()
day2T = hspec $ do
  describe "the game parser" $ do
    it "needs to parse games correctly" $ do
      case readMaybe ((\ (f, _, _) -> f) (head testCases)) :: Maybe Game of
        Nothing       -> False
        Just (Game (_, g)) -> g ==
          [ Cubes <$> [(3, Blue), (4, Red)]
          , Cubes <$> [(1, Red), (2, Green), (6, Blue)]
          , Cubes <$> [(2, Green)]
          ]
  
  let (lr, lg, lb) = part1limits
  describe "the possibility checker for part 1" $ do
    it "needs to correctly determing that games 3 and 4 are impossible" $ do
      forM_ testCases $ \ (test, p1e, _) ->
        all (isPossible lr lg lb) (gCubes (read test)) `shouldBe` p1e

  describe "the power calculator for part 2" $ do
    it "needs to identify the power of each game's cube sets" $ do
      forM_ testCases $ \ (test, _, p2e) ->
        power (gCubes (read test)) `shouldBe` p2e
