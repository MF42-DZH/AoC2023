module AoC.Day.Day8T ( day8T ) where

import AoC.Day.Day8
import Data.MonoidImpls
import Test.Hspec

testCase1 :: String
testCase1 =
  "RL\n\
  \\n\
  \AAA = (BBB, CCC)\n\
  \BBB = (DDD, EEE)\n\
  \CCC = (ZZZ, GGG)\n\
  \DDD = (DDD, DDD)\n\
  \EEE = (EEE, EEE)\n\
  \GGG = (GGG, GGG)\n\
  \ZZZ = (ZZZ, ZZZ)"

testCase2 :: String
testCase2 =
  "LLR\n\
  \\n\
  \AAA = (BBB, BBB)\n\
  \BBB = (AAA, ZZZ)\n\
  \ZZZ = (ZZZ, ZZZ)"

testCaseGhost :: String
testCaseGhost =
  "LR\n\
  \\n\
  \11A = (11B, XXX)\n\
  \11B = (XXX, 11Z)\n\
  \11Z = (11B, XXX)\n\
  \22A = (22B, XXX)\n\
  \22B = (22C, 22C)\n\
  \22C = (22Z, 22Z)\n\
  \22Z = (22B, 22B)\n\
  \XXX = (XXX, XXX)"

day8T :: IO ()
day8T = hspec $ do
  let Input (i1, (_, nFV1, vFK1)) = read testCase1
      Input (i2, (_, nFV2, vFK2)) = read testCase2
      Input (iG, (_, nFVG, vFKG)) = read testCaseGhost
      ktMN1                       = (nFV1 <$>) . vFK1
      ktMN2                       = (nFV2 <$>) . vFK2
      ktMNG                       = (nFVG <$>) . vFKG

  describe "the instruction and graph psrser" $ do
    it "should parse a cyclic list of instructions" $ do
      take 4 i1 `shouldBe` [R, L, R, L]
      take 6 i2 `shouldBe` [L, L, R, L, L, R]
    
    it "should parse a directed graph with the appropriate nodes connected" $ do
      ktMN1 "AAA" `shouldBe` Just ((), "AAA", ["BBB", "CCC"])
      ktMN1 "CCC" `shouldBe` Just ((), "CCC", ["ZZZ", "GGG"])
      ktMN1 "ZZZ" `shouldBe` Just ((), "ZZZ", ["ZZZ", "ZZZ"])

      ktMN2 "AAA" `shouldBe` Just ((), "AAA", ["BBB", "BBB"])
      ktMN2 "BBB" `shouldBe` Just ((), "BBB", ["AAA", "ZZZ"])
      ktMN2 "ZZZ" `shouldBe` Just ((), "ZZZ", ["ZZZ", "ZZZ"])

  describe "the navigator" $ do
    it "should return the correct amount of steps from AAA to ZZZ" $ do
      navigate i1 ktMN1 `shouldBe` 2
      navigate i2 ktMN2 `shouldBe` 6
  
  describe "ghost navigation" $ do
    it "should return the LCM of two ghosts navigating from all -A nodes" $ do
      let lcm11A = navigateGhost iG "11A" ktMNG
          lcm22A = navigateGhost iG "22A" ktMNG
      
      getLCM lcm11A `shouldBe` 2
      getLCM lcm22A `shouldBe` 3

      getLCM (lcm11A <> lcm22A) `shouldBe` 6
