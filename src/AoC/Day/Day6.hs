module AoC.Day.Day6 where

import AoC.Day.Class
import AoC.Util
import Data.Char
import Text.ParserCombinators.ReadP hiding ( count )

day6 :: Solution
day6 input = solution $ fmap submit
  [ foldr (\ (t, dr) acc -> optimize t dr * acc) 1 $ races $ read input
  , uncurry optimize $ race $ read input
  ]

type Time           = Int
type DistanceRecord = Int
type WaysToWin      = Int

optimize :: Time -> DistanceRecord -> WaysToWin
optimize raceTime record = count canWin [0..raceTime]
  where canWin holdTime = (raceTime - holdTime) * holdTime > record

newtype Races = Races { races :: [(Time, DistanceRecord)] }
newtype SingleRace = SingleRace { race :: (Time, DistanceRecord) }

racesP :: ReadP Races
racesP = do
  _         <- string "Time:"
  skipSpaces
  times     <- sepBy1 intP oneOrMoreSpaces
  skipSpaces
  _         <- string "Distance:"
  skipSpaces
  distances <- sepBy1 intP oneOrMoreSpaces
  return (Races (zip times distances))

singleRaceP :: ReadP SingleRace
singleRaceP = do
  _        <- string "Time:"
  time     <- read <$> many (skipSpaces *> satisfy isDigit)
  skipSpaces
  _        <- string "Distance:"
  distance <- read <$> many (skipSpaces *> satisfy isDigit)
  return (SingleRace (time, distance))

instance Read Races where
  readsPrec _ = readP_to_S racesP

instance Read SingleRace where
  readsPrec _ = readP_to_S singleRaceP
