module Main ( main ) where

import AoC.Day.Day1T
import AoC.Day.Day2T
import AoC.Day.Day3T
import AoC.Day.Day4T
import AoC.Day.Day5T
import AoC.Day.Day6T
import AoC.Day.Day7T
import AoC.Day.Day8T
import AoC.Day.Day9T
import AoC.Day.Day10T
import AoC.Day.Day11T
import AoC.Day.Day12T
import AoC.Day.Day13T
import AoC.Day.Day14T
import AoC.Day.Day15T
import AoC.Day.Day16T
import AoC.Day.Day17T
import AoC.Day.Day18T
import AoC.Day.Day19T
import AoC.Day.Day20T
import AoC.Day.Day21T
import AoC.Day.Day22T
import AoC.Day.Day23T
import AoC.Day.Day24T
import AoC.Day.Day25T
import Control.Monad.Trans.Class ( lift )
import System.Console.Haskeline
import Text.Read ( readMaybe )

main :: IO ()
main = runInputT defaultSettings getDay
  where
    getDay :: InputT IO ()
    getDay = do
      input <- getInputLine "Enter day number for test (1-25; assuming input is present): "
      case input of
        Nothing -> outputStrLn "No input found." >> getDay
        Just d  -> case readMaybe d :: Maybe Int of
          Nothing  -> outputStrLn "Invalid input. Must be an integer between 1 and 25." >> getDay
          Just day -> lift (specs !! day)

    specs =
      [  day1T,  day2T,  day3T,  day4T,  day5T
      ,  day6T,  day7T,  day8T,  day9T, day10T
      , day11T, day12T, day13T, day14T, day15T
      , day16T, day17T, day18T, day19T, day20T
      , day21T, day22T, day23T, day24T, day25T
      ]
