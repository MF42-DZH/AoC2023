module Main ( main ) where

import AoC.Day.Day1T ( day1T )
import AoC.Day.Day2T ( day2T )
import AoC.Day.Day3T ( day3T )
import AoC.Day.Day4T ( day4T )
import AoC.Day.Day5T ( day5T )
import AoC.Day.Day6T ( day6T )
import AoC.Day.Day7T ( day7T )
import AoC.Day.Day8T ( day8T )
import AoC.Day.Day9T ( day9T )
import AoC.Day.Day10T ( day10T )
import AoC.Day.Day11T ( day11T )
import AoC.Day.Day12T ( day12T )
import AoC.Day.Day13T ( day13T )
import AoC.Day.Day14T ( day14T )
import AoC.Day.Day15T ( day15T )
import AoC.Day.Day16T ( day16T )
import AoC.Day.Day17T ( day17T )
import AoC.Day.Day18T ( day18T )
import AoC.Day.Day19T ( day19T )
import AoC.Day.Day20T ( day20T )
import AoC.Day.Day21T ( day21T )
import AoC.Day.Day22T ( day22T )
import AoC.Day.Day23T ( day23T )
import AoC.Day.Day24T ( day24T )
import AoC.Day.Day25T ( day25T )
import Control.Monad ( forM_ )
import Control.Monad.Trans.Class ( lift )
import System.Console.Haskeline
import System.Environment
import Text.Read ( readMaybe )

-- Writing tests:
-- https://hspec.github.io/writing-specs.html

main :: IO ()
main = runInputT defaultSettings $ do
  args <- lift (lookupEnv "CONTINUOUS")
  case args of
    Just _  -> forM_ specs lift
    Nothing -> getDay
  where
    getDay :: InputT IO ()
    getDay = do
      input <- getInputLine "Enter day number for test (1-25): "
      case input of
        Nothing -> outputStrLn "No input found." >> getDay
        Just d  -> case readMaybe d :: Maybe Int of
          Nothing  -> outputStrLn "Invalid input. Must be an integer between 1 and 25." >> getDay
          Just day -> lift (specs !! (day - 1))

    specs =
      [  day1T,  day2T,  day3T,  day4T,  day5T
      ,  day6T,  day7T,  day8T,  day9T, day10T
      , day11T, day12T, day13T, day14T, day15T
      , day16T, day17T, day18T, day19T, day20T
      , day21T, day22T, day23T, day24T, day25T
      ]
