module AoC.Day.Impls ( allSolutions ) where

import AoC.Day.Class
import AoC.Day.Day1
import AoC.Day.Day2
import AoC.Day.Day3
import AoC.Day.Day4
import AoC.Day.Day5
import AoC.Day.Day6
import AoC.Day.Day7
import AoC.Day.Day8
import AoC.Day.Day9
import AoC.Day.Day10
import AoC.Day.Day11
import AoC.Day.Day12
import AoC.Day.Day13
import AoC.Day.Day14
import AoC.Day.Day15
import AoC.Day.Day16
import AoC.Day.Day17
import AoC.Day.Day18
import AoC.Day.Day19
import AoC.Day.Day20
import AoC.Day.Day21
import AoC.Day.Day22
import AoC.Day.Day23
import AoC.Day.Day24
import AoC.Day.Day25

allSolutions :: Int -> Solution
allSolutions 1  = day1
allSolutions 2  = day2
allSolutions 3  = day3
allSolutions 4  = day4
allSolutions 5  = day5
allSolutions 6  = day6
allSolutions 7  = day7
allSolutions 8  = day8
allSolutions 9  = day9
allSolutions 10 = day10
allSolutions 11 = day11
allSolutions 12 = day12
allSolutions 13 = day13
allSolutions 14 = day14
allSolutions 15 = day15
allSolutions 16 = day16
allSolutions 17 = day17
allSolutions 18 = day18
allSolutions 19 = day19
allSolutions 20 = day20
allSolutions 21 = day21
allSolutions 22 = day22
allSolutions 23 = day23
allSolutions 24 = day24
allSolutions 25 = day25
allSolutions _  = (\ _ -> return "Please input a valid day (1-25).")
