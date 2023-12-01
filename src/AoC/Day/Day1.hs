module AoC.Day.Day1 where

import AoC.Day.Class
import Data.List ( find, isPrefixOf )
import Data.Map ( Map )
import qualified Data.Map as M

day1 :: Solution
day1 input =
  let allLines  = lines input
      digitsIn1 = process1 False allLines
      digitsIn2 = process1 True allLines
  in  do
    return $ unlines $ fmap show [digitsIn1, digitsIn2]

fnl :: [a] -> [a]
fnl l = head l : [last l]

process1 :: Bool -> [String] -> Int
process1 p2 = sum  . fmap (concatInt . fnl . digits p2)

concatInt :: [Int] -> Int
concatInt = foldl (\ a x -> a * 10 + x) 0

digits :: Bool -> String -> [Int]
digits _ ""            = []
digits p2 xxs@(_ : xs) = case find (\ (word, _) -> word `isPrefixOf` xxs) (M.assocs um) of
  Nothing     -> digits p2 xs
  Just (_, v) -> v : digits p2 xs
  where
    um = usedMap p2

usedMap :: Bool -> Map String Int
usedMap False = digitMap
usedMap True  = M.union remapping digitMap

digitMap :: Map String Int
digitMap = M.fromList [(show x, x) | x <- [1..9]]

remapping :: Map String Int
remapping = M.fromList
  [ ("one", 1)
  , ("two", 2)
  , ("three", 3)
  , ("four", 4)
  , ("five", 5)
  , ("six", 6)
  , ("seven", 7)
  , ("eight", 8)
  , ("nine", 9)
  ]
