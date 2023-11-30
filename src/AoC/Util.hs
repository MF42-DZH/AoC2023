module AoC.Util where

import System.Directory ( doesFileExist )

loadInput :: Int -> IO String
loadInput day = do
  let fname = ("input/day" ++ show day ++ ".txt")
  exists <- doesFileExist fname

  if   exists
  then readFile fname
  else return ""
