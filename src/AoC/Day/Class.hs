module AoC.Day.Class ( Solution, notImplemented ) where

type Solution = String -> IO String

notImplemented :: Solution
notImplemented = (\ _ -> return "Not implemented yet.")
