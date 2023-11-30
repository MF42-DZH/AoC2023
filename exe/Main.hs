module Main where

import AoC.Day.Impls ( allSolutions )
import AoC.Util ( loadInput )
import Control.Monad.Trans.Class ( lift )
import System.Console.Haskeline
import Text.Read ( readMaybe )

main :: IO ()
main = runInputT defaultSettings getDay
  where
    getDay :: InputT IO ()
    getDay = do
      input <- getInputLine "Enter day number for solution (1-25; assuming input is present): "
      case input of
        Nothing -> outputStrLn "No input found." >> getDay
        Just d  -> case readMaybe d :: Maybe Int of
          Nothing  -> outputStrLn "Invalid input. Must be an integer between 1 and 25." >> getDay
          Just day -> lift (allSolutions day =<< loadInput day) >>= outputStrLn
