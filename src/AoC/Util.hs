module AoC.Util where

import Data.Map ( Map )
import qualified Data.Map as M
import System.Directory ( doesFileExist )

loadInput :: Int -> IO String
loadInput day = do
  let fname = ("input/day" ++ show day ++ ".txt")
  exists <- doesFileExist fname

  if   exists
  then readFile fname
  else return ""

mergeJ :: Ord k => k -> (a -> a) -> a -> Map k a -> Map k a
mergeJ k f def = M.alter mf k
  where mf Nothing  = Just def
        mf (Just x) = Just (f x)
