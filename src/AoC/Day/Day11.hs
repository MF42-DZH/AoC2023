module AoC.Day.Day11 where

import AoC.Day.Class
import AoC.Util
import Control.Monad
import Data.Char
import Data.List
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.String ()

day11 :: Solution
day11 input =
  let galaxies   = expandedMap 1 input
      galaxies1M = expandedMap 999999 input
      paths      = uncurry pathLength <$> triangle galaxies
      paths1M    = uncurry pathLength <$> triangle galaxies1M
  in  solution $ fmap (submit . sum) [paths, paths1M]

triangle :: [a] -> [(a, a)]
triangle []       = []
triangle (x : xs) = ((x,) <$> xs) ++ triangle xs

newtype Galaxy
  = Galaxy { at :: (Int, Int) }
  deriving (Show, Eq, Ord)

type Galaxies = [Galaxy]
type GalaxyMapParser = Parsec String Galaxies

galaxyP :: GalaxyMapParser ()
galaxyP = do
  _ <- char '#'

  p <- getPosition
  let pos = (sourceColumn p - 1, sourceLine p)
  modifyState (Galaxy pos :)

rawMapP :: GalaxyMapParser ()
rawMapP = skipE *> void (many (galaxyP *> skipE))
  where skipE = skipMany (satisfy (\ c -> c == '.' || isSpace c))

expandedMapP :: Int -> GalaxyMapParser Galaxies
expandedMapP mag = rawMapP *> modifyState expandAll *> getState
  where
    expandAll gs
      = fmap Galaxy (expR [maxY, maxY - 1 .. minY] (expC [maxX, maxX - 1 .. minX] ugs))
      where
        ugs = fmap at gs

        (minX, minY) = foldl1' (\ (xi, yi) (xa, ya) -> (min xi xa, min yi ya) ) ugs
        (maxX, maxY) = foldl1' (\ (xi, yi) (xa, ya) -> (max xi xa, max yi ya) ) ugs

        expR [] cs = cs
        expR (y : ys) cs
          | null (filter ((== y) . snd) cs) = expR ys (expandR y cs)
          | otherwise                       = expR ys cs

        expC [] cs = cs
        expC (x : xs) cs
          | null (filter ((== x) . fst) cs) = expC xs (expandC x cs)
          | otherwise                       = expC xs cs

        expandR cy cs = fmap (\ (x, y) -> if y > cy then (x, y + mag) else (x, y)) cs
        expandC cx cs = fmap (\ (x, y) -> if x > cx then (x + mag, y) else (x, y)) cs

expandedMap :: Int -> String -> Galaxies
expandedMap mag input =
  case runP (expandedMapP mag) [] "Galaxies" input of
    Left err -> error (showErrorMessages "Or:" "Unk:" "Exp:" "UnExp:" "EOF" (errorMessages err))
    Right bs -> bs

pathLength :: Galaxy -> Galaxy -> Int
pathLength (Galaxy (x1, y1)) (Galaxy (x2, y2))
  = abs (x2 - x1) + abs (y2 - y1)
