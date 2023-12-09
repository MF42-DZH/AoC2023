module AoC.Day.Day9 where

import AoC.Day.Class
import AoC.Util ( pnIntsP, shead, slast, stail )
import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import Text.ParserCombinators.ReadP

day9 :: Solution
day9 input =
  let allLines = lines input
      allHists = fmap (getHistory . read) allLines
      extended = fmap nthDifferenceExt allHists
  in  solution $ fmap (submit . sum . ($ extended) . fmap) [slast, shead]

nthDifferenceExt :: Seq Int -> Seq Int
-- Pre: There are enough items in the sequence for the order of the polynomial.
--      This is (p + 2) for an pth-order polynomial.
nthDifferenceExt ns
  | all (== 0) differences = shead ns S.<| (ns S.|> slast ns)
  | otherwise              = (shead ns - shead recur) S.<| (ns S.|> (slast ns + slast recur))
  where
    differences = S.zipWith (-) (stail ns) ns
    recur       = nthDifferenceExt differences

newtype History = History { getHistory :: Seq Int }
  deriving (Eq, Ord, Show)

historyP :: ReadP History
historyP = History . S.fromList <$> pnIntsP

instance Read History where
  readsPrec _ = readP_to_S historyP
