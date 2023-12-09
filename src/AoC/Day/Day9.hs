module AoC.Day.Day9 where

import AoC.Day.Class
import AoC.Util ( pnIntsP )
import Data.Sequence ( Seq )
import qualified Data.Sequence as S
import Text.ParserCombinators.ReadP

day9 :: Solution
day9 input =
  let allLines = lines input
      allHists = fmap (getHistory . read) allLines
      extended = fmap nthDifferenceExt allHists
  in  solution $ fmap submit [sum (fmap slast extended), sum (fmap shead extended)]

nthDifferenceExt :: Seq Int -> Seq Int
-- Pre: There is at least 1 item in the sequence.
nthDifferenceExt ns
  | all (== 0) differences = shead ns S.<| (ns S.|> slast ns)
  | otherwise              = (shead ns - shead recur) S.<| (ns S.|> (slast ns + slast recur))
  where
    differences = S.zipWith (-) (stail ns) ns
    recur       = nthDifferenceExt differences

stail :: Seq a -> Seq a
stail S.Empty      = error "Empty Seq."
stail (_ S.:<| xs) = xs

shead :: Seq a -> a
shead S.Empty     = error "Empty Seq."
shead (x S.:<| _) = x

slast :: Seq a -> a
slast S.Empty     = error "Empty Seq."
slast (_ S.:|> x) = x

newtype History = History { getHistory :: Seq Int }
  deriving (Eq, Ord, Show)

historyP :: ReadP History
historyP = History . S.fromList <$> pnIntsP

instance Read History where
  readsPrec _ = readP_to_S historyP
