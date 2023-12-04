module AoC.Day.Day4 where

import AoC.Day.Class
import Data.Char
import Data.Map ( Map )
import qualified Data.Map as M
import Data.Set ( Set )
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

day4 :: Solution
day4 input = let allLines = lines input
                 allCards = read <$> allLines
                 scores   = points <$> allCards
                 cmap     = toCopies allCards
                 cmap'    = foldl (flip addCopiesFor) cmap allCards
             in  solution $ fmap (submit . sum) [scores, M.elems cmap']

data ScratchCard
  = ScratchCard { cid :: Int, winning :: Set Int, played :: Set Int }

-- These instances are illegal according to the laws of Eq and Ord,
-- but shouldn't be an issue as cards are constant per run.
instance Eq ScratchCard where
  c1 == c2 = cid c1 == cid c2

instance Ord ScratchCard where
  c1 <= c2 = cid c1 <= cid c2

intsP :: ReadP [Int]
intsP = sepBy1 intP (satisfy isSpace *> skipSpaces)

intP :: ReadP Int
intP = read <$> many1 (satisfy isDigit)

cardP :: ReadP ScratchCard
cardP = do
  _       <- string "Card"
  skipSpaces
  cid     <- intP
  _       <- char ':'
  skipSpaces
  winning <- intsP
  skipSpaces
  _       <- char '|'
  skipSpaces
  played  <- intsP
  return (ScratchCard cid (S.fromList winning) (S.fromList played))

instance Read ScratchCard where
  readsPrec _ = readP_to_S cardP

points :: ScratchCard -> Int
points (ScratchCard _ w p) = case S.size (S.intersection w p) of
  0 -> 0
  s -> 2 ^ (s - 1)

type CardCopies = Map ScratchCard Int

toCopies :: [ScratchCard] -> CardCopies
toCopies = foldr (\ c -> M.insert c 1) M.empty

addCopiesFor :: ScratchCard -> CardCopies -> CardCopies
addCopiesFor card@(ScratchCard idx w p) copies =
  let next     = S.size (S.intersection w p)
      toAdd    = copies M.! card
      dummyC c = ScratchCard c S.empty S.empty
  in  foldr (\ x c -> M.adjust (+ toAdd) (dummyC x) c) copies [idx + 1 .. idx + next]
