module AoC.Day.Day7 where

import AoC.Day.Class
import AoC.Util
import Control.Monad ( replicateM )
import Data.Bifunctor
import Data.Char
import Data.List ( sortOn )
import qualified Data.Map as M
import Data.Maybe ( fromMaybe )
import Text.ParserCombinators.ReadP

day7 :: Solution
day7 input =
  let njGame = ranksValue $ nj $ read input
      jGame  = ranksValue $ gwj $ read input
      toIntg = fromIntegral :: Int -> Integer
  in  solution $ fmap (submit . sum . fmap (uncurry (*)) . fmap (bimap (toIntg . snd) toIntg))
    [njGame, jGame]

data Card
  = Joker
  | Value Int
  | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord)

instance Show Card where
  show Joker     = "J"
  show (Value x) = show x
  show Ten       = "T"
  show Jack      = "J"
  show Queen     = "Q"
  show King      = "K"
  show Ace       = "A"

value :: Int -> Card
value x
  | x >= 2 && x <= 9 = Value x
  | otherwise        = error "Invalid card value."

data Hand
  = High [Card]
  | Pair [Card]
  | TwoPair [Card]
  | ThreeK [Card]
  | FullH [Card]
  | FourK [Card]
  | FiveK [Card]
  deriving (Eq, Ord)

instance Show Hand where
  show (High cs)    = " H: " ++ concat (fmap show cs)
  show (Pair cs)    = " P: " ++ concat (fmap show cs)
  show (TwoPair cs) = "TP: " ++ concat (fmap show cs)
  show (ThreeK cs)  = "3K: " ++ concat (fmap show cs)
  show (FullH cs)   = "FH: " ++ concat (fmap show cs)
  show (FourK cs)   = "4K: " ++ concat (fmap show cs)
  show (FiveK cs)   = "5K: " ++ concat (fmap show cs)

cardsInHand :: Hand -> [Card]
cardsInHand (High cs)    = cs
cardsInHand (Pair cs)    = cs
cardsInHand (TwoPair cs) = cs
cardsInHand (ThreeK cs)  = cs
cardsInHand (FullH cs)   = cs
cardsInHand (FourK cs)   = cs
cardsInHand (FiveK cs)   = cs

cardP :: Bool -> ReadP Card
cardP jokers = do
  c <- get
  case c of
    x | isDigit x -> return (value (read [x]))
    'T'           -> return Ten
    'J'           -> return (if jokers then Joker else Jack)
    'Q'           -> return Queen
    'K'           -> return King
    'A'           -> return Ace
    _             -> pfail

{-

Jokers can upgrade hands. Differing amounts of jokers can upgrade differently.

1 Joker:
  High            -> Pair
  Pair            -> Three-of-a-Kind
  Two Pair        -> Full House
  Three-of-a-Kind -> Four-of-a-Kind
  Full House      -> Four-of-a-Kind
  Four-of-a-Kind  -> Five-of-a-Kind
2 Jokers:
  Pair            -> Three-of-a-kind (The jokers form the pair itself.)
  Two Pair        -> Four-of-a-Kind (The 2nd pair must be a pair of jokers.)
  Three-of-a-Kind -> Five-of-a-Kind
  Full House      -> Five-of-a-Kind
3 Jokers:
  Three-of-a-Kind -> Four-of-a-Kind (The three are jokers to start with.)
  Full House      -> Five-of-a-Kind (See above.)
4 Jokers:
  Four-of-a-Kind -> Five-of-a-Kind
5 Jokers:
  (Nothing changes!)

 -}

handBetP :: Bool -> ReadP (Hand, Int)
handBetP jokers = do
  cards <- replicateM 5 (cardP jokers)
  skipSpaces
  bet   <- intP

  let cardM = foldr (\ c -> mergeJ c (+ 1) (1 :: Int)) M.empty cards
  return $ (, bet) $ upgrade jokers (fromMaybe 0 (cardM M.!? Joker)) $ case M.size cardM of
    1 -> FiveK cards
    2 -> if   4 `elem` M.elems cardM
         then FourK cards
         else FullH cards
    3 -> if   3 `elem` M.elems cardM
         then ThreeK cards
         else TwoPair cards
    4 -> Pair cards
    5 -> High cards
    _ -> error ("What kind of hand is this? " ++ show cards)
  where
    how j hand = error $ concat
      [ "How'd you get here? ("
      , show (j :: Int)
      , "; "
      , show hand
      , ")"]

    -- There's probably something easier than manually encoding the upgrades lmao.
    upgrade False _ hand = hand
    upgrade True 0 hand  = hand
    upgrade True 1 hand  = case hand of
      High c    -> Pair c
      Pair c    -> ThreeK c
      TwoPair c -> FullH c
      ThreeK c  -> FourK c
      FullH c   -> FourK c
      FourK c   -> FiveK c
      _         -> how 1 hand
    upgrade True 2 hand  = case hand of
      Pair c    -> ThreeK c
      TwoPair c -> FourK c
      ThreeK c  -> FiveK c
      FullH c   -> FiveK c
      _         -> how 2 hand
    upgrade True 3 hand  = case hand of
      ThreeK c -> FourK c
      FullH c  -> FiveK c
      _        -> how 3 hand
    upgrade True 4 hand  = case hand of
      FourK c -> FiveK c
      _       -> how 4 hand
    upgrade True 5 hand  = hand
    upgrade _ _ hand     = how (-1) hand

newtype HandWithJokers = HandWithJokers { hbj :: (Hand, Int) }

instance {-# OVERLAPPING #-} Read (Hand, Int) where
  readsPrec _ = readP_to_S (handBetP False)

instance Read HandWithJokers where
  readsPrec _ = readP_to_S (HandWithJokers <$> handBetP True)

newtype NoJokers = NoJokers { nj :: [(Hand, Int)] }
newtype GameWithJokers = GameWithJokers { gwj :: [(Hand, Int)] }

instance Read NoJokers where
  readsPrec _ = readP_to_S (NoJokers <$> sepBy1 (handBetP False) oneOrMoreSpaces)

instance Read GameWithJokers where
  readsPrec _ = readP_to_S (GameWithJokers <$> sepBy1 (handBetP True) oneOrMoreSpaces)

ranksValue :: [(Hand, Int)] -> [((Hand, Int), Int)]
ranksValue hands = zip (sortOn fst hands) [1..]
