{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module AoC.Day.Day8 where

import AoC.Day.Class
import AoC.Util ( oneOrMoreSpaces )
import Data.Char ( isAlphaNum )
import Data.Graph ( graphFromEdges, vertices, Graph, Vertex )
import Data.List ( foldl1' )
import Text.ParserCombinators.ReadP

day8 :: Solution
day8 input = let Input (ins, (g, nVF, vFK)) = read input
                 ktMN                       = (nVF <$>) . vFK
                 allAs                      = filter ((== 'A') . last) (sndT . nVF <$> vertices g)
                 ghosts                     = fmap (\ st -> navigateGhost ins st ktMN) allAs
             in  solution $ fmap submit
               [ navigate ins ktMN
               , getLCM (foldl1' (<>) ghosts)
               ]
  where
    sndT (_, s, _) = s

data Instruction
  = L | R
  deriving (Eq, Ord, Show, Enum, Bounded)

instrP :: ReadP [Instruction]
instrP = cycle <$> many1 ctorP
  where
    ctorP = get >>= \ case
      'L' -> return L
      'R' -> return R
      _   -> pfail

type Location = String
type Node     = ()

location :: String -> Location
location = id

node :: Node
node = ()

type DesertMap = (Graph, Vertex -> (Node, Location, [Location]), Location -> Maybe Vertex)

graphP :: ReadP DesertMap
graphP = graphFromEdges <$> sepBy1 vertP oneOrMoreSpaces
  where
    vertP = do
      name  <- many1 (satisfy isAlphaNum)
      skipSpaces
      _     <- char '='
      skipSpaces
      _     <- char '('
      left  <- many1 (satisfy isAlphaNum)
      _     <- char ','
      skipSpaces
      right <- many1 (satisfy isAlphaNum)
      _     <- char ')'
      return (node, location name, [location left, location right])

navigate :: [Instruction] -> (Location -> Maybe (Node, Location, [Location])) -> Integer
navigate ins nodeFromKey = go ins (nodeFromKey "AAA") 0
  where
    go _ Nothing _ = error "Somehow you've ended up outside the map"
    go ~(i : is) ~(Just ((), loc, [left, right])) steps
      | loc == "ZZZ" = steps
      | otherwise    = case i of
        L -> go is (nodeFromKey left) (steps + 1)
        R -> go is (nodeFromKey right) (steps + 1)

navigateGhost :: [Instruction] -> Location -> (Location -> Maybe (Node, Location, [Location])) -> LCM Integer
navigateGhost ins start nodeFromKey = LCM (go ins (nodeFromKey start) 0)
  where
    go _ Nothing _ = error "Somehow you've ended up outside the map"
    go ~(i : is) ~(Just ((), loc, [left, right])) steps
      | last loc == 'Z' = steps
      | otherwise       = case i of
        L -> go is (nodeFromKey left) (steps + 1)
        R -> go is (nodeFromKey right) (steps + 1)

newtype Input = Input { getParsed :: ([Instruction], DesertMap) }

instance Read Input where
  readsPrec _ = readP_to_S (Input <$> ((,) <$> instrP <*> (skipSpaces *> graphP)))

newtype LCM a = LCM { getLCM :: a }
  deriving (Eq, Ord, Show)

instance Integral a => Semigroup (LCM a) where
  (LCM a) <> (LCM b) = LCM (lcm a b)
