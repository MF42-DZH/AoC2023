module AoC.Day.Day2 where

import AoC.Day.Class
import AoC.Util
import Text.ParserCombinators.ReadP

day2 :: Solution
day2 input = let allLines        = lines input
                 allGames        = read <$> allLines :: [Game]
                 (lr1, lg1, lb1) = part1limits
                 p1Possible      = filter (\ g -> all (isPossible lr1 lg1 lb1) (gCubes g)) allGames
                 p1Sum           = sum (gId <$> p1Possible)
                 p2Powers        = (power . gCubes) <$> allGames
                 p2Sum           = sum p2Powers
             in  solution $ fmap submit [p1Sum, p2Sum]

part1limits :: (Int, Int, Int)
part1limits = (12, 13, 14)

isPossible :: Int -> Int -> Int -> [Cubes] -> Bool
isPossible reds greens blues cubes =
  case cubes of
    []                      -> True
    (Cubes (r, Red) : cs)   -> (r <= reds) && isPossible reds greens blues cs
    (Cubes (g, Green) : cs) -> (g <= greens) && isPossible reds greens blues cs
    (Cubes (b, Blue) : cs)  -> (b <= blues) && isPossible reds greens blues cs

minPossible :: [[Cubes]] -> (Int, Int, Int)
minPossible [] = (0, 0, 0)
minPossible (cs : css) =
  let ms'               = minPossible css
      mins [] ms        = ms
      mins (c : cs') ms =
        let (r', g', b') = mins cs' ms
        in  case c of
          Cubes (r, Red)   -> (max r r', g', b')
          Cubes (g, Green) -> (r', max g g', b')
          Cubes (b, Blue)  -> (r', g', max b b')
  in  mins cs ms'

power :: [[Cubes]] -> Int
power = (\ (r, g, b) -> r * g * b) . minPossible

data Colour = Red | Green | Blue
  deriving (Eq, Ord, Enum, Show, Bounded)

newtype Cubes = Cubes { unCube :: (Int, Colour) }
  deriving (Eq, Show)

colour :: Cubes -> Colour
colour = snd . unCube

amount :: Cubes -> Int
amount = fst . unCube

newtype Game = Game { unGame :: (Int, [[Cubes]]) }
  deriving Eq

gId :: Game -> Int
gId = fst . unGame

gCubes :: Game -> [[Cubes]]
gCubes = snd . unGame

instance Read Game where
  readsPrec _ = readP_to_S gameP

gameP :: ReadP Game
gameP = Game <$> do
  _   <- string "Game"
  skipSpaces
  gid <- intP
  _   <- char ':'
  skipSpaces
  (gid,) <$> sepBy cubesSetP (string "; ")

cubesSetP :: ReadP [Cubes]
cubesSetP = sepBy cubesP (string ", ")

cubesP :: ReadP Cubes
cubesP = Cubes <$> ((,) <$> (intP <* skipSpaces) <*> colourP)

colourP :: ReadP Colour
colourP = choice [Red <$ string "red", Green <$ string "green", Blue <$ string "blue"]
