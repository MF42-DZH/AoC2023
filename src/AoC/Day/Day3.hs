module AoC.Day.Day3 where

import AoC.Day.Class
import Data.Bifunctor
import Data.Char ( isDigit, isSpace )
import Data.Map ( Map )
import qualified Data.Map as M
import Text.Parsec
import Text.Parsec.Error
import Text.Parsec.String ()

day3 :: Solution
day3 input = let possible = findPossible input
                 parts    = findParts possible
                 gears'   = gears possible
             in  solution
               [ submit (sum (fmap number parts))
               , submit (sum (fmap gearRatio gears'))
               ]

type LookState = (SymMap, Nums)
type EngineParserT = ParsecT String LookState

data MPart = MPart { number :: Int, isAdjacent :: (Int, Int) -> Bool }

mPart :: Int -> (Int, Int) -> MPart
mPart n (line, col) = MPart n predicate
  where
    predicate (l, c) = abs (l - line) <= 1 && cc (c - col)
      where cc c' = c' >= -1 && c' <= length (show n)

type SymMap = Map (Int, Int) Char
type Nums   = [MPart]

symP :: Monad m => EngineParserT m ()
symP = do
  p   <- getPosition
  let pos = (sourceLine p, sourceColumn p)
  chr <- satisfy (\ c -> not (isDigit c) && c /= '.')
  modifyState (first (M.insert pos chr))

mPartP :: Monad m => EngineParserT m ()
mPartP = do
  p   <- getPosition
  let pos = (sourceLine p, sourceColumn p)

  num <- many1 (satisfy isDigit)
  modifyState (second (mPart (read num) pos :))

engineP :: Monad m => EngineParserT m LookState
engineP = skipW >> many parts >> getState
  where
    skipW = skipMany (satisfy (\ c -> c == '.' || isSpace c))
    parts = (symP <|> mPartP) >> skipW


findPossible :: String -> LookState
findPossible engine = case runP engineP (M.empty, []) "Engine" engine of
  Left err -> error (showErrorMessages "Or:" "Unk:" "Exp:" "UnExp:" "EOF" (errorMessages err))
  Right ls -> ls

findParts :: LookState -> [MPart]
findParts (syms, nums) = foldr finder [] nums
  where
    finder n acc
      | any (isAdjacent n) (M.keys syms) = n : acc
      | otherwise                        = acc

type Gear = (MPart, MPart)

gearRatio :: Gear -> Int
gearRatio (l, r) = number l * number r

gears :: LookState -> [Gear]
gears (syms, nums) =
  let pgears = M.filter (== '*') syms
  in  foldr gf [] (M.keys pgears)
  where
    gf pos acc = case filter (`isAdjacent` pos) nums of
      [l, r] -> (l, r) : acc
      _      -> acc
