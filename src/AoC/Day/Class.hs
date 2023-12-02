{-# LANGUAGE GADTs #-}

module AoC.Day.Class ( Solution, notImplemented, solution, solutionRaw, submit ) where

type Solution = String -> IO SolutionOf

data Showable where
  Showable :: forall a . Show a => a -> Showable

instance Show Showable where
  show (Showable s) = show s

submit :: forall a . Show a => a -> Showable
submit = Showable

data SolutionOf where
  RawString :: String -> SolutionOf
  Parts     :: [Showable] -> SolutionOf

notImplemented :: Solution
notImplemented = (\ _ -> solutionRaw "Not implemented yet.")

solution :: [Showable] -> IO SolutionOf
solution = return . Parts

solutionRaw :: String -> IO SolutionOf
solutionRaw = return . RawString

instance Show SolutionOf where
  show (RawString s) = s
  show (Parts s)     = unlines (show <$> s)
