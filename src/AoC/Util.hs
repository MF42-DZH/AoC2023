{-# LANGUAGE LambdaCase #-}

module AoC.Util where

import Control.Concurrent
  ( ThreadId
  , forkFinally
  , newEmptyMVar
  , putMVar
  , takeMVar
  , MVar
  )
import Control.Monad ( void )
import Data.Char
import Data.Map ( Map )
import qualified Data.Map as M
import Debug.Trace
import GHC.Exception ( throw )
import System.Directory ( doesFileExist )
import Text.ParserCombinators.ReadP

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

newtype JoinHandle a = JH (ThreadId, MVar a)

-- WARNING: In GHC, holding onto the ThreadID after a thread has finished
--          running causes a memory leak.
forkJoinable :: IO a -> IO (JoinHandle a)
forkJoinable action = do
  lock <- newEmptyMVar :: IO (MVar a)
  tid  <- forkFinally action (\ case
    Right x -> putMVar lock x
    Left e  -> throw e)

  return $ JH (tid, lock)

joinHandle :: JoinHandle a -> IO a
joinHandle (JH (_, lock)) = takeMVar lock

joinHandle_ :: JoinHandle a -> IO ()
joinHandle_ = void . joinHandle

joinTid :: JoinHandle a -> ThreadId
joinTid (JH (tid, _)) = tid

count :: Foldable f => (a -> Bool) -> f a -> Int
count predicate = foldr (\ x c -> if predicate x then c + 1 else c) 0

oneOrMoreSpaces :: ReadP ()
oneOrMoreSpaces = satisfy isSpace *> skipSpaces

intsP :: ReadP [Int]
intsP = sepBy1 intP (satisfy isSpace *> skipSpaces)

intP :: ReadP Int
intP = read <$> many1 (satisfy isDigit)

trc :: Show a => a -> a
trc x = trace (show x) x

trcf :: Show a => [a] -> [a]
trcf xs = trace (unlines (fmap show xs)) xs
