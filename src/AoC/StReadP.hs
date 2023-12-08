-- Stateful ReadP, when Parsec is overkill for a parsing job,
-- but stateful parsers are needed.

module AoC.StReadP
  ( StReadP
  , readEitherSt
  , readSt

  -- Convenience re-exports.
  , module AoC.Has
  , module Control.Monad.Trans.Class
  , module Text.ParserCombinators.ReadP
  ) where

import AoC.Has
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Lazy ( StateT(runStateT) )
import Text.ParserCombinators.ReadP
import qualified Text.ParserCombinators.ReadPrec as RPC

type StReadP s = StateT s ReadP

readEitherSt :: StReadP s a -> s -> String -> Either String (a, s)
readEitherSt stPar initialState str =
  case [ x | (x, "") <- RPC.readPrec_to_S parser RPC.minPrec str ] of
    [x] -> Right x
    []  -> Left "readSt: no parse"
    _   -> Left "readSt: ambiguous parse"
  where
    rpp    = RPC.lift (runStateT stPar initialState)
    parser = do
      x <- rpp
      RPC.lift skipSpaces
      return x

readSt :: StReadP s a -> s -> String -> (a, s)
readSt stPar initialState str =
  case readEitherSt stPar initialState str of
    Right x -> x
    Left e  -> error e
