module Data.Remainder
  ( ModEq
  , modEq
  , modEq'
  , solveTwo
  , solveTwo'
  , solveMany
  , solveMany'
  ) where

-- Implementation of Chinese Remainder Theorem for solving systems of modular
-- equations (x === a_i (mod n_i) for some range [1..i]).
-- This also assumes all n_i are pairwise co-prime.
-- It is recommended that you use Integer.

import Data.List ( foldl' )
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Semigroup

data ModEq a = ModEq { mvalue :: a, mmodulus :: a }
  deriving (Eq, Ord)

modEq :: Integral a => a -> a -> ModEq a
modEq value modulus =
  case modEq' value modulus of
    Nothing -> error "modEq: zero modulus"
    Just me -> me

modEq' :: Integral a => a -> a -> Maybe (ModEq a)
modEq' value modulus
  | modulus == 0 = Nothing
  | otherwise    = Just (ModEq value modulus)

instance Show a => Show (ModEq a) where
  show me = concat ["x === ", show (mvalue me), " (mod ", show (mmodulus me) ,")"]

solveTwo :: Integral a => ModEq a -> ModEq a -> a
solveTwo (ModEq a1 n1) (ModEq a2 n2) =
  let (m1, m2) = bezout n1 n2
  in  a1 * m2 * n2 + a2 * m1 * n1

solveTwo' :: Integral a => ModEq a -> ModEq a -> ModEq a
solveTwo' (ModEq a1 n1) (ModEq a2 n2) =
  let (m1, m2) = bezout n1 n2
      prod     = n1 * n2
  in  ModEq ((a1 * m2 * n2 + a2 * m1 * n1) `mod` prod) prod

solveMany :: Integral a => NonEmpty (ModEq a) -> a
solveMany = mvalue . solveMany'

solveMany' :: Integral a => NonEmpty (ModEq a) -> ModEq a
solveMany' (f :| r) = foldl' solveTwo' f r

instance Integral a => Semigroup (ModEq a) where
  (<>)    = solveTwo'
  sconcat = solveMany'

instance Integral a => Monoid (ModEq a) where
  mappend = (<>)
  mempty  = ModEq 0 1

bezout :: Integral a => a -> a -> (a, a)
bezout _ 0 = (1, 0)
bezout a b =
  let (q, r) = a `divMod` b
      (u, v) = bezout b r
  in  (v, u - (q * v))
