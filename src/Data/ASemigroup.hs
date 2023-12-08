module Data.ASemigroup
  ( ASemigroup(..)

  -- Convenience re-exports.
  , module Data.Semigroup
  ) where

import Data.Monoid
import Data.Semigroup

-- The class of semigroups that have an "absorption" element which
-- consumes any other element when used with <>.
-- This is useful for writing short-circuiting computations.
--
-- Required laws:
--   x <> azero = azero
--   azero <> x = azero
class Semigroup a => ASemigroup a where
  azero :: a

instance ASemigroup All where
  azero = All False

instance ASemigroup Any where
  azero = Any True

instance Num a => ASemigroup (Product a) where
  azero = Product 0

instance (Ord a, Bounded a) => ASemigroup (Min a) where
  azero = Min minBound

instance (Ord a, Bounded a) => ASemigroup (Max a) where
  azero = Max maxBound
