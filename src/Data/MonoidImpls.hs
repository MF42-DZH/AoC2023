module Data.MonoidImpls
  ( LCM(..)
  , GCD(..)

  -- Convenience re-exports.
  , module Data.ASemigroup
  ) where

import Control.Monad.Fix
import Control.Monad.Zip
import Data.ASemigroup
import Data.Data
import GHC.Generics

-- Various useful (?) monoid implementations.

newtype LCM a = LCM { getLCM :: a }
  deriving
    ( Eq
    , Ord
    , Show
    , Functor
    , Foldable
    , Traversable
    , Generic
    , Generic1
    , Data
    )

instance Applicative LCM where
  pure                = LCM
  (LCM f) <*> (LCM x) = LCM (f x)

instance Monad LCM where
  return        = pure
  (LCM x) >>= f = f x

instance MonadFix LCM where
  mfix f = LCM (fix (getLCM . f))

instance MonadZip LCM where
  mzip x y = (,) <$> x <*> y

instance Integral a => Semigroup (LCM a) where
  (LCM a) <> (LCM b) = LCM (lcm a b)

instance Integral a => Monoid (LCM a) where
  mappend = (<>)
  mempty  = LCM 1

newtype GCD a = GCD { getGCD :: a }
  deriving
    ( Eq
    , Ord
    , Show
    , Functor
    , Foldable
    , Traversable
    , Generic
    , Generic1
    , Data
    )

instance Applicative GCD where
  pure                = GCD
  (GCD f) <*> (GCD x) = GCD (f x)

instance Monad GCD where
  return        = pure
  (GCD x) >>= f = f x

instance MonadFix GCD where
  mfix f = GCD (fix (getGCD . f))

instance MonadZip GCD where
  mzip x y = (,) <$> x <*> y

instance Integral a => Semigroup (GCD a) where
  (GCD a) <> (GCD b) = GCD (gcd a b)

instance Integral a => ASemigroup (GCD a) where
  azero = GCD 1
