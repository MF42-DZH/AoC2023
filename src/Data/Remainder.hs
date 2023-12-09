module Data.Remainder
  ( ModEq( mvalue, mmodulus )
  , modEq
  , modEq'
  , solveTwo
  , solveTwo'
  , solveTwoE
  , solveMany
  , solveMany'
  , solveManyE
  ) where

-- Implementation of Chinese Remainder Theorem for solving systems of modular
-- equations (x === a_i (mod n_i) for some range [1..i]).
-- It is recommended that you use Integer as that is lossless for the modulus.

import Data.List ( foldl' )
import Data.List.NonEmpty ( NonEmpty( (:|) ))
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
  | otherwise    = Just (ModEq (value `mod` modulus) modulus)

instance Show a => Show (ModEq a) where
  show me = concat ["x === ", show (mvalue me), " (mod ", show (mmodulus me) ,")"]

solveTwo :: Integral a => ModEq a -> ModEq a -> a
solveTwo e1 e2 = mvalue (solveTwo' e1 e2)

solveTwo' :: Integral a => ModEq a -> ModEq a -> ModEq a
solveTwo' e1 e2 =
  case solveTwoE e1 e2 of
    Left e  -> error e
    Right x -> x

solveTwoE :: Integral a => ModEq a -> ModEq a -> Either String (ModEq a)
solveTwoE (ModEq a1 n1) (ModEq a2 n2)
  | not (coprime n1 n2) && incongruent
    = Left "solveTwoE: the two moduli are not coprime, and their values are not congruent under the moduli"
  | otherwise
    = let (m1, m2) = bezout (basis n1) (basis n2)
          prod     = lcm n1 n2
      in  Right (ModEq ((a1 * m2 * (basis n2) + a2 * m1 * (basis n1)) `mod` prod) prod)
  where
    g           = gcd n1 n2
    incongruent = (a1 `mod` g) /= (a2 `mod` g)
    basis n     = n `div` g

solveMany :: Integral a => [ModEq a] -> a
solveMany = mvalue . solveMany'

solveMany' :: Integral a => [ModEq a] -> ModEq a
solveMany' eqs =
  case solveManyE eqs of
    Left e  -> error e
    Right x -> x

solveManyE :: Integral a => [ModEq a] -> Either String (ModEq a)
solveManyE= foldl' (\ acc x -> acc >>= (`solveTwoE` x)) (Right mempty)

instance Integral a => Semigroup (ModEq a) where
  (<>)              = solveTwo'
  sconcat (x :| xs) = solveMany' (x : xs)

instance Integral a => Monoid (ModEq a) where
  mappend = (<>)
  mempty  = ModEq 0 1

bezout :: Integral a => a -> a -> (a, a)
bezout _ 0 = (1, 0)
bezout a b =
  let (q, r) = a `divMod` b
      (u, v) = bezout b r
  in  (v, u - (q * v))

coprime :: Integral a => a -> a -> Bool
coprime a b = gcd a b == 1
