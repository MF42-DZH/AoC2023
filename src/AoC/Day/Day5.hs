{-# LANGUAGE MultiWayIf #-}

module AoC.Day.Day5 where

import AoC.Day.Class
import AoC.Day.Day4 ( intP, intsP )
import Control.Arrow ( (>>>) )
import Control.Monad ( (<=<) )
import Control.Monad.Freer
import Data.Set ( Set )
import qualified Data.Set as S
import Text.ParserCombinators.ReadP

type SetM = Freer Set

runSetM :: Ord a => SetM a -> Set a
runSetM (Freer xs next) = let ss = S.foldr (\ x acc -> next x : acc) [] xs
                          in  S.unions (runSetM <$> ss)
runSetM (Purer x)       = S.fromList [x]

day5 :: Solution
day5 input =
  let ((Seeds seeds), pl1)   = read input :: (Seeds, SeedPipeline (Int -> Int))
      ((SeedRanges rs), pl2) = read input :: (SeedRanges, SeedPipeline (Set RangeInfo))
      localMins2             = runSetM (fst <$> (etaF rs >>= pipeRange pl2))
  in  solution $ fmap submit [minimum $ fmap (pipe pl1) seeds, minimum $ localMins2]

type DestStart   = Int
type SourceStart = Int
type RangeSize   = Int
type Mapped      = Int

data RangeInfo
  = RangeInfo { destStart :: DestStart, sourceStart :: SourceStart, rangeSize :: RangeSize }
  deriving (Eq, Show)

instance Ord RangeInfo where
  compare ri1 ri2 =
    compare (sourceStart ri1) (sourceStart ri2)
    `mappend` compare (destStart ri1, rangeSize ri1) (destStart ri2, rangeSize ri2)

riP :: ReadP RangeInfo
riP = do
  ds <- intP
  skipSpaces
  ss <- intP
  skipSpaces
  rs <- intP
  return (RangeInfo ds ss rs)

composeRI :: [RangeInfo] -> (Int -> Int)
composeRI ris n = case eitherF n of
  Left x  -> x
  Right x -> x
  where
    eitherF = foldr genF Right ris

    genF ri acc =
      acc <=< (remap ri)

    remap (RangeInfo ds ss rs) m
      | m < ss + rs && m >= ss = Left (ds + (m - ss))
      | otherwise              = Right m

splitRange :: Set RangeInfo -> (Seed, Seed) -> Set (Seed, Seed)
splitRange mappings r =
  let result = S.foldl'
        (\ accE ri -> accE >>= remapLoop ri)
        (Right (r, S.empty))
        mappings
  in  either id (uncurry S.insert) result
  where
    remapLoop ri ((f, t), acc) = 
      if | t < from ri -> Left (S.insert (f, t) acc)
         | f > t       -> Left acc
         | f < from ri -> Right ((from ri, t), S.insert (f, min t (from ri - 1)) acc) >>= remapLoop ri
         | f <= to ri  -> Right ((to ri + 1, t), S.insert (f + adj ri, min t (to ri) + adj ri) acc) >>= remapLoop ri
         | f <= t      -> Right ((f, t), acc)
         | otherwise   -> Left acc

    adj  (RangeInfo ds ss _) = ds - ss
    from (RangeInfo _ ss _)  = ss
    to   (RangeInfo _ ss rs) = ss + rs - 1

newtype Unmap = Unmap { unmap :: Int -> [Int] }

composeUnmap :: Set RangeInfo -> Unmap
composeUnmap ris = Unmap $ \ n -> case filter (`contains` n) (S.toList ris) of
  []   -> [n]
  ris' -> fmap (\ ri -> n - adj ri) ris'
  where
    contains ri n = n >= from ri && n <= to ri

    adj  (RangeInfo ds ss _) = ds - ss -- Inverted adjustment.
    from (RangeInfo ds _ _)  = ds
    to   (RangeInfo ds _ rs) = ds + rs - 1

newtype Seeds = Seeds { unSeeds :: [Seed] }

seedsP :: ReadP Seeds
seedsP = Seeds <$> (string "seeds: " *> intsP)

newtype SeedRanges = SeedRanges { unSeedRanges :: Set (Seed, Seed) }

seedRangeP :: ReadP (Seed, Seed)
seedRangeP = do
  from <- intP
  _    <- char ' '
  size <- intP
  return (from, from + size - 1)

seedRangesP :: ReadP SeedRanges
seedRangesP = SeedRanges . S.fromList <$> (string "seeds: " *> sepBy1 seedRangeP (char ' '))

newtype SdtSl a = SdtSl { seedToSoil :: a }
newtype SltF  a = SltF { soilToFert :: a }
newtype FtW   a = FtW { fertToWater :: a }
newtype WtL   a = WtL { waterToLight :: a }
newtype LtT   a = LtT { lightToTemp :: a }
newtype TtH   a = TtH { tempToHumid :: a }
newtype HtL   a = HtL { humidToLoc :: a }

data SeedPipeline a
  = SeedPipeline (SdtSl a) (SltF a) (FtW a) (WtL a) (LtT a) (TtH a) (HtL a)

pipelineP :: ([RangeInfo] -> a) -> ReadP (SeedPipeline a)
pipelineP rangeInfoF = do
  _     <- string "seed-to-soil map:"
  skipSpaces
  sdtsl <- SdtSl . rangeInfoF <$> sepBy1 riP (char '\n')
  skipSpaces
  _    <- string "soil-to-fertilizer map:"
  skipSpaces
  sltf <- SltF . rangeInfoF <$> sepBy1 riP (char '\n')
  skipSpaces
  _   <- string "fertilizer-to-water map:"
  skipSpaces
  ftw <- FtW . rangeInfoF <$> sepBy1 riP (char '\n')
  skipSpaces
  _   <- string "water-to-light map:"
  skipSpaces
  wtl <- WtL . rangeInfoF <$> sepBy1 riP (char '\n')
  skipSpaces
  _   <- string "light-to-temperature map:"
  skipSpaces
  ltt <- LtT . rangeInfoF <$> sepBy1 riP (char '\n')
  skipSpaces
  _   <- string "temperature-to-humidity map:"
  skipSpaces
  tth <- TtH . rangeInfoF <$> sepBy1 riP (char '\n')
  skipSpaces
  _   <- string "humidity-to-location map:"
  skipSpaces
  htl <- HtL . rangeInfoF <$> sepBy1 riP (char '\n')
  return (SeedPipeline sdtsl sltf ftw wtl ltt tth htl)

instance {-# OVERLAPPING #-} Read (Seeds, SeedPipeline (Int -> Int)) where
  readsPrec _ = readP_to_S ((,) <$> seedsP <*> (skipSpaces *> pipelineP composeRI))

instance {-# OVERLAPPING #-} Read (SeedRanges, SeedPipeline (Set RangeInfo)) where
  readsPrec _ = readP_to_S ((,) <$> seedRangesP <*> (skipSpaces *> pipelineP S.fromList))

type Seed     = Int
type Location = Int

pipe :: SeedPipeline (Int -> Int) -> Seed -> Location
pipe (SeedPipeline sdtsl sltf ftw wtl ltt tth htl) =
      seedToSoil sdtsl
  >>> soilToFert sltf
  >>> fertToWater ftw
  >>> waterToLight wtl
  >>> lightToTemp ltt
  >>> tempToHumid tth
  >>> humidToLoc htl

pipeRange :: SeedPipeline (Set RangeInfo) -> (Seed, Seed) -> SetM (Seed, Seed)
pipeRange (SeedPipeline sdtsl sltf ftw wtl ltt tth htl) range = do
  soil  <- etaF (seedToSoil sdtsl `splitRange` range)
  fert  <- etaF (soilToFert sltf `splitRange` soil)
  water <- etaF (fertToWater ftw `splitRange` fert)
  light <- etaF (waterToLight wtl `splitRange` water)
  temp  <- etaF (lightToTemp ltt `splitRange` light)
  hum   <- etaF (tempToHumid tth `splitRange` temp)
  etaF (humidToLoc htl `splitRange` hum)
