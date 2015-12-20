{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dies :: Int -> Rand StdGen [DieValue]
dies n = fmap (take n) getRandoms

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving Show

-- (attackers, defenders) -> attackerDies -> defenderDies -> (newA, newD)
updateField :: (Int, Int) -> [DieValue] -> [DieValue] -> (Int, Int)
updateField (0, x) _ _ = (0, x)
updateField (x, 0) _ _ = (x, 0)
updateField (x, y) [] _ = (x, y)
updateField (x, y) _ [] = (x, y)
updateField (x, y) (ad:ads) (dd:dds) =
    if (ad > dd) then (updateField (x, y-1) ads dds)
    else (updateField (x-1, y) ads dds)


battle :: Battlefield -> Rand StdGen Battlefield
battle field = do
  let numAttacks = min ((attackers field) - 1) 3
      numDefenders = min (defenders field) 2
  attackerDies <- dies numAttacks
  defenderDies <- dies numDefenders
  let newstate = updateField (numAttacks, numDefenders) attackerDies defenderDies
  return $ Battlefield ((attackers field) + (fst newstate) - numAttacks)
             ((defenders field) + (snd newstate) - numDefenders)

invade :: Battlefield -> Rand StdGen Battlefield
invade field =
    if ((attackers field) < 2) || ((defenders field) == 0) then return field
    else battle field >>= invade

winIndex :: Battlefield -> Int
winIndex field = if (attackers field) > 1 then 1 else 0

data RandomTrialState = RTS { field :: Battlefield,
                              count :: Int,
                              attackerWins :: Int } deriving Show


successProb :: Rand StdGen Double
successProb = let runs = replicateM 1000 (invade (Battlefield 3 3))
              in fmap ((/ 1000.0) . fromIntegral . sum . (map winIndex)) runs
