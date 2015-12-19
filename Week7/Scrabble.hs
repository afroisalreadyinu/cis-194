{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import qualified Data.Map as Map
import Data.Char
import Data.Monoid
import Sized

charScores :: Map.Map Char Integer
charScores = Map.fromList $ zip "abcdefghijklmnopqrstuvwxyz" [1,3,3,2,1,4,2,4,1,8,5,1,3,1,1,3,10,1,1,1,1,4,4,8,4,10]

newtype Score = Score Integer
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Integer
getScore (Score s) = s

instance Monoid Score where
    mempty = Score 0
    mappend sc1 sc2 = Score (getScore sc1 + getScore sc2)

instance Sized Score where
    size = Size . fromIntegral . getScore

score :: Char -> Score
score c = case Map.lookup c charScores of
          Just sc -> Score sc
          Nothing -> Score 0

scoreString :: String -> Score
scoreString = sum . (map score) . (map toLower)
