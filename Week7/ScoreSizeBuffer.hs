{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module ScoreSizeBuffer where

import Data.Monoid
import Buffer
import JoinList
import Scrabble
import Sized

instance Buffer (JoinList (Score, Size) String) where
  toString       = concat . jlToList
  fromString line = Single ((scoreString line), Size(length line)) line
  line           = indexJ
  replaceLine n l b = (takeJ (n - 1) b) +++ fromString l +++ (dropJ n b)
  numLines = getSize . snd . tag
  value        = fromIntegral . getScore . fst . tag

doubleScoreLine :: String -> JoinList (Score, Size) String
doubleScoreLine line = Single ((scoreString line), Size(length line)) line
