module JoinList where

import Data.Monoid
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                    deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList (Single m a) = [a]
jlToList (Append _ left right) = jlToList left ++ jlToList right

tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Append m _ _) = m

sizedTag :: (Sized m, Monoid m) => JoinList m a -> Int
sizedTag = getSize . size . tag

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single jlSize val) | i == 0 && (size jlSize) == 1 = Just val
                             | otherwise = Nothing

indexJ i jl@(Append jlSize left right)
    | i >= sizedTag jl = Nothing
    | i >= sizedTag left = indexJ (i - sizedTag left) right
    | otherwise = indexJ i left

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ i jl@(Append jlSize left right)
    | i == sizedTag left = right
    | i > sizedTag left = dropJ (i - sizedTag left) right
    | otherwise = (dropJ i left) +++ right

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ i jl@(Single jlSize val)
    | i == 1 = jl
    | otherwise = error $ "tried to takeJ longer than the list: " ++ show i

takeJ i jl@(Append jlSize left right)
    | i == sizedTag left = left
    | i > sizedTag left = left +++ takeJ (i - sizedTag left) right
    | otherwise = takeJ i left

scoreLine :: String -> JoinList Score String
scoreLine line = Single (scoreString line) line
