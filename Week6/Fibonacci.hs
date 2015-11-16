import Data.List (scanl, foldl')

-- Exercise 1
fib :: Integer -> Integer
fib x | x == 0 = 0
      | x == 1 = 1
      | x < 0 = error "only positive integers"
      | otherwise = fib (x - 1) + fib (x - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = map snd $ scanl collect (0,1) [0..]
        where collect (a,b) _ = (b, a+b)

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList (Cons a rest) = (a : streamToList rest)

instance Show a => Show (Stream a) where
    show st = (show (take 3 (streamToList st))) ++ "..."

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a st) = Cons (f a) (streamMap f st)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed s a = Cons a $ streamFromSeed s (s a)

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) 0

posNats :: Stream Integer
posNats = streamFromSeed (+1) 1

interleave :: Stream a -> Stream a -> Stream a
interleave (Cons a st1) (Cons b st2) = Cons a $ Cons b $ interleave st1 st2

-- not working yet
ruler :: Stream Integer
ruler = foldl' interleave (streamRepeat 0) (streamToList (streamMap streamRepeat posNats))
