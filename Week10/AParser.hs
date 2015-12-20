{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

applyToFirst :: (a -> b) -> (a,c) -> (b,c)
applyToFirst f (a,c) = (f a, c)


instance Functor Parser where
    fmap f parser = Parser (\string -> (applyToFirst f) <$> (runParser parser string))

instance Applicative Parser where
    pure a = Parser (\string -> Just (a, string))
    parser1 <*> parser2 =
        Parser combined
            where combined string =
                      let firstOut = runParser parser1 string
                          sndOut = (snd <$> firstOut) >>= runParser parser2
                      in liftA2 (,) ((fst <$> firstOut) <*> (fst <$> sndOut)) (snd <$> sndOut)

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

intPair :: Parser [Integer]
intPair = (\x y -> [x,y]) <$> ((curry fst) <$> posInt <*> char ' ') <*> posInt

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------


instance Alternative Parser where
    empty = Parser (\_ -> Nothing)
    (<|>) pr1 pr2 = Parser (\string -> (runParser pr1 string) <|> (runParser pr2 string))


upperCaseParser :: Parser ()
upperCaseParser = Parser f
    where f [] = Nothing
          f xs = Just ((), dropWhile isUpper xs)

nullify :: a -> ()
nullify _ = ()

intOrUppercase :: Parser ()
intOrUppercase = nullify <$> posInt <|> (nullify <$> satisfy isUpper)
