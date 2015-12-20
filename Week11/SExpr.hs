module SExpr where

import AParserNew
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------



zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =  oneOrMore p <|> pure []


oneOrMore :: Parser a -> Parser [a]
oneOrMore p = ((:) <$> p) <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)


------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = (N <$> posInt) <|> (I <$> ident)

parsePar :: Parser SExpr
parsePar = char '(' *> (Comb <$> oneOrMore (spaces *> parseSExpr)) <* char ')'

parseSExpr :: Parser SExpr
parseSExpr = parsePar <|> (A <$> parseAtom)
