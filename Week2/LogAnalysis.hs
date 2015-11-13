{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

-- This assignment serves two purposes: An introduction to working
-- with strings as lists with the standard prelude functions, and
-- manipulating (binary) trees using ADTs in Haskell.

import Log
import Data.Char

parse :: String -> [LogMessage]
parse page = map parseLine (lines page)

parseLine :: String -> LogMessage
parseLine line =
      case line of
        ('I':rest) -> parseNormal Info rest
        ('W':rest) -> parseNormal Warning rest
        ('E':rest) -> parseError rest
        _ -> Unknown line

trim :: String -> String
trim s = dropWhile (== ' ') s

parseNormal :: MessageType -> String -> LogMessage
parseNormal mt s =
    let (timestamp, message) = parsePrefixInt $ trim s
    in LogMessage mt timestamp message

parseError :: String -> LogMessage
parseError s =
    let (level, preMessage) = parsePrefixInt $ trim s
        (timestamp, message) = parsePrefixInt preMessage
    in LogMessage (Error level) timestamp message

parsePrefixInt :: String -> (Int, String)
parsePrefixInt s = parsePrefixInt' s 0

toInt :: Char -> Int
toInt c = (ord c) - (ord '0')

parsePrefixInt' :: String -> Int -> (Int, String)
parsePrefixInt' alls@(first:rests) c =
    if first `elem` ['0'..'9'] then
       parsePrefixInt' rests (c * 10 + toInt first)
    else
       (c, (dropWhile (== ' ') alls))
parsePrefixInt' "" c = (c, "")

insert :: LogMessage -> MessageTree -> MessageTree
insert lm Leaf = Node Leaf lm Leaf
insert (Unknown _) mt = mt
insert lm (Node tl elm tr) =
    let (LogMessage _ timestampNew _) = lm
        (LogMessage _ timestampOld _) = elm
    in if timestampNew > timestampOld then
           Node tl elm (insert lm tr)
       else Node (insert lm tl) elm tr

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder = inOrder' []

inOrder' :: [LogMessage] -> MessageTree -> [LogMessage]
inOrder' lms Leaf = lms
inOrder' lms (Node treeLeft lm treeRight) =
    inOrder' (lm : lms) treeLeft ++ inOrder' [] treeRight

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lms =
    let errors = [ lm | lm@(LogMessage (Error level) _ _) <- lms, level > 50]
    in [ msg | (LogMessage _ _ msg) <- inOrder (build errors)]
