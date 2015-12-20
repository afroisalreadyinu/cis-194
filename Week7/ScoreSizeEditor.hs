module Main where

import ScoreSizeBuffer
import Editor
    import JoinList

main = runEditor editor $ foldl1 (+++) $ map doubleScoreLine
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
