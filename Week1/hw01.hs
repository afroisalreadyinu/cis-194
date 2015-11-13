import Data.Char (digitToInt)

-- First assignment is a very straightforward credit card ID
-- validator. The nice surprise at the end is that when you write the
-- functions described in the individual steps, the `validate`
-- function that brings them together is a simple compision

toDigits :: Integer -> [Integer]
toDigits = map toInteger . map digitToInt . show

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x1:x2:xs) = x1 : x2*2 : doubleEveryOtherRev xs


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits = sum . concat . map toDigits

validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toDigits


--------------------------------------------------------------------------
-- This assignment is a powerful demonstration of how seemingly
-- difficult problems become instantly solvable when you see the
-- recursion pattern.

-- to move n from a to b:
-- -move n-1 from a to c
-- -move the top from a to b
-- -move n-1 from b to c


type Peg = String
type Move = (Peg, Peg)

-- poor man's association list
getByPeg :: State -> Peg -> [Integer]
getByPeg [] peg = error $ "Peg " ++ peg ++ " not in State"
getByPeg ((firstPegName, firstPegList):rest) peg
    | firstPegName == peg = firstPegList
    | otherwise = getByPeg rest peg

moveOne :: State -> Peg -> Peg -> State
moveOne state fromPeg toPeg =
    let fromList = getByPeg state fromPeg
        disk = head fromList
        toList = getByPeg state toPeg
    in setByPeg (setByPeg state fromPeg (tail fromList)) toPeg (disk : toList)


setByPeg :: State -> Peg -> [Integer] -> State
setByPeg [] peg _ = error $ "Peg " ++ peg ++ " not in State"
setByPeg ((firstPegName, firstPegList):rest) peg newList
    | firstPegName == peg = ((peg,newList) : rest)
    | otherwise = ((firstPegName, firstPegList) : setByPeg rest peg newList)

getAltPeg :: State -> [Peg] -> Peg
getAltPeg state pegs =
    let existing = map fst state
    in head $ filter (not . (`elem` pegs)) existing

hanoiInner :: State -> Integer -> Peg -> Peg -> ([Move], State)
hanoiInner state count fromPeg toPeg
    | count == 1 = ([(fromPeg, toPeg)], moveOne state fromPeg toPeg)
    | count > 1 =
        let alternativePeg = getAltPeg state [fromPeg, toPeg]
            (moves, newState) = hanoiInner state (count - 1) fromPeg alternativePeg
            (newMoves, nextState) = hanoiInner newState 1 fromPeg toPeg
            (moreMoves, lastState) = hanoiInner nextState (count - 1) alternativePeg toPeg
        in (moves ++ newMoves ++ moreMoves, lastState)
    | otherwise = undefined

type State = [(Peg, [Integer])]

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi i p1 p2 p3 = fst $ hanoiInner [(p1, [1..i]), (p2, []), (p3, [])] i p1 p2
